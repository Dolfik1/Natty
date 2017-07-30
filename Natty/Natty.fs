module Natty.Data

//open Chocolate.Mapper

open System
open System.Collections.Generic
open System.Data
open System.Text.RegularExpressions
open System.Reflection
open FSharp.Reflection


// Natty.Mapper deps
open Microsoft.Extensions.Caching.Memory
open System.Collections
open System.Diagnostics

[<AttributeUsage(AttributeTargets.Field ||| AttributeTargets.Property, AllowMultiple = false, Inherited = false)>]
type Id() = inherit Attribute()

Natty.Mapper.Configuration.IdentifierAttributeType <- typedefof<Id>

type SqlQuery<'a> = 
    { QueryText: string
      Parameters: (string * obj) list option }
      
      
type ChocolateConfig = 
    { Delimiter: char
      DefaultIdName: string
      IgnoreCase: bool }
let defaultConfig = { Delimiter = '_'; DefaultIdName = "Id"; IgnoreCase = false }

let private isOption (t: Type) = t.GetTypeInfo().IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>>
let private getParamValue value =
    if isNull value then null
    else
        let tp = value.GetType()
        if isOption tp then 
            let _, fields = FSharpValue.GetUnionFields(value, value.GetType())
            if fields.Length = 0 then null
            else fields.[0]
        else 
            value

let private getFieldsNames config (reader: IDataReader) =
    seq {
        for i in 0 .. reader.FieldCount-1 do
            yield reader.GetName(i)
    } |> Seq.toArray


let private readRows (reader: IDataReader) (fieldsNames: string[]) : IDictionary<string, obj> seq = 
    seq {
        while reader.Read() do
            let values = Array.zeroCreate<obj> reader.FieldCount
            reader.GetValues(values) |> ignore
            
            let dic = new Dictionary<string, obj>()
            for i in 0 .. reader.FieldCount-1 do
                dic.Add(fieldsNames.[i], values.[i])
                
            yield dic :> IDictionary<string, obj>
    }

let execute (conn: IDbConnection) config (query: SqlQuery<'a>): seq<'a> = 

    if conn.State = ConnectionState.Closed 
    || conn.State = ConnectionState.Broken then conn.Open()

    use command = conn.CreateCommand()
    command.CommandText <- query.QueryText
    
    if query.Parameters.IsSome then
        query.Parameters.Value
        |> Seq.iter (fun (name, value) -> 
            let param = command.CreateParameter()
            
            param.ParameterName <- name
            let v = getParamValue value
            param.Value <- if isNull v then (box DBNull.Value) else v

            command.Parameters.Add(param) |> ignore)

    let tp = typedefof<'a>
    let tpinfo = tp.GetTypeInfo()

    if tp = typedefof<unit> then 
        command.ExecuteNonQuery() |> ignore
        Seq.empty
    else if tpinfo.IsPrimitive || tp = typedefof<string> then
        let v = command.ExecuteScalar() :?> 'a
        [ v ] |> List.toSeq
    else
        use reader = command.ExecuteReader()
        let fieldsNames = getFieldsNames config reader
        let rows = readRows reader fieldsNames

        
        Natty.Mapper.Map<'a>(rows, false)
        

let executeSingle conn config query = 
    execute conn config query |> Seq.head
let executeSingleOrDefault conn config query =
    execute conn config query |> Seq.tryHead

 
let executeAsync conn config query = 
    async { return execute conn config query }
let executeSingleAsync conn config query = 
    async { return executeSingle conn config query }
let executeSingleOrDefaultAsync conn config query =
    async { return executeSingleOrDefault conn config query }

let sqlQuery<'a> queryText parameters = 
    { QueryText = queryText; Parameters = parameters; }: SqlQuery<'a>


let private printfFormatProc (worker: string * obj list -> 'd) (query: PrintfFormat<'a, _, _, 'd>) : 'a = 
    if not (FSharpType.IsFunction typeof<'a>) then 
        unbox (worker (query.Value, [])) 
    else 
        let rec getFlattenedFunctionElements (functionType: Type) = 
            let domain, range = FSharpType.GetFunctionElements functionType 
            if not (FSharpType.IsFunction range) 
                then domain::[range] 
                else domain::getFlattenedFunctionElements(range) 
        let types = getFlattenedFunctionElements typeof<'a> 
        let rec proc (types: Type list) (values: obj list) (a: obj) : obj = 
            let values = a::values 
            match types with 
            | [x;_] -> 
                let result = worker (query.Value, List.rev values) 
                box result 
            | x::y::z::xs -> 
                let cont = proc (y::z::xs) values 
                let ft = FSharpType.MakeFunctionType(y,z) 
                let cont = FSharpValue.MakeFunction(ft, cont) 
                box cont 
            | _ -> failwith "shouldn't happen" 
        let handler = proc types [] 
        unbox (FSharpValue.MakeFunction(typeof<'a>, handler))

let private sqlProcessor<'x> (sql: string, values: obj list) : SqlQuery<'x> =
    let stripFormatting s =
        let i = ref -1
        let eval (rxMatch: Match) =
            incr i
            sprintf "@p%d" !i
        Regex.Replace(s, "%.", eval)
    
    let sql = stripFormatting sql
    let vals = (values |> Seq.mapi (fun i x -> (sprintf "@p%i" i, x))) |> Seq.toList
    sqlQuery<'x> sql (Some vals)
    

let sqlQueryf a = printfFormatProc sqlProcessor a