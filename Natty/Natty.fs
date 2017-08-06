module Natty.Data

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

[<AttributeUsage(AttributeTargets.Field ||| AttributeTargets.Property, 
                 AllowMultiple = false, Inherited = false)>]
type Id() = 
    inherit Attribute()

Natty.Mapper.Configuration.IdentifierAttributeType <- typedefof<Id>

type SqlQuery<'a> = 
    { QueryText : string
      QueryTextRaw : string
      Parameters : (string * obj) list option }

type NattyConfig = 
    { Delimiter : char
      DefaultIdName : string
      IgnoreCase : bool }

let defaultConfig = 
    { Delimiter = '_'
      DefaultIdName = "Id"
      IgnoreCase = false }

let private isOption (t : Type) = 
    t.GetTypeInfo().IsGenericType 
    && t.GetGenericTypeDefinition() = typedefof<option<_>>

let private getParamValue value = 
    if isNull value then null
    else 
        let tp = value.GetType()
        if isOption tp then 
            let _, fields = FSharpValue.GetUnionFields(value, value.GetType())
            if fields.Length = 0 then null
            else fields.[0]
        else value

let private getFieldsNames config (reader : IDataReader) = 
    seq { 
        for i in 0..reader.FieldCount - 1 do
            yield reader.GetName(i)
    }
    |> Seq.toArray

let private readRows (reader : IDataReader) (fieldsNames : string []) : IDictionary<string, obj> seq = 
    seq { 
        while reader.Read() do
            let values = Array.zeroCreate<obj> reader.FieldCount
            reader.GetValues(values) |> ignore
            let dic = new Dictionary<string, obj>()
            for i in 0..reader.FieldCount - 1 do
                dic.Add(fieldsNames.[i], values.[i])
            yield dic :> IDictionary<string, obj>
    }

let execute (conn : IDbConnection) config (query : SqlQuery<'a>) : seq<'a> = 
    if conn.State = ConnectionState.Closed 
       || conn.State = ConnectionState.Broken then conn.Open()
    use command = conn.CreateCommand()
    command.CommandText <- query.QueryText
    if query.Parameters.IsSome then 
        query.Parameters.Value |> Seq.iter (fun (name, value) -> 
                                      let param = command.CreateParameter()
                                      param.ParameterName <- name
                                      let v = getParamValue value
                                      param.Value <- if isNull v then 
                                                         (box DBNull.Value)
                                                     else v
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

let executeSingle conn config query = execute conn config query |> Seq.head
let executeSingleOrDefault conn config query = 
    execute conn config query |> Seq.tryHead
let executeAsync conn config query = async { return execute conn config query }
let executeSingleAsync conn config query = 
    async { return executeSingle conn config query }
let executeSingleOrDefaultAsync conn config query = 
    async { return executeSingleOrDefault conn config query }

let sqlQuery<'a> queryText parameters : SqlQuery<'a> = 
    { QueryText = queryText
      /// "Raw" string from sqlQueryf
      QueryTextRaw = queryText
      Parameters = parameters }

let private printfFormatProc (worker : string * obj list -> 'd) 
    (query : PrintfFormat<'a, _, _, 'd>) : 'a = 
    if not (FSharpType.IsFunction typeof<'a>) then 
        unbox (worker (query.Value, []))
    else 
        let rec proc (functionType : Type) (values : obj list) (a : obj) : obj = 
            let values = a :: values
            let domain, range = FSharpType.GetFunctionElements functionType
            if not (FSharpType.IsFunction range) then 
                let result = worker (query.Value, List.rev values)
                box result
            else 
                let impl = proc range values
                let cont = FSharpValue.MakeFunction(range, impl)
                box cont
        
        let handler = proc typeof<'a> []
        unbox (FSharpValue.MakeFunction(typeof<'a>, handler))

let rec private getProcessedSql (idx: int ref) (sql : string) (values : obj list) = 
    let localIdx = ref 0

    let items = List<obj>()
    let values = values |> List.toArray

    let eval (rxMatch : Match) = 

        let item = values.[!localIdx]

        let tp = item.GetType()
        if tp = typeof<SqlQuery<_>> then

            let queryText = tp.GetProperty("QueryTextRaw").GetValue(item) :?> string
            let parameters = tp.GetProperty("Parameters").GetValue(item) :?> (string * obj) list option
            
            let sqlQuery, vals = 
                match parameters with
                | Some parameters -> getProcessedSql idx queryText (parameters |> List.map (fun (_, x) -> x))
                | None -> queryText, []

            items.AddRange (vals)
            incr localIdx

            sqlQuery
        else 
            items.Add(item)

            incr idx
            incr localIdx
            sprintf "@p%d" !idx

    let sql = Regex.Replace(sql, "%.", eval)
    (sql, (items |> Seq.toList))

let private sqlProcessor<'x> (sqlRaw : string, values : obj list) : SqlQuery<'x> = 
    let idx = ref -1
    let sql, vals = getProcessedSql idx sqlRaw values
    let vals = vals |> List.mapi (fun i x -> (sprintf "@p%i" i), x)
    { QueryText = sql; QueryTextRaw = sqlRaw; Parameters = (Some vals) }: SqlQuery<'x>

let sqlQueryf a = printfFormatProc sqlProcessor a