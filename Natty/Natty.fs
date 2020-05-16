module Natty.Data

open System
open System.Data
open System.Linq.Expressions
open System.Collections.Concurrent
open System.Text.RegularExpressions
open System.Reflection
open FSharp.Reflection

type ISqlQuery =
  abstract QueryTextRaw : string
  abstract QueryText : string
  abstract Parameters : obj[]

[<Struct>]
type SqlQuery<'a> = 
  { QueryText: string
    QueryTextRaw: string
    Parameters: obj[] }
  interface ISqlQuery with
    member x.QueryText = x.QueryText
    member x.QueryTextRaw = x.QueryTextRaw
    member x.Parameters = x.Parameters
  
let paramReaders = ConcurrentDictionary<Type, Action<IDbCommand, obj>>()

let private createParamReader tp =
  let par = Expression.Parameter(typeof<obj>)
  let commandPar = Expression.Parameter(typeof<IDbCommand>)
  
  let unboxed = Expression.Convert(par, tp)
  let expressions =
    seq {
      yield! tp.GetFields() |> Seq.map (fun x -> x :> MemberInfo, x.FieldType)
      yield! tp.GetProperties() |> Seq.map (fun x -> x :> MemberInfo, x.PropertyType)
    }
    |> Seq.map (fun (mi, tp) ->
      let method =
        if Helpers.isOption tp then
          let tp = tp.GetGenericArguments().[0]
          Helpers.addParameterOptionMethodInfo
            .MakeGenericMethod(tp)
        else
          Helpers.addParameterMethodInfo
            .MakeGenericMethod(tp)
      
      Expression.Call(
        method,
        commandPar,
        Expression.Constant(mi.Name),
        Expression.PropertyOrField(unboxed, mi.Name)) :> Expression)
    
  let block = Expression.Block(expressions)
  Expression.Lambda<Action<IDbCommand, obj>>(block, commandPar, par).Compile()

let private getOrCreateParamReader tp =
  paramReaders.GetOrAdd(tp, createParamReader tp)
  
let execute (conn : IDbConnection) mapFn (query : SqlQuery<'a>) : seq<'a> = 
  if conn.State = ConnectionState.Closed 
     || conn.State = ConnectionState.Broken then conn.Open()
  use command = conn.CreateCommand()
  command.CommandText <- query.QueryText
  
  let idx = ref 0
  
  for param in query.Parameters do
    let tp = lazy(param.GetType())
    if param = null then
      Helpers.addParameter command (sprintf "p%i" !idx) (box DBNull.Value)
      incr idx
    else if Helpers.canBeInlineParameter tp.Value then
      Helpers.addParameter command (sprintf "p%i" !idx) param
      incr idx
    else if Helpers.isInlineOption tp.Value then
      Helpers.addParameter command (sprintf "p%i" !idx) (Helpers.unboxOptionToDb param)
      incr idx
    else
      let reader = getOrCreateParamReader tp.Value
      reader.Invoke(command, param)
    
  let tp = typedefof<'a>
  if tp = typedefof<unit> then 
    command.ExecuteNonQuery() |> ignore
    Seq.empty
  else if tp.IsPrimitive || tp = typedefof<string> then 
    let v = command.ExecuteScalar() :?> 'a
    [ v ] |> Seq.ofList
  else 
    use reader = command.ExecuteReader()
    mapFn reader

let executeSingle conn config query = execute conn config query |> Seq.head

let executeSingleOrDefault conn config query = 
  execute conn config query |> Seq.tryHead

let executeAsync conn config query = async { return execute conn config query }

let executeSingleAsync conn config query = 
  async { return executeSingle conn config query }

let executeSingleOrDefaultAsync conn config query = 
  async { return executeSingleOrDefault conn config query }

let inline sqlQuery<'a, 'p> queryText (param: 'p option) : SqlQuery<'a> = 
  { QueryText = queryText
    /// "Raw" string from sqlQueryf
    QueryTextRaw = queryText
    Parameters = if param.IsSome then [| box param.Value |] else [||] }

let private printfFormatProc (worker : string * obj list -> 'd) 
  (query : PrintfFormat<'a, _, _, 'd>) : 'a = 
  if not (FSharpType.IsFunction typeof<'a>) then 
    unbox (worker (query.Value, []))
  else 
    let rec proc (functionType : Type) (values : obj list) (a : obj) : obj = 
      let values = a :: values
      let range = snd <| FSharpType.GetFunctionElements functionType
      if not (FSharpType.IsFunction range) then 
        let result = worker (query.Value, List.rev values)
        box result
      else 
        let impl = proc range values
        let cont = FSharpValue.MakeFunction(range, impl)
        box cont
        
    let handler = proc typeof<'a> []
    unbox (FSharpValue.MakeFunction(typeof<'a>, handler))

let rec private getProcessedSql (idx: int ref) (sql: string) (values: obj[]) (resultValues: ResizeArray<obj>) = 
  let localIdx = ref 0
  
  let eval (_ : Match) = 
    let item = values.[!localIdx]
    incr localIdx
    
    match item with
    | :? ISqlQuery as q ->
      getProcessedSql idx q.QueryTextRaw q.Parameters resultValues
    | _ ->
      resultValues.Add(item)
      incr idx
      sprintf "@p%i" !idx

  let sql = Regex.Replace(sql, "%.", eval)
  sql

let private sqlProcessor (sqlRaw : string, values : obj list) : SqlQuery<'a> = 
  let idx = ref -1
  let resultValues = ResizeArray<obj>()
  let sql = getProcessedSql idx sqlRaw (values |> Array.ofList) resultValues
  { QueryText = sql; QueryTextRaw = sqlRaw; Parameters = resultValues |> Array.ofSeq }: SqlQuery<'a>

let sqlQueryf a = printfFormatProc sqlProcessor a