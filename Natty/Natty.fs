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
  
let private paramReaders = ConcurrentDictionary<Type, Action<IDbCommand, Set<string>, obj>>()

let private createParamReader tp =
  let commandPar = Expression.Parameter(typeof<IDbCommand>)
  let variablesPar = Expression.Parameter(typeof<Set<string>>)
  let par = Expression.Parameter(typeof<obj>)
  
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
        variablesPar,
        Expression.Constant(mi.Name),
        Expression.PropertyOrField(unboxed, mi.Name)) :> Expression)
    
  let block = Expression.Block(expressions)
  Expression.Lambda<Action<IDbCommand, Set<string>, obj>>(block, commandPar, variablesPar, par).Compile()

let private getOrCreateParamReader tp =
  paramReaders.GetOrAdd(tp, createParamReader tp)
  
let queryToCommand (query: SqlQuery<'a>) (command: IDbCommand) =
  command.CommandText <- query.QueryText
  
  let idx = ref 0
  
  let maxVariables = query.QueryText |> Seq.sumBy (fun x -> if x = '@' then 1 else 0)
  let variables =
    if maxVariables > 0 then
      let queryText = query.QueryText

      let mutable startIndex = -1
      seq {
        let idx = ref -1
        for c in query.QueryText do
          incr idx
          match c, startIndex with
          | '@', _ -> 
            startIndex <- !idx + 1
          | _, -1 -> ()
          | _ when c <> '_' && Char.IsLetterOrDigit(c) |> not ->
            yield queryText.Substring(startIndex, !idx - startIndex)
            startIndex <- -1
          | _ -> ()

        if startIndex > 0 then
          yield queryText.Substring(startIndex)
      } |> Set.ofSeq
    else
      Set.empty

  let rec readParam (param: obj) =
    let tp = lazy(param.GetType())
    match param with
    | _ when Object.ReferenceEquals(param, null) ->
      Helpers.addParameter command variables (sprintf "p%i" !idx) (box DBNull.Value)
      incr idx

    | _ when Helpers.canBeInlineParameter tp.Value ->
      Helpers.addParameter command variables (sprintf "p%i" !idx) param
      incr idx

    | _ when Helpers.isInlineOption tp.Value ->
      Helpers.addParameter command variables (sprintf "p%i" !idx) (Helpers.unboxOptionToDb param)
      incr idx

    | :? (string * obj) as arg ->
      Helpers.addParameter command variables (fst arg) (snd arg)

    | :? System.Collections.Generic.IEnumerable<(string * obj)> as seq ->
      seq |> Seq.iter readParam

    | _ ->
      let reader = getOrCreateParamReader tp.Value
      reader.Invoke(command, variables, param)

  query.Parameters |> Seq.iter readParam
  command

let execute (conn: IDbConnection) mapFn (query: SqlQuery<'a>) : seq<'a> = 
  if conn.State = ConnectionState.Closed 
     || conn.State = ConnectionState.Broken then conn.Open()
  use command = conn.CreateCommand() |> queryToCommand query
  
  let tp = typeof<'a>
  if tp = typeof<unit> then 
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