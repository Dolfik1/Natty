module Natty.Data

open System
open System.Collections.Generic
open System.Data
open System.Text.RegularExpressions
open System.Reflection
open FSharp.Reflection

[<Struct>]
type SqlQuery<'a> = 
  { QueryText : string
    QueryTextRaw : string
    Parameters : (string * obj) list option }

let private getParamValue value = 
  if isNull value then null
  else
    if Helpers.isOption (value.GetType()) then 
      let _, fields = FSharpValue.GetUnionFields(value, value.GetType())
      if fields.Length = 0 then null else fields.[0]
    else value

let execute (conn : IDbConnection) mapFn (query : SqlQuery<'a>) : seq<'a> = 
  if conn.State = ConnectionState.Closed 
     || conn.State = ConnectionState.Broken then conn.Open()
  use command = conn.CreateCommand()
  command.CommandText <- query.QueryText
  if query.Parameters.IsSome then 
    query.Parameters.Value
    |> Seq.iter (
      fun (name, value) -> 
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
    mapFn reader

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

let rec private getProcessedSql (idx: int ref) (sql : string) (values : obj list) = 
  let localIdx = ref 0

  let items = List<obj>()
  let values = values |> List.toArray

  let eval (_ : Match) = 
    let item = values.[!localIdx]
    let tp = if (isNull item) |> not then item.GetType() else null
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