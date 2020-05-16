module internal Helpers

open System
open System.Data
open System.Reflection
open FSharp.Reflection

let isOption (t: System.Type) = 
  t.IsGenericType 
    && t.GetGenericTypeDefinition() = typedefof<Option<_>>
    
let canBeInlineParameter t =
  t = typeof<string> || t.IsPrimitive
    
let isInlineOption t =
  isOption t && canBeInlineParameter (t.GetGenericArguments().[0])

let unboxOptionToDb obj =
  let _, fields = FSharpValue.GetUnionFields(obj, obj.GetType())
  if fields.Length = 0
  then box DBNull.Value
  else fields.[0]
  

let getMethodInfo name =
  Assembly.GetExecutingAssembly()
    .GetType("Helpers").GetMethod(name, BindingFlags.NonPublic ||| BindingFlags.Static)
 
[<ReflectedDefinition>]
let addParameter (command: IDbCommand) name value =
  let param = command.CreateParameter()
  param.ParameterName <- name
  param.Value <- value
  command.Parameters.Add(param) |> ignore

let addParameterMethodInfo =
  getMethodInfo "addParameter"
  
[<ReflectedDefinition>]
let addParameterOption (command: IDbCommand) name (value: 'a option) =
  match value with
  | Some p ->
    addParameter command name p
  | _ ->
    addParameter command name DBNull.Value
let addParameterOptionMethodInfo =
  getMethodInfo "addParameterOption"