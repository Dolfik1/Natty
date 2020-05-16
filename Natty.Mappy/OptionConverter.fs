module internal Natty.Mappy.Converters

open System
open System.Linq.Expressions
open System.Collections.Concurrent
open Mappy.Converters

let isOption (t: System.Type) = 
  t.IsGenericType 
    && t.GetGenericTypeDefinition() = typedefof<Option<_>>

let generateExpression<'t> =
  let value = Expression.Parameter(typedefof<obj>)
  let resultType = typeof<'t>
  let ctor = resultType.GetConstructors().[0]
  let underlying = resultType.GetGenericArguments().[0]
  Expression.Lambda<Func<obj, 't>>(
    Expression.New(ctor, Expression.Convert(value, underlying)), value
  ).Compile()

type OptionConverter() =
 
  let expressions = ConcurrentDictionary<Type, obj>()
   
  interface ITypeConverter with
    member x.Order = -1
    member x.CanConvert<'t>(_) =
      isOption (typedefof<'t>)
     
    member x.Convert<'a>(value) =
      if isNull value |> not && value <> box DBNull.Value then
        let fn =
          expressions.GetOrAdd(
            typeof<'a>,
            valueFactory = (fun _ -> generateExpression<'a> |> box))
          |> unbox<Func<obj, 'a>>
        fn.Invoke(value)
      else
        None |> box |> unbox<'a>