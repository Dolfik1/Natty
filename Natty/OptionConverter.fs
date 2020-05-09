module internal Natty.Converters

open Mappy.Converters

let isOption (t : System.Type) = 
  t.GetType().IsGenericType 
    && t.GetGenericTypeDefinition() = typedefof<option<_>>

type OptionConverter() =
  interface ITypeConverter with
    member x.Order = -1
    member x.CanConvert<'t>(value) = isOption (typedefof<'t>)
     
    member x.Convert(value) =
      let value = unbox<'a> value
      if System.Object.ReferenceEquals(value, null) then
        Some value |> box |> unbox<'a>
      else
        None |> box |> unbox<'a>