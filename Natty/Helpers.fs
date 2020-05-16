module internal Helpers
  open System.Runtime.CompilerServices

  let isOption (t: System.Type) = 
    t.IsGenericType 
      && t.GetGenericTypeDefinition() = typedefof<Option<_>>