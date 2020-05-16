module Natty.Mappy
  open Converters
  open System.Data
  open System.Collections.Generic

  let private getFieldsNames (reader : IDataReader) = 
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
        let dic = Dictionary<string, obj>()
        for i in 0..reader.FieldCount - 1 do
          dic.Add(fieldsNames.[i], values.[i])
        yield dic :> IDictionary<string, obj>
    }
  
  let createMapper (options: Mappy.MappyOptions) =
    options.AddConverter(OptionConverter())
    let mappy = Mappy.Mappy(options)

    fun (reader: IDataReader) ->
      let fieldsNames = getFieldsNames reader
      let rows = readRows reader fieldsNames
      mappy.Map<'a>(rows)