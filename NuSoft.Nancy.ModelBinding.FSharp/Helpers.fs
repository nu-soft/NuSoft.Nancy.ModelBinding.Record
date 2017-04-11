module Helpers

  open System.Collections.Generic
  open Nancy
  open System
  open System.Globalization
  open System.Reflection
  open System.Text.RegularExpressions

  let indexReg = new Regex("^\[(?<i>\d+)\]")

  let contains (form: DynamicDictionary) field =
    form.ContainsKey(field)

  let startsWith (form: DynamicDictionary) field =
    form.Keys
    |> Seq.exists (fun k -> k.StartsWith(field,StringComparison.OrdinalIgnoreCase))
    //form.ContainsKey(field)

  let getFieldStrict (form: DynamicDictionary) field =
    let f = 
      if form.ContainsKey(field) then
        form.[field]
      else
        failwithf "Key %s not found" field
        new KeyNotFoundException() |> raise
    f :?> DynamicDictionaryValue

  let convert (t: Type) value =
    Convert.ChangeType(value, t, CultureInfo.InvariantCulture)

  let creatRecord (t:Type) (func: ParameterInfo -> obj) = 
    let ctor = t.GetConstructors () |> Array.head
    let pps = ctor.GetParameters()
    pps 
    |> Array.map func
    |> ctor.Invoke

  let indexes (form: DynamicDictionary) prefix =
    form.Keys 
    |> Seq.where (fun k -> k.StartsWith(prefix,StringComparison.OrdinalIgnoreCase))
    |> Seq.map (fun k -> k.Substring(prefix |> Seq.length) |> indexReg.Match)
    |> Seq.filter (fun m -> m.Success)
    |> Seq.map(fun m-> m.Groups.["i"].Value |> int )



