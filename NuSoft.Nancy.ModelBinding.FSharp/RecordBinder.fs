namespace Nancy.ModelBinding

open Microsoft.FSharp.Reflection
open System.Collections
open System.Collections.Generic
open System
open System.Globalization
open Nancy

module Parse =
  let rec bind (form: DynamicDictionary) (t: Type) (prefix: string) =
    match t with
      | t when FSharpType.IsRecord t ->
        Helpers.creatRecord t (fun p ->
          sprintf "%s%s" (if prefix = "" then "" else sprintf "%s." prefix) p.Name |> bind form p.ParameterType 
        )
      | t when t.IsEnum ->
        
        let dynValue = Helpers.getFieldStrict form prefix
        match Type.GetTypeCode(dynValue.Value.GetType()) with
          | TypeCode.Byte
          | TypeCode.SByte
          | TypeCode.UInt16
          | TypeCode.UInt32
          | TypeCode.UInt64
          | TypeCode.Int16
          | TypeCode.Int32
          | TypeCode.Int64 -> 
            let converted = Convert.ChangeType(dynValue.Value,t.GetEnumUnderlyingType())
            if Enum.IsDefined(t, converted) |> not then 
              failwithf "Invalid enum value for key %s; only defined fileds are currently supported" prefix
            Enum.ToObject(t, converted)
          | _ -> failwithf "Invalid enum value for key %s; only integer based values are currently supported" prefix
      | t when t.IsPrimitive || t = typedefof<string> ->
        Helpers.getFieldStrict form prefix
        |> Helpers.convert t
      | t when t.IsArray  ->
        let indexes = Helpers.indexes form prefix
        let et = t.GetElementType()
        if indexes |> Seq.isEmpty 
        then 
          Array.CreateInstance(et,0) :> obj
        else 
          let arr = Array.CreateInstance(et,indexes |> Seq.max |> (+) 1) :> IList
        
          seq { 0 .. (indexes |> Seq.max) } 
          |> Seq.map (sprintf "%s[%i]" prefix >> bind form et >> Helpers.convert et)
          |> Seq.zip (seq { 0 .. (indexes |> Seq.max) })
          |> Seq.iter (fun (idx,value) -> arr.[idx] <- value)

          arr :> obj
      | t when t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Option<_>> ->
        
        match prefix |> Helpers.startsWith form with 
            | false -> Option.None :> obj
            | _ -> 
              let gt = t.GetGenericArguments() |> Seq.head
              Activator.CreateInstance(t,bind form gt prefix) 

        

type RecordBinder() =
  interface IModelBinder with
    member __.CanBind modelType =
      FSharpType.IsRecord modelType 
      || modelType.IsArray && modelType.GetElementType() |> FSharpType.IsRecord 
      || modelType.IsGenericType && modelType.GetGenericTypeDefinition() = typedefof<Option<_>>
      || modelType.IsArray && let et = modelType.GetElementType() in et.IsGenericType && et.GetGenericTypeDefinition() = typedefof<Option<_>>

    member self.Bind (context, modelType,_,_,_) =
      let form = context.Request.Form :?> DynamicDictionary
      Parse.bind form modelType ""