namespace Nancy.ModelBinding

open Microsoft.FSharp.Reflection
open System.Collections
open System.Collections.Generic
open System
open System.Globalization

type RecordBinder() =
  interface IModelBinder with
    member __.CanBind modelType =
      FSharpType.IsRecord modelType 
      || modelType.IsArray && modelType.GetElementType() |> FSharpType.IsRecord 

    member self.Bind (context, modelType,_,_,_) =
      let form = context.Request.Form :?> IDictionary<string,obj> 


      match modelType with
        | mt when FSharpType.IsRecord mt ->
          self.BindInternal form modelType ""
        | mt when mt.IsArray && mt.GetElementType() |> FSharpType.IsRecord ->
          let maxIdx = 
            form.Keys
            |> Seq.where(fun x -> x.StartsWith("[" , StringComparison.InvariantCultureIgnoreCase))
            |> Seq.map (fun x->
              let s = x.IndexOf('[')
              let e = x.IndexOf(']',s)
              let index = x.Substring(s+1,e-s-1) |> System.Int32.Parse
              index
            ) 
            |> Seq.max

          let res = Array.CreateInstance(mt.GetElementType(),maxIdx + 1)
          [ 0 .. maxIdx ]
          |> List.iter (fun x -> (res :> IList).[x]  <- self.BindInternal form (mt.GetElementType()) (sprintf "[%i]." x))
          res :> obj
        | mt -> printfn "%A" mt;failwith "Unhandled type"
      

  member self.BindInternal (form: IDictionary<string,obj>) (mt: Type) prefix =
    let ctor = mt.GetConstructors () |> Array.head
    let pps = ctor.GetParameters()
    ctor.Invoke(
      pps 
      |> Array.map (function
        | pt when pt.ParameterType.IsPrimitive || pt.ParameterType = typedefof<string> ->
          Convert.ChangeType(form.[sprintf "%s%s" prefix pt.Name],pt.ParameterType,CultureInfo.InvariantCulture)
        | pt when pt.ParameterType.IsArray && let et = pt.ParameterType.GetElementType() in (et.IsPrimitive || et = typedefof<string>)->
          let fields = 
            form.Keys
            |> Seq.where(fun x -> x.StartsWith(sprintf "%s%s[" prefix pt.Name, StringComparison.InvariantCultureIgnoreCase))
            |> Seq.map (fun x->
              let s = x.IndexOf('[', prefix.Length)
              let e = x.IndexOf(']',s)
              let index = x.Substring(s+1,e-s-1) |> System.Int32.Parse
              index
            ) 
          if fields |> Seq.isEmpty then
            Array.CreateInstance(pt.ParameterType.GetElementType(),0) :> obj
          else
            let maxIdx = 
              fields
              |> Seq.max

            let res = Array.CreateInstance(pt.ParameterType.GetElementType(),maxIdx + 1)
          
            [ 0 .. maxIdx ]
            |> List.iter (fun x -> (res :> IList).[x]  <- Convert.ChangeType(form.[sprintf "%s%s[%i]" prefix pt.Name x],pt.ParameterType.GetElementType(),CultureInfo.InvariantCulture))
            res :> obj
        | pt when FSharpType.IsRecord pt.ParameterType ->
          self.BindInternal form pt.ParameterType (pt.Name+".")
        | pt when pt.ParameterType.IsArray && pt.ParameterType.GetElementType() |> FSharpType.IsRecord ->
          
          let fields = 
            form.Keys
            |> Seq.where(fun x -> x.StartsWith(sprintf "%s%s[" prefix pt.Name, StringComparison.InvariantCultureIgnoreCase))
            |> Seq.map (fun x->
              let s = x.IndexOf('[', prefix.Length)
              let e = x.IndexOf(']',s)
              let index = x.Substring(s+1,e-s-1) |> System.Int32.Parse
              index
            ) 
          if fields |> Seq.isEmpty then
            Array.CreateInstance(pt.ParameterType.GetElementType(),0) :> obj
          else
            let maxIdx = 
              fields
              |> Seq.max

            let res = Array.CreateInstance(pt.ParameterType.GetElementType(),maxIdx + 1)
            [ 0 .. maxIdx ]
            |> List.iter (fun x -> (res :> IList).[x]  <- self.BindInternal form (pt.ParameterType.GetElementType()) (sprintf "%s[%i]." pt.Name x))
            res :> obj
        | pt -> printfn "%A" pt; failwith "Unhandled type"
      )
    )
