namespace NuSoft.Nancy.ModelBinding.FSharp.Tests
open Xunit

//[<Collection("Tests of record types")>]
module RecordTests = 
  open Nancy
  open Nancy.ModelBinding
  
  let rb = new RecordBinder() :> IModelBinder

  type SimpleRecord = {
    Field1: int
    Field2: string
    Field3 : bool
    
    OpField1: int option
    OpField2: string option
    OpField3 : bool option

    Arr1: int array
    Arr2: string array
    Arr3: bool array
    
    OpArr1: int option array
    
    ArrOp1: int array option
    ArrOp2: int array option
  }
  
  type NestedRecord = {
    Field: SimpleRecord
    OpField: SimpleRecord option
    
    ArrField: SimpleRecord array
  }

  type BYTEEnum = | Value = 1uy 
  type LINTEnum = | Value = 2L 
  type DINTEnum = | Value = -3L 

  type RecordWithEnum = {
    Field1: BYTEEnum
    Field2: LINTEnum
    Field3: DINTEnum
  }

  

  [<Fact>]
  let ``Test flat record`` () = 
    let dd = new DynamicDictionary()
    let field1 = 22
    let field2 = "Alice"
    let opField1 = Some 1
    let opField2 = Some "has cat"
    let opField3 = None
    dd.Add("field1", field1)
    dd.Add("field2", field2)
    dd.Add("field3", true)
    dd.Add("opField1", opField1.Value)
    dd.Add("opField2", opField2.Value)
    dd.Add("arr1[0]", field1)
    dd.Add("arr2[0]", field2)
    dd.Add("arr2[1]", opField2.Value)
    dd.Add("arr3[0]", true)
    dd.Add("arr3[1]", false)
    dd.Add("arr3[2]", true)
    
    dd.Add("oparr1[0]", field1)
    dd.Add("oparr1[3]", field1)

    dd.Add("arrop1[0]", field1)

    
    Assert.True(rb.CanBind(typeof<SimpleRecord>))
    let res = Parse.bind dd typeof<SimpleRecord> "" :?> SimpleRecord

    Assert.Equal(field1,res.Field1)
    Assert.Equal<string>(field2,res.Field2)
    Assert.True(res.Field3)
    Assert.Equal(opField1,res.OpField1)
    Assert.Equal(opField2,res.OpField2)
    Assert.Equal(opField3,res.OpField3)

    Assert.Equal(1,res.Arr1.Length)
    Assert.Equal(field1,res.Arr1.[0])
    Assert.Equal(2,res.Arr2.Length)
    Assert.Equal(field2,res.Arr2.[0])
    Assert.Equal(opField2.Value,res.Arr2.[1])
    
    Assert.Equal(3,res.Arr3.Length)
    Assert.True(res.Arr3.[0])
    Assert.False(res.Arr3.[1])
    Assert.True(res.Arr3.[2])


    
    Assert.Equal(4,res.OpArr1.Length)
    Assert.Equal(Some(field1),res.OpArr1.[0])
    Assert.Equal(None,res.OpArr1.[1])
    Assert.Equal(None,res.OpArr1.[2])
    Assert.Equal(Some(field1),res.OpArr1.[3])

    
    Assert.True(res.ArrOp1 |> Option.isSome)
    Assert.Equal(1,res.ArrOp1.Value.Length)

    Assert.True(res.ArrOp2 |> Option.isNone)




    ()
    
  [<Fact>]
  let ``Test nested record`` () = 
    let dd = new DynamicDictionary()
    let field1 = 22
    let field2 = "Alice"
    let field3 = true
    let opField1 = Some 1
    let opField2 = Some "has cat"
    let opField3 = None
    let opField = None
    dd.Add("Field.field1", 22)
    dd.Add("Field.field2", "Alice")
    dd.Add("Field.field3", true)
    dd.Add("Field.opField1", 1)
    dd.Add("Field.opField2", "has cat")
    
    dd.Add("ArrField[0].field1", 22)
    dd.Add("ArrField[0].field2", "Alice")
    dd.Add("ArrField[0].field3", true)
    dd.Add("ArrField[0].opField1", 1)
    dd.Add("ArrField[0].opField2", "has cat")
    
    Assert.True(rb.CanBind(typeof<NestedRecord>))
    let res = Parse.bind dd typeof<NestedRecord> "" :?> NestedRecord

    Assert.Equal(field1,res.Field.Field1)
    Assert.Equal<string>(field2,res.Field.Field2)
    Assert.Equal(field3,res.Field.Field3)
    Assert.Equal(opField1,res.Field.OpField1)
    Assert.Equal(opField2,res.Field.OpField2)
    Assert.Equal(opField3,res.Field.OpField3)
    Assert.Equal(opField,res.OpField)

    Assert.Equal(1,res.ArrField.Length)
    
    Assert.Equal(field1,res.ArrField.[0].Field1)
    Assert.Equal<string>(field2,res.ArrField.[0].Field2)
    Assert.Equal(field3,res.ArrField.[0].Field3)
    Assert.Equal(opField1,res.ArrField.[0].OpField1)
    Assert.Equal(opField2,res.ArrField.[0].OpField2)
    Assert.Equal(opField3,res.ArrField.[0].OpField3)

    ()

    
  [<Fact>]
  let ``Array of records`` () = 
    let dd = new DynamicDictionary()
    let field1 = 22
    let field2 = "Alice"
    let field3 = true
    let opField1 = Some 1
    let opField2 = Some "has cat"
    let opField3 = None
    dd.Add("[0].field1", 22)
    dd.Add("[0].field2", "Alice")
    dd.Add("[0].field3", true)
    dd.Add("[0].opField1", 1)
    dd.Add("[0].opField2", "has cat")
    
    Assert.True(rb.CanBind(typeof<SimpleRecord array>))
    let res = Parse.bind dd typeof<SimpleRecord array> "" :?> SimpleRecord array

    Assert.Equal(field1,res.[0].Field1)
    Assert.Equal<string>(field2,res.[0].Field2)
    Assert.Equal(field3,res.[0].Field3)
    Assert.Equal(opField1,res.[0].OpField1)
    Assert.Equal(opField2,res.[0].OpField2)
    Assert.Equal(opField3,res.[0].OpField3)

    ()
    
  [<Fact>]
  let ``Array of record options`` () = 
    let dd = new DynamicDictionary()
    let field1 = 22
    let field2 = "Alice"
    let field3 = true
    let opField1 = Some 1
    let opField2 = Some "has cat"
    let opField3 = None
    dd.Add("[1].field1", 22)
    dd.Add("[1].field2", "Alice")
    dd.Add("[1].field3", true)
    dd.Add("[1].opField1", 1)
    dd.Add("[1].opField2", "has cat")

    Assert.True(rb.CanBind(typeof<SimpleRecord option array>))
    let res = Parse.bind dd typeof<SimpleRecord option array > "" :?> SimpleRecord option array 

    Assert.Equal(2,res.Length)
    Assert.Equal(None,res.[0])
    Assert.True(res.[1] |> Option.isSome)
    Assert.Equal(field1,res.[1].Value.Field1)
    Assert.Equal<string>(field2,res.[1].Value.Field2)
    Assert.Equal(field3,res.[1].Value.Field3)
    Assert.Equal(opField1,res.[1].Value.OpField1)
    Assert.Equal(opField2,res.[1].Value.OpField2)
    Assert.Equal(opField3,res.[1].Value.OpField3)
    

    ()
    
    
  [<Fact>]
  let ``record option`` () = 
    let dd = new DynamicDictionary()
    let field1 = 22
    let field2 = "Alice"
    let field3 = true
    let opField1 = Some 1
    let opField2 = Some "has cat"
    let opField3 = None
    dd.Add("field1", 22)
    dd.Add("field2", "Alice")
    dd.Add("field3", true)
    dd.Add("opField1", 1)
    dd.Add("opField2", "has cat")

    Assert.True(rb.CanBind(typeof<Option<SimpleRecord>>))

    let res = Parse.bind dd typeof<Option<SimpleRecord>> "" :?> Option<SimpleRecord>

    Assert.True(res |> Option.isSome)
    Assert.Equal(field1,res.Value.Field1)
    Assert.Equal<string>(field2,res.Value.Field2)
    Assert.Equal(field3,res.Value.Field3)
    Assert.Equal(opField1,res.Value.OpField1)
    Assert.Equal(opField2,res.Value.OpField2)
    Assert.Equal(opField3,res.Value.OpField3)

  [<Fact>]
  let ``record option with empty request`` () =
    let dd = new DynamicDictionary()
    
    Assert.True(rb.CanBind(typeof<SimpleRecord option>))
    let res = Parse.bind dd typeof<SimpleRecord option> "" :?> SimpleRecord option

    Assert.True(res |> Option.isNone)
    
    
  [<Fact>]
  let ``record enum`` () =
    let dd = new DynamicDictionary()
    let field1 = BYTEEnum.Value
    let field2 = LINTEnum.Value
    let field3 = DINTEnum.Value
    dd.Add("field1", field1 |> byte)
    dd.Add("field2", field2 |> int64)
    dd.Add("field3", field3 |> int)
    
    Assert.True(rb.CanBind(typeof<RecordWithEnum>))
    let res = Parse.bind dd typeof<RecordWithEnum> "" :?> RecordWithEnum

    Assert.Equal(field1, res.Field1)
    Assert.Equal(field2, res.Field2)
    Assert.Equal(field3, res.Field3)
    
  [<Fact>]
  let ``record with invalid enum`` () =
    let dd = new DynamicDictionary()
    let field = 234
    dd.Add("field1", field )
    dd.Add("field2", field )
    dd.Add("field3", field )
    
    Assert.True(rb.CanBind(typeof<RecordWithEnum>))
    
    

    let ex = Assert.ThrowsAny(fun () -> Parse.bind dd typeof<RecordWithEnum> "")

    ()
