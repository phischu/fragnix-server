import Graphics.Element (..)
import Text (..)
import List (..)
import List as List
import Json.Decode (..)

type alias Slice = {
    sliceID : SliceID,
    language : List Extension,
    fragment : List Declaration
}
type alias SliceID = Int
type alias Extension = String
type alias Declaration = String
type alias Usage = {
    qualification : Maybe ModuleName,
    usedName : String,
    reference : Reference
}
type Reference = OtherSlice SliceID | Primitive ModuleName
type alias ModuleName = String

renderSlice : Slice -> Element
renderSlice slice = flow down (List.map plainText slice.fragment)

exampleSlice : Slice
exampleSlice = {
    sliceID = 5,
    language = [],
    fragment = [exampleDeclaration]}

exampleDeclaration : Declaration
exampleDeclaration = "main = putStrLn \"Hello world\""

sliceDecoder : Decoder Slice
sliceDecoder = object3 Slice
    ("sliceID" := int)
    ("language" := list string)
    ("fragment" := list string)

exampleSliceString : String
exampleSliceString = "{\"sliceID\":843391,\"language\":[],\"fragment\":[\"main = wht\"]}"

main : Element
main = renderSliceString


renderSliceString = case decodeString sliceDecoder exampleSliceString of
    Ok slice -> renderSlice slice
    Err error -> plainText error
