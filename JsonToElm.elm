module JsonToElm exposing (..)

import Json.Decode exposing (int, string, float, Decoder, nullable, field, andThen, fail, succeed)
import Json.Encode exposing (..)
import Models exposing (..)

encodeId : TodoId -> Value
encodeId id =
  Json.Encode.int <| case id of TId value -> value

decodeId : Decoder TodoId
decodeId =
  let 
    convert : String -> Decoder TodoId
    convert raw = 
      case String.toInt raw of
        Ok value -> succeed (TId value)

        Err error -> fail error
  in
  Json.Decode.string |> andThen convert


decodeColor : Json.Decode.Decoder Color
decodeColor =
    Json.Decode.map3 Color
        (field "red" Json.Decode.int)
        (field "green" Json.Decode.int)
        (field "blue" Json.Decode.int)

encodeColor : Color -> Json.Encode.Value
encodeColor record =
    Json.Encode.object
        [ ("red",  Json.Encode.int <| record.red)
        , ("green",  Json.Encode.int <| record.green)
        , ("blue",  Json.Encode.int <| record.blue)
        ]

decodeTodoItem : Json.Decode.Decoder TodoItem
decodeTodoItem =
    Json.Decode.map4 TodoItem
        (field "name" Json.Decode.string)
        (field "id" decodeId)
        (field "done" Json.Decode.bool)
        (field "color" decodeColor)

encodeTodoItem : TodoItem -> Json.Encode.Value
encodeTodoItem record =
    Json.Encode.object
        [ ("name",  Json.Encode.string <| record.name)
        , ("id",  encodeId record.id)
        , ("done",  Json.Encode.bool <| record.done)
        , ("color",  encodeColor <| record.color)
        ]

decodeModel : Json.Decode.Decoder Model
decodeModel =
    Json.Decode.map4 Model
        (field "todos" <| Json.Decode.list decodeTodoItem)
        (field "selectedTodo" (Json.Decode.maybe decodeId))
        (field "lastId" Json.Decode.int)
        (field "colorPalette" <| Json.Decode.list decodeColor)

encodeModel : Model -> Json.Encode.Value
encodeModel record =
    Json.Encode.object
        [ ("todos",  Json.Encode.list <| List.map encodeTodoItem <| record.todos)
        , ("selectedTodo", encodeMaybe encodeId record.selectedTodo)
        , ("lastId",  Json.Encode.int record.lastId)
        , ("colorPalette",  Json.Encode.list <| List.map encodeColor <| record.colorPalette)
        ]

encodeMaybe : (a -> Value) -> Maybe a -> Value
encodeMaybe encoder maybe =
  case maybe of 
    Just value -> encoder value
    Nothing -> Json.Encode.null