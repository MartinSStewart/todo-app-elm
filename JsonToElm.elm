import Models exposing (..)
import Json.Encode exposing (..)
import Json.Encode.Extra exposing (..)
import Json.Decode exposing (..)

decodeId : Json.Decode.Decoder Id
decodeId =
    Json.Decode.map Id
        (field "todoId" Json.Decode.int)

encodeId : Id -> Json.Encode.Value
encodeId record =
    Json.Encode.object
        [ ("todoId",  Json.Encode.int <| record.todoId)
        ]

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
        , ("id",  encodeId <| record.id)
        , ("done",  Json.Encode.bool <| record.done)
        , ("color",  encodeColor <| record.color)
        ]

decodeModel : Json.Decode.Decoder Model
decodeModel =
    Json.Decode.map4 Model
        (field "todos" (Json.Decode.list decodeTodoItem))
        (field "selectedTodo" (Json.Decode.maybe decodeId))
        (field "lastId" decodeId)
        (field "colorPalette" (Json.Decode.list decodeColor))

-- encodeModel : Model -> Json.Encode.Value
-- encodeModel record =
--     Json.Encode.object
--         [ ("todos",  Json.Encode.list <| List.map encodeTodoItem <| record.todos)
--         , ("selectedTodo",  Json.Encode.maybe record.selectedTodo)
--         , ("lastId",  encodeId <| record.lastId)
--         , ("colorPalette",  Json.Encode.list <| List.map encodeColor <| record.colorPalette)
--         ]