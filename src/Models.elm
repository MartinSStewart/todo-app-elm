module Models exposing (..)


type TodoId
    = TodoId Int


type alias Color =
    { red : Int, green : Int, blue : Int }


type alias TodoItem =
    { name : String, id : TodoId, done : Bool, color : Color }


type alias Model =
    { todos : List TodoItem, selectedTodo : Maybe TodoId, lastId : Int, colorPalette : List Color }
