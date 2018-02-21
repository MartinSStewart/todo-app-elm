module Models exposing (..)
import Json.Decode

type alias Id = { todoId: Int }
type alias Color = { red: Int, green: Int, blue: Int }
type alias TodoItem = { name: String, id: Id, done: Bool, color: Color }
type alias Model = { todos: List TodoItem, selectedTodo: Maybe Id, lastId: Id, colorPalette: List Color }