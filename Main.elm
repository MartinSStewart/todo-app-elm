import Html exposing (Html, button, div, text, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onDoubleClick)
import Json.Decode as Decode

main : Program Never Model Msg
main =
  Html.beginnerProgram { model = model, view = view, update = update }


-- MODEL

type alias Id = { todoId: Int }
type alias Color = { red: Int, green: Int, blue: Int }
type alias TodoItem = { name: String, id: Id, done: Bool, color: Color }
type alias Model = { todos: List TodoItem, selectedTodo: Maybe Id, lastId: Id, colorPalette: List Color }

model : Model
model = Model 
  [] 
  Maybe.Nothing 
  (Id 0) 
  [ Color 255 255 255
  , Color 200 200 200
  , Color 100 100 100
  , Color 200 100 100
  , Color 100 200 100
  , Color 100 100 200
  ]

-- UPDATE

removeFromList : Int -> List a -> List a
removeFromList i list =
  (List.take i list) ++ (List.drop (i+1) list) 

type Msg 
  = AddTodo 
  | RemoveTodo TodoItem
  | ToggleTodo TodoItem
  | DoAll
  | SetTodoName TodoItem String
  | RemoveFinished
  | SelectTodo (Maybe Id)
  | PickColor TodoItem Color

colorToString : Color -> String
colorToString color =
  "rgb(" 
  ++ toString color.red ++ ", " 
  ++ toString color.green ++ ", " 
  ++ toString color.blue ++ ")"

replaceById : { b | id : a } -> List { b | id : a } -> List { b | id : a }
replaceById newItem list =
  List.map (\x -> if x.id == newItem.id then newItem else x) list

removeById : { b | id : a } -> List { b | id : a } -> List { b | id : a }
removeById item list =
  List.filter (\x -> not (x.id == item.id)) list

update : Msg -> Model -> Model
update msg model =
  case msg of
    AddTodo ->
      { model | 
        todos = List.append model.todos [ (TodoItem "" model.lastId False (Color 255 255 255)) ], 
        lastId = { todoId = model.lastId.todoId + 1 } }

    RemoveTodo todoItem ->
      { model | todos = removeById todoItem model.todos }

    ToggleTodo todoItem ->
      { model | todos = replaceById { todoItem | done = not todoItem.done } model.todos }

    DoAll ->
      { model | todos = List.map (\x -> { x | done = True}) model.todos}

    SetTodoName todoItem newName ->
      { model | todos = replaceById { todoItem | name = newName } model.todos}

    RemoveFinished ->
      { model | todos = List.filter (\x -> not x.done) model.todos}

    SelectTodo todoId ->
      { model | selectedTodo = todoId }

    PickColor todoItem color ->
      { model | todos = replaceById { todoItem | color = color } model.todos }


-- VIEW

onEvent : a -> b -> Html.Attribute b
onEvent eventName callback = 
  Html.Events.onWithOptions 
    "click" 
    { stopPropagation = True, preventDefault = True } 
    (Decode.succeed callback)

colorBox : Color -> a -> Html a
colorBox color click =
  div 
    [ style [ ("display", "inline-block"), ("backgroundColor", colorToString color), ("width", "20pt"), ("height", "20pt") ] 
    , onEvent "click" click
    ] 
    []

todoView : List Color -> Bool -> TodoItem -> Html Msg
todoView colorPalette selected todoItem = 
  let todoBody =
    if selected then 
      [ div [] <| List.map (\x -> colorBox x <| PickColor todoItem x) colorPalette ] 
    else 
      []
  in
  div [ style [ ("backgroundColor", colorToString todoItem.color) ], onEvent "click" <| SelectTodo <| Maybe.Just todoItem.id ] 
    (
      [ input 
        [ type_ "checkbox"
        , checked todoItem.done
        , onEvent "click" (ToggleTodo todoItem)
        ] 
        []
      , input 
        [ type_ "textbox"
        , placeholder "A thing to do..."
        , value todoItem.name
        , onInput (SetTodoName todoItem) 
        ] 
        []
      , button [ onEvent "click" <| RemoveTodo todoItem ] [ text "✕" ]
      ] ++ todoBody
    )

view : Model -> Html Msg
view model =
  div 
    [ onEvent "click" <| SelectTodo Maybe.Nothing
    , style [ ("width", "100%"), ("height", "100%")] 
    ]
    ([ button [ onEvent "click" AddTodo ] [ text "+" ]
    , button [ onEvent "click" DoAll ] [ text "Finish All" ]
    , button [ onEvent "click" RemoveFinished, disabled (List.all (\x -> not x.done) model.todos) ] [ text "Remove finished"]
    ] ++ (List.map (\x -> todoView model.colorPalette ((Maybe.Just x.id) == model.selectedTodo) x) model.todos))