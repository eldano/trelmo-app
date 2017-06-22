module TBoard exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)

main = Html.program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }

-- MODEL

type alias Model = TBoard
type alias TBoard = { name : String, lists : List TList, increment : Int }
type alias TList = { id : Int, name : String, newCard : String, cards : List TCard, isEditing : Bool }
type alias TCard = { description : String }

init : (Model, Cmd Msg)
init =
  let
    list1 = TList 1 "Listardi" "" [TCard "Cartita", TCard "Cartonga"] False
    list2 = TList 2 "Listox" "" [TCard "Cartucha" ] False
  in
    (TBoard "Boarding" [list1, list2] 3, Cmd.none)

-- UPDATE

type Msg = AddList
         | DeleteList Int
         | AddCard Int
         | TypeCard Int String
         | EditModeList Int
         | TypeList Int String

update : Msg -> Model -> (Model, Cmd Msg)
update msg board =
  case msg of
    EditModeList id ->
      let
        updateEntry list =
          if list.id == id then
            { list | isEditing = not list.isEditing }
          else
            list
      in
        ({ board | lists = List.map updateEntry board.lists }, Cmd.none)

    TypeList id name ->
      let
        updateEntry list =
          if list.id == id then
            { list | name = name }
          else
            list
      in
        ({ board | lists = List.map updateEntry board.lists }, Cmd.none)

    AddList ->
      let
        lists = List.append board.lists [(TList board.increment "New List" "" [] False)]
      in
        ({ board | lists = lists, increment = board.increment + 1 }, Cmd.none)

    DeleteList id ->
      ({ board | lists = List.filter (\n -> n.id /= id) board.lists }, Cmd.none)

    TypeCard id name ->
      let
        updateEntry list =
          if list.id == id then
            { list | newCard = name }
          else
            list
      in
        ({ board | lists = List.map updateEntry board.lists }, Cmd.none)

    AddCard id ->
      let
        updateEntry list =
          if list.id == id then
            { list | cards = (List.append list.cards [TCard list.newCard]), newCard = "" }
          else
            list
      in
        ({ board | lists = List.map updateEntry board.lists }, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW

view : Model -> Html Msg
view board =
  div [ class "col-xs-12" ] [
    h3 [] [ text board.name ]
  , hr [] []
  , div [ class "row" ]
      ((List.map displayList board.lists) ++
      [displayNewList])
  ]

displayNewList : Html Msg
displayNewList =
  div [ class "col-xs-12 col-sm-3" ] [
    div [ class "panel panel-default" ] [
      a [ onClick AddList ] [
        div [ class "panel-body" ] [
          h3 [ class "panel-title" ] [ text "Create new list..." ]
        ]
      ]
    ]
  ]

displayList : TList -> Html Msg
displayList list =
  div [ class "col-xs-12 col-sm-3" ] [
    div [ class "panel panel-default" ] [
      displayTitle list
    , displayCards list
    , displayForm list
    ]
  ]

displayTitle : TList -> Html Msg
displayTitle list =
  div [ class "panel-body" ] [
    if list.isEditing then
      div [ class "form-inline edit-title" ] [
        input [ onInput (TypeList list.id), value list.name, class "form-control" ] []
      , button [ onClick (EditModeList list.id), class "btn btn-default btn-sm" ] [ text "OK" ]
      ]
    else
      h3 [ class "panel-title" ] [
        span [ onClick (EditModeList list.id) ] [ text list.name ]
      , a [ class "delete-btn", onClick (DeleteList list.id) ] [
          span [ class "glyphicon glyphicon-remove" ] []
        ]
      ]
  ]

displayCard : TCard -> Html Msg
displayCard card =
  li [ class "list-group-item" ] [ text card.description ]

displayCards : TList -> Html Msg
displayCards list =
  ul [class "list-group"] (List.map displayCard list.cards)

displayForm : TList -> Html Msg
displayForm list =
  div [ class "panel-footer" ] [
    div [ class "form-group" ] [
      textarea [ onInput (TypeCard list.id),
                 value list.newCard,
                 placeholder "card name",
                 class "form-control" ] []
    ]
  , button [ onClick (AddCard list.id), class "btn btn-default btn-sm" ] [ text "Create Card" ]
  ]
