module TBoard exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)

main = Html.beginnerProgram {
  model = board1,
  view = view,
  update = update
  }

type alias TBoard = { name : String, lists : List TList, increment : Int }
type alias TList = { uid : Int, name : String, newCard : String, cards : List TCard }
type alias TCard = { description : String }

type Msg = AddList | AddCard Int | TypeCard Int String

update : Msg -> TBoard -> TBoard
update msg board =
  case msg of
    AddList ->
      let
        lists = List.append board.lists [(TList board.increment "NList" "" [])]
      in
        { board | lists = lists, increment = board.increment + 1 }

    TypeCard uid name ->
      let
        updateEntry list =
          if list.uid == uid then
            { list | newCard = name }
          else
            list
      in
        { board | lists = List.map updateEntry board.lists }

    AddCard uid ->
      let
        updateEntry list =
          if list.uid == uid then
            { list | cards = (List.append list.cards [TCard list.newCard]), newCard = "" }
          else
            list
      in
        { board | lists = List.map updateEntry board.lists }

-- PLAYGROUND

list1 = TList 1 "Listardi" "" [TCard "Cartita", TCard "Cartonga"]
list2 = TList 2 "Listox" "" [TCard "Cartucha" ]

board1 : TBoard
board1 = TBoard "Boarding" [list1, list2] 3

-- VIEW

view : TBoard -> Html Msg
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
    h3 [ class "panel-title" ] [ text list.name ]
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
      textarea [ onInput (TypeCard list.uid),
                 value list.newCard,
                 placeholder "card name",
                 class "form-control" ] []
    ]
  , button [ onClick (AddCard list.uid), class "btn btn-default btn-sm" ] [ text "Create Card" ]
  ]
