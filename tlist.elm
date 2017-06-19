module TList exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)

main = Html.beginnerProgram {
  model = list1,
  view = view,
  update = update
  }

type alias TList = { name : String, newCard : String, cards : List TCard }
type alias TCard = { description : String }

type Msg = Submit | Name String

update : Msg -> TList -> TList
update msg list =
  case msg of
    Name name ->
      { list | newCard = name }
    Submit ->
      let newCards = List.append list.cards [TCard list.newCard]
      in
        { list | cards = newCards, newCard = "" }

-- PLAYGROUND

card1 = TCard "Cartita"
card2 = TCard "Cartonga"

list1 : TList
list1 = TList "Listardi" "" [card1, card2]

-- VIEW

view list =
  div [ class "panel panel-default" ] [
    displayTitle list
  , displayCards list
  , displayForm list
  ]

displayTitle: TList -> Html Msg
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
      textarea [ onInput Name,
                 value list.newCard,
                 placeholder "card name",
                 class "form-control" ] []
    ]
  , button [ onClick Submit, class "btn btn-default btn-sm" ] [ text "Create Card" ]
  ]
