import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)

main = Html.beginnerProgram {
  model = model,
  view = view,
  update = update
  }

type alias Model = TList
type alias TList = { name : String, newCard: String, cards : List TCard }
type alias TCard = { description : String }

type Msg = Submit | Name String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | newCard = name }
    Submit ->
      let newCards = List.append model.cards [TCard model.newCard]
      in
        { model | cards = newCards, newCard = "" }

-- PLAYGROUND

card1 = TCard "cartita"
card2 = TCard "cartonga"

model : Model
model = TList "listin" "" [card1, card2]

-- VIEW

view model =
  div [] [
    h2 [] [ text model.name ]
  , div [] (displayCards model)
  , div [] (displayForm model)
  ]

displayCard : TCard -> Html Msg
displayCard card =
  div [] [ text card.description ]

displayCards : Model -> List (Html Msg)
displayCards list =
  List.map displayCard list.cards

displayForm : Model -> List (Html Msg)
displayForm model =
  [
    div [] [textarea [onInput Name,
                      value model.newCard,
                      placeholder "card name"] [] ]
  , div [] [ button [ onClick Submit ] [ text "Submit" ] ]
  ]
