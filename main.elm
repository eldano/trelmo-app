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

card1 = TCard "Cartita"
card2 = TCard "Cartonga"

model : Model
model = TList "Listardi" "" [card1, card2]

-- VIEW

--<div class="panel-footer">
--  <form action="#" method="post">
--    <div class="form-group">
--      <textarea class="form-control" placeholder="Add a new card" name="title"></textarea>
--    </div>
--    <input type="submit" name="commit" value="Create Card" class="btn btn-default btn-sm"/>
--  </form>
--</div>


view model =
  div [ class "panel panel-default" ] [
    div [ class "panel-body" ] [
      h3 [ class "panel-title" ] [ text model.name ]
    ]
  , ul [ class "list-group" ] (displayCards model)
  , div [ class "panel-footer" ] [
      div [ class "form-group" ] [textarea [onInput Name,
                                            value model.newCard,
                                            placeholder "card name",
                                            class "form-control" ] [] ]
    , button [ onClick Submit, class "btn btn-default btn-sm" ] [ text "Create Card" ]
    ]
  ]

displayCard : TCard -> Html Msg
displayCard card =
  li [ class "list-group-item" ] [ text card.description ]

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
