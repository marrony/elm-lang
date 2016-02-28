import Graphics.Element exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Signal exposing (..)
import Task exposing (..)
import Time exposing (..)
import Mouse exposing (..)
import Debug exposing (..)

type Action
  = NoOp
  | Inc
  | Set Int
  | Create
  | Clear

type alias Model = {
  attrib : Int,
  ts : List Float
}

port tick : Signal (Task a ())
port tick =
  every second
  |> Debug.log "tick"
  |> Signal.map (\t -> Signal.send actions.address Inc)

port clicks : Signal (Task a ())
port clicks =
  Mouse.clicks
  |> Debug.log "clicks"
  |> Signal.map (\t -> Signal.send actions.address Create)

main : Signal Html
main = Signal.map (view actions.address) model
--main =  Signal.map (\s -> Html.text s) orders

myFoldp : (action -> state -> state) -> state -> Signal action -> Signal state
myFoldp fn ini sig =
  Signal.map2 (\a b -> fn a ini) sig

model : Signal Model
model =
  Signal.foldp update initialModel (Time.timestamp actions.signal)

initialModel : Model
initialModel =
  {attrib = 10, ts = []}

update : (Float, Action) -> Model -> Model
update (ts, action) model =
  case action of
  NoOp -> model
  Inc -> { model | attrib = model.attrib + 1 }
  Set value -> { model | attrib = value }
  Create -> { model | ts = List.append model.ts [ts] }
  Clear -> { model | ts = [] }

view : Address Action -> Model -> Html
view address model =
  Html.div [] [
    Html.button [ onClick address Inc ] [Html.text "inc"],
    Html.button [ onClick address (Set 100) ] [Html.text "set 100"],
    Html.button [ onClick address (Set 200) ] [Html.text "set 200"],
    Html.button [ onClick address Create ] [Html.text "add"],
    Html.button [ onClick address Clear ] [Html.text "clear"],
    Html.div [] [Html.text (toString model.attrib)],
    Html.div [] (
      List.map (\t -> Html.div [] [ Html.text (toString t) ]) model.ts
    )
  ]

actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp

