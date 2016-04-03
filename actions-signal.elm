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

port tick2 : Signal (Task a ())
port tick2 =
  Signal.map (\t -> Signal.send actions2.address Inc2) (every (2*second))

main : Signal Html
main =
  let
    s1 = Signal.map (view actions.address) model
    s2 = Signal.map (view2 actions2.address) model2
  in
    Signal.map2 (\d1 d2 -> Html.div[] [d1, d2]) s1 s2

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

---------------------------------------------------
type Action2
  = NoOp2
  | Inc2


model2 : Signal Int
model2 =
  Signal.foldp update2 initialModel2 (Time.timestamp actions2.signal)

initialModel2 : Int
initialModel2 = 0

update2 : (Float, Action2) -> Int -> Int
update2 (ts, action) model =
  case action of
  NoOp2 -> model
  Inc2 -> model + 1

view2 : Address Action2 -> Int -> Html
view2 address model = Html.div [] [ Html.text (toString model) ]

actions2 : Signal.Mailbox Action2
actions2 =
  Signal.mailbox NoOp2

