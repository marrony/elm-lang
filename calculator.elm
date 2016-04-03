import Html exposing (..)
import Html.Events exposing (..)
import Signal exposing (..)
import String exposing (..)
import StartApp.Simple exposing (start)
import Debug exposing (..)

type alias Model = {
  register : Float,
  answer : Maybe Float,
  stack : List Char,
  operation : Float -> Float -> Float,
  state : State
}

type State
  = Unary
  | Binary

type Action
  = NoOp
  | Clear
  | Percent
  | Operation (Float -> Float -> Float)
  | Equal
  | Digit Char

initModel : Model
initModel = {
    register = 0,
    answer = Nothing,
    stack = [],
    operation = \a b -> b,
    state = Unary
  }

update : Action -> Model -> Model
update action model =
  case action of
    NoOp -> model

    Clear -> initModel

    Percent -> { model |
      stack = String.toList <| toString <| (model.register * (displayValue model) / 100.0)
    }

    Operation op -> { model |
      register = displayValue model,
      stack = [],
      operation = op,
      state = Unary
    }

    Equal -> case model.state of
      Unary -> doUnary model
      Binary -> doBinary model

    Digit digit -> { model |
      stack = model.stack ++ [ digit ],
      state = Binary
    }

displayValue : Model -> Float
displayValue model =
  stackValueOrDefault model.stack (answerOrRegister model)

answerOrRegister : Model -> Float
answerOrRegister model =
  Maybe.withDefault model.register model.answer

stackValueOrDefault : List Char -> Float -> Float
stackValueOrDefault stack defaultValue =
  let
    display = String.concat <| List.map String.fromChar stack
  in
    case String.toFloat display of
      Ok value -> value
      Err _ -> defaultValue

doBinary : Model -> Model
doBinary model =
  let
    display = displayValue model
    answer = model.operation model.register display
  in { model |
    state = Unary,
    answer = Just answer,
    register = display,
    stack = []
  }

doUnary : Model -> Model
doUnary model =
  let
    answer = model.operation (answerOrRegister model) model.register
  in { model |
    answer = Just answer,
    stack = []
  }

display : Model -> String
display model =
  let
    number = displayValue model
  in
    if isNaN number then
      "Not a number"
    else if isInfinite number then
      "Infinity number"
    else
      toString number

view : Signal.Address Action -> Model -> Html
view address model =
  Html.div [] [
    Html.div [] [ Html.text (display model) ],
    Html.div [] [
      Html.button [ onClick address Clear ] [ Html.text "C" ],
      Html.button [] [ Html.text "+/-" ],
      Html.button [ onClick address Percent ] [ Html.text "%" ],
      Html.button [ onClick address (Operation (/)) ] [ Html.text "/" ]
    ],
    Html.div [] [
      Html.button [ onClick address (Digit '7') ] [ Html.text "7" ],
      Html.button [ onClick address (Digit '8') ] [ Html.text "8" ],
      Html.button [ onClick address (Digit '9') ] [ Html.text "9" ],
      Html.button [ onClick address (Operation (*)) ] [ Html.text "X" ]
    ],
    Html.div [] [
      Html.button [ onClick address (Digit '4') ] [ Html.text "4" ],
      Html.button [ onClick address (Digit '5') ] [ Html.text "5" ],
      Html.button [ onClick address (Digit '6') ] [ Html.text "6" ],
      Html.button [ onClick address (Operation (-)) ] [ Html.text "-" ]
    ],
    Html.div [] [
      Html.button [ onClick address (Digit '1') ] [ Html.text "1" ],
      Html.button [ onClick address (Digit '2') ] [ Html.text "2" ],
      Html.button [ onClick address (Digit '3') ] [ Html.text "3" ],
      Html.button [ onClick address (Operation (+)) ] [ Html.text "+" ]
    ],
    Html.div [] [
      Html.button [ onClick address (Digit '0') ] [ Html.text "0" ],
      Html.button [ onClick address (Digit '0') ] [ Html.text "0" ],
      Html.button [ onClick address (Digit '.') ] [ Html.text "." ],
      Html.button [ onClick address Equal ] [ Html.text "=" ]
    ],
    Html.div [] [ Html.text ("state " ++ (toString model.state)) ],
    Html.div [] [ Html.text ("answer " ++ toString model.answer) ],
    Html.div [] [ Html.text ("register " ++ toString model.register) ],
    Html.div [] [ Html.text ("stack " ++ toString model.stack) ]
  ]

main : Signal Html
main =
  start {
    model = initModel,
    update = update,
    view = view
  }

