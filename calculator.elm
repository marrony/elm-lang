import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes as Attributes exposing (..)
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
  | Negate
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

addDigit : Model -> Char -> Model
addDigit model digit =
  if digit == '0' && (List.isEmpty model.stack) then
    model
  else if digit == '.' && (List.isEmpty model.stack) then
    { model |
      stack = model.stack ++ [ '0', '.' ],
      state = Binary
    }
  else
    { model |
      stack = model.stack ++ [ digit ],
      state = Binary
    }
  
update : Action -> Model -> Model
update action model =
  case action of
    NoOp -> model

    Clear -> initModel

    Negate -> { model |
      stack = String.toList <| toString <| -(displayValue model)
    }

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

    Digit digit -> addDigit model digit

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

headerStyle : String -> String -> List (String, String)
headerStyle fgColor bgColor =
  [ ("width", "35px")
  , ("height", "35px")
  , ("background-color", bgColor)
  , ("color", fgColor)
  , ("text-align", "center")
  , ("padding", "5px")
  , ("margin", "0px")
  , ("border", "1px solid rgb(142, 142, 142)")
  ]

buttonStyle1 : Html.Attribute
buttonStyle1 = Attributes.style (headerStyle "black" "rgb(224, 224, 224)")

buttonStyle2 : Html.Attribute
buttonStyle2 = Attributes.style (headerStyle "white" "rgb(245, 146, 62)")

buttonStyle3 : Html.Attribute
buttonStyle3 = Attributes.style (headerStyle "black" "rgb(214, 214, 214)")

tableStyle : Html.Attribute
tableStyle = Attributes.style
  [ ("border-spacing", "0px")
  , ("border-collapse", "collapse")
  ]

displayStyle : Html.Attribute
displayStyle = Attributes.style
  [ ("width", "100%")
  ]

displayInnerStyle : Html.Attribute
displayInnerStyle = Attributes.style
  [ ("background-color", "rgb(76, 76, 76)")
  , ("padding", "10px")
  , ("font-size", "xx-large")
  , ("text-align", "right")
  , ("color", "white")
  , ("display", "block")
  ]

calculatorStyle : Html.Attribute
calculatorStyle = Attributes.style
  [ ("display", "inline-block") ]

view : Signal.Address Action -> Model -> Html
view address model =
  Html.div [ calculatorStyle ] [
    Html.div [ displayStyle ] [
      Html.div [ displayInnerStyle ] [ Html.text (display model) ]
    ],
    Html.table [ tableStyle ] [
      Html.tr [] [
        Html.td [ buttonStyle3, onClick address Clear ] [ Html.text "AC" ],
        Html.td [ buttonStyle3, onClick address Negate ] [ Html.text "+/-" ],
        Html.td [ buttonStyle3, onClick address Percent ] [ Html.text "%" ],
        Html.td [ buttonStyle2, onClick address (Operation (/)) ] [ Html.text "/" ]
      ],
      Html.tr [] [
        Html.td [ buttonStyle1, onClick address (Digit '7') ] [ Html.text "7" ],
        Html.td [ buttonStyle1, onClick address (Digit '8') ] [ Html.text "8" ],
        Html.td [ buttonStyle1, onClick address (Digit '9') ] [ Html.text "9" ],
        Html.td [ buttonStyle2, onClick address (Operation (*)) ] [ Html.text "x" ]
      ],
      Html.tr [] [
        Html.td [ buttonStyle1, onClick address (Digit '4') ] [ Html.text "4" ],
        Html.td [ buttonStyle1, onClick address (Digit '5') ] [ Html.text "5" ],
        Html.td [ buttonStyle1, onClick address (Digit '6') ] [ Html.text "6" ],
        Html.td [ buttonStyle2, onClick address (Operation (-)) ] [ Html.text "-" ]
      ],
      Html.tr [] [
        Html.td [ buttonStyle1, onClick address (Digit '1') ] [ Html.text "1" ],
        Html.td [ buttonStyle1, onClick address (Digit '2') ] [ Html.text "2" ],
        Html.td [ buttonStyle1, onClick address (Digit '3') ] [ Html.text "3" ],
        Html.td [ buttonStyle2, onClick address (Operation (+)) ] [ Html.text "+" ]
      ],
      Html.tr [] [
        Html.td [ buttonStyle1, onClick address (Digit '0') ] [ Html.text "0" ],
        Html.td [ buttonStyle1, onClick address (Digit '0') ] [ Html.text "0" ],
        Html.td [ buttonStyle1, onClick address (Digit '.') ] [ Html.text "." ],
        Html.td [ buttonStyle2, onClick address Equal ] [ Html.text "=" ]
      ]
    ],
    Html.div [] [ Html.text ("state " ++ (toString model.state)) ],
    Html.div [] [ Html.text ("answer " ++ toString model.answer) ],
    Html.div [] [ Html.text ("register " ++ toString model.register) ],
    Html.div [] [ Html.text ("stack " ++ toString model.stack) ]
  ]

main : Signal Html
main =
  StartApp.Simple.start {
    model = initModel,
    update = update,
    view = view
  }

