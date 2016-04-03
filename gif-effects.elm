import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Http exposing (..)
import Json.Decode as Json
import Signal exposing (..)
import Task exposing (..)
import Effects exposing (..)
import StartApp exposing (start)
import Debug exposing (..)

type alias Model = {
  topic : String,
  url : String
}

type Action
  = RequestMore
  | NewGif (Maybe String)

init : String -> (Model, Effects Action)
init topic =
  (Model topic "assets/waiting.gif", randomGif topic)

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    RequestMore ->
      Debug.log "RequestMore"
      (model, randomGif model.topic)

    NewGif url ->
      Debug.log "NewGif"
      (Model model.topic (Maybe.withDefault model.url url), Effects.none)

view : Signal.Address Action -> Model -> Html
view address model =
  Html.div [] [
    Html.h2 [] [ Html.text model.topic ],
    Html.h2 [] [ Html.text model.url ],
    Html.button [onClick address RequestMore] [ Html.text "more images" ],
    Html.div [] [ Html.img [ src model.url ] [] ]
  ]

randomGif : String -> Effects Action
randomGif topic =
  Http.get decodeUrl (randomUrl topic)
    |> Task.toMaybe
    |> Task.map NewGif
    |> Effects.task

randomUrl : String -> String
randomUrl topic =
  Http.url "http://api.giphy.com/v1/gifs/random"
    [ ("api_key", "dc6zaTOxFJmzC"), ("tag", topic) ]

decodeUrl : Json.Decoder String
decodeUrl =
  Json.at ["data", "image_url"] Json.string

app =
  StartApp.start {
    init = init "funny cats",
    update = update,
    view = view,
    inputs = []
  }

main : Signal Html
main = app.html

port tasks : Signal (Task.Task Never ())
port tasks = app.tasks

