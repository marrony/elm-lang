import Http
import Markdown
import Html exposing (Html)
import Task exposing (Task, andThen)
import Debug
import Mouse

main : Signal Html
main =
  Signal.map Markdown.toHtml readme.signal

type alias Test = {
    a : String,
    b : Int
}

readme2 : Test
readme2 =
  Test "Marrony" 30
--  {a = "Marrony", b = 30}

-- set up mailbox
--   the signal is piped directly to main
--   the address lets us update the signal
readme : Signal.Mailbox String
readme =
  Signal.mailbox (Debug.log "readme" "")


-- send some markdown to our readme mailbox
report : String -> Task x ()
report markdown =
  Signal.send readme.address markdown


-- get the readme *and then* send the result to our mailbox
port fetchReadme : Task Http.Error ()
port fetchReadme =
  Http.getString readmeUrl `andThen` report


-- the URL of the README.md that we desire
readmeUrl : String
readmeUrl =
  "https://raw.githubusercontent.com/elm-lang/core/master/README.md"

