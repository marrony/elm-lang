import Graphics.Element exposing (show)
import Task exposing (Task)
import Time exposing (second, Time)
import Debug
import Graphics.Element exposing (..)

-- A signal that updates to the current time every second
clock : Signal Time
clock =
  Time.every second

print : a -> Task x ()
print value =
  {-let
    y = Debug.log "print" value
  in-}
    Task.succeed ()

-- Turn the clock into a signal of tasks
printTasks : Signal (Task x ())
printTasks =
  let
    x = Debug.log "printTasks" runner
  in
    Signal.map print clock

port blah : String
port blah =
  Debug.log "blah" "test"

-- Actually perform all those tasks
port runner : Signal (Task x ())
port runner =
  let
    x = Debug.log "runner" runner
  in
    printTasks


main : Signal Element
--  show (Debug.log "main" "Open your browser's Developer Console.")
main =
  Signal.map show contentMailbox.signal


contentMailbox : Signal.Mailbox String
contentMailbox =
  Signal.mailbox "mailbox"

sendMessage : Float -> Task x ()
sendMessage time =
  Signal.send contentMailbox.address ("hello from clock! " ++ toString time)

port everySecond : Signal (Task x ())
port everySecond =
  Signal.map sendMessage clock

port updateContent : Task x ()
port updateContent =
  Signal.send contentMailbox.address "hello!"


