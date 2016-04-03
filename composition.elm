import Html exposing (..)
import Html.Events exposing (..)
import Signal exposing (..)
import StartApp.Simple exposing (start)
import Debug exposing (..)

--Counter

type alias CounterModel = Int
type CounterAction = IncrementCounter | DecrementCounter

initCounter : Int -> CounterModel
initCounter value = value

updateCounter : CounterAction -> CounterModel -> CounterModel
updateCounter action model =
  case action of
    IncrementCounter -> Debug.log "Inc" ( model + 1 )
    DecrementCounter -> Debug.log "Dec" ( model - 1 )

viewCounter : Signal.Address CounterAction -> CounterModel -> Html
viewCounter address model =
  Html.span [] [
    Html.button [onClick address IncrementCounter] [Html.text "+"],
    Html.span [] [Html.text (toString model)],
    Html.button [onClick address DecrementCounter] [Html.text "-"]
  ]

--Composite

type alias CompositeCounterModel = {
  counter1 : CounterModel,
  counter2 : CounterModel
}

type CompositeAction
   = Reset
   | Counter1 CounterAction
   | Counter2 CounterAction

initComposite : Int -> Int -> CompositeCounterModel
initComposite value1 value2 =
  {counter1 = initCounter value1, counter2 = initCounter value2}

updateComposite : CompositeAction -> CompositeCounterModel -> CompositeCounterModel
updateComposite action model =
  case action of
    Reset -> Debug.log "Reset" ( initComposite 0 0 )

    Counter1 act -> { model |
      counter1 = Debug.log "Counter1" ( updateCounter act model.counter1 ) }

    Counter2 act -> { model |
      counter2 = Debug.log "Counter2" ( updateCounter act model.counter2 ) }

viewComposite : Signal.Address CompositeAction -> CompositeCounterModel -> Html
viewComposite address model =
  Html.div [] [
    Html.div [] [ viewCounter (Signal.forwardTo address Counter1) model.counter1 ],
    Html.div [] [ viewCounter (Signal.forwardTo address Counter2) model.counter2 ],
    Html.button [onClick address Reset] [Html.text "Reset"]
  ]

--CounterList

type alias ID = Int

type alias CounterListModel = {
  counters : List (ID, CounterModel),
  nextID : ID
}

type CounterListAction
   = InsertCounter
   | RemoveCounter
   | ResetCounters
   | RemoveSpecific ID
   | ModifyCounter ID CounterAction

initCounterList : CounterListModel
initCounterList = CounterListModel [] 0

updateCounterList : CounterListAction -> CounterListModel -> CounterListModel
updateCounterList action model =
  case action of
    InsertCounter ->
      let
        newCounter = (model.nextID, initCounter 0)
        newCounters = model.counters ++ [ newCounter ]
      in
        { model | counters = newCounters, nextID = model.nextID + 1 }

    RemoveCounter ->
      { model | counters = List.drop 1 model.counters }

    ResetCounters ->
      let
        reset (counterID, counterModel) = (counterID, initCounter 0)
      in
        { model | counters = List.map reset model.counters }

    RemoveSpecific id -> { model |
      counters = List.filter (\(counterId, counter) -> counterId /= id) model.counters
    }

    ModifyCounter id act ->
      let update (counterID, counterModel) =
        if counterID == id then
          (counterID, updateCounter act counterModel)
        else
          (counterID, counterModel)
      in
        { model | counters = Debug.log "ModifyCounter" ( List.map update model.counters ) }

viewCounterList : Signal.Address CounterListAction -> CounterListModel -> Html
viewCounterList address model =
  let
    counters = List.map (viewCounterAux address) model.counters
    insert = Html.button [onClick address InsertCounter] [Html.text "Insert"]
    remove = Html.button [onClick address RemoveCounter] [Html.text "Remove"]
    reset = Html.button [onClick address ResetCounters] [Html.text "Reset"]
  in
    Html.div[] ([insert, remove, reset] ++ counters)

viewCounterAux : Signal.Address CounterListAction -> (ID, CounterModel) -> Html
viewCounterAux address (id, model) =
  Html.div [] [
    viewCounter (Signal.forwardTo address (ModifyCounter id)) model,
    Html.button [ onClick address (RemoveSpecific id) ] [ Html.text "Remove" ]
  ]

--CompositeAll

type alias CompositeAllModel = {
  compositeCounter : CompositeCounterModel,
  counterList : CounterListModel
}

type CompositeAllAction
   = NoOp1 CompositeAction
   | NoOp2 CounterListAction

initCompositeAll : CompositeAllModel
initCompositeAll =
  { compositeCounter = initComposite 0 0, counterList = initCounterList }

updateCompositeAll : CompositeAllAction -> CompositeAllModel -> CompositeAllModel
updateCompositeAll action model =
  case action of
    NoOp1 act -> { model |
      compositeCounter = Debug.log "NoOp1" ( updateComposite act model.compositeCounter ) }
    
    NoOp2 act -> { model |
      counterList = Debug.log "NoOp2" ( updateCounterList act model.counterList ) }

viewCompositeAll : Signal.Address CompositeAllAction -> CompositeAllModel -> Html
viewCompositeAll address model =
  Html.div [] [
    Html.span [] [ viewComposite (Signal.forwardTo address NoOp1) model.compositeCounter ],
    Html.span [] [ viewCounterList (Signal.forwardTo address NoOp2) model.counterList ]
  ]

--main
type TypeA = Ins | Mod

type TypeB
  = Remove TypeA
  | Modify TypeA
  | Blah Int TypeA

--always : a -> b -> a

main : Signal Html
main =
  let
    f : a -> TypeB
    f = always (Remove Ins)
    h : TypeA -> TypeB
    h = Blah 10
    g : TypeB
    g = f "blah"
  in
  start {
    model = initCompositeAll,
    update = updateCompositeAll,
    view = viewCompositeAll
  }

