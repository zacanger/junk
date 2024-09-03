module EventLog where

import Html               exposing (..)
import Html.Events        exposing (..)
import Html.Attributes    exposing (..)
import Signal             exposing (..)
import StartApp

type alias Model =
  { events: List String }

initialModel : Model
initialModel =
  { events = [] }

type Action = Mark String
            | Reset

actions: Mailbox Action
actions =
  mailbox Reset

model: Signal Model
model =
  foldp update initialModel actions.signal

update : Action -> Model -> Model
update action log =
  case action of
    Mark date ->
      { log | events <- log.events ++ [date] }
    Reset ->
      { log | events <- [] }

view : Address Action -> Model -> Html
view address model =
  div []
    [ button [ onClick address (Mark "?") ]
      [ text "Mark" ],
    h2 []
      [ text (model.events |> List.length |> toString), text " Events" ],
    div []
      (List.map (\t -> text t) model.events)]

main : Signal Html
main =
  map (view actions.address) model
