module Main exposing (..)
-- where

import Dict exposing (Dict)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as App

import Model exposing (..)
import Viewer exposing (..)
import ExampleData
import Style
import Utils exposing (..)


update : Msg -> Model -> Model
update msg model =
  case msg of
    MouseOverTrace trace ->
      { model | overTrace = Just trace }

    MouseOutTrace ->
      { model | overTrace = Nothing }

    PinCall callId ->
      { model | pinnedCall = callId }

    NoOp ->
      model


view : Model -> Html Msg
view model =
  let
    overSpan =
      model.overTrace
      `Maybe.andThen` (\trace ->
        case trace of
          Literal sourceSpan ->
            Just sourceSpan

          _ ->
            Nothing
      )
  in
    div [style [("display", "flex")]]
      [ div [] [ viewSource overSpan model.source ]
      , div [] [ viewStack model ]
      ]


viewStack : Model -> Html Msg
viewStack model =
  let
    stack =
      model.pinnedCall
      |> stackForCall model.callTree
  in
    stack
    |> List.map (\call ->
      li []
        [ text (call.name ++ ":")
        , viewStackFrame model.overTrace call
        ]
    )
    |> ul []


viewStackFrame : Maybe Trace -> Call -> Html Msg
viewStackFrame overTrace call =
  let
    viewVal val =
      div [style Style.viewValue] [viewValue overTrace val]
  in
    ul []
      [ li []
          [ text "args:"
          , ul [] (call.args |> List.map (\argVal -> li [] [viewVal argVal]))
          ]
      , li []
          [ text "result:"
          , div [style Style.viewValue] [viewVal call.result]
          ]
      ]


main =
  App.beginnerProgram
    { model =
        initialModel ExampleData.callTree ExampleData.funcDefinitionSpans ExampleData.source
    , view = view
    , update = update
    }
