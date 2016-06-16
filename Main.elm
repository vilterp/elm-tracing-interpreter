module Main exposing (..)
-- where

import Dict exposing (Dict)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
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
    maybeOverSpan =
      model.overTrace
      `Maybe.andThen` (\trace ->
        case trace of
          Literal callId sourceSpan ->
            Just (callId, sourceSpan)

          _ ->
            Nothing
      )
  in
    div [style [("display", "flex")]]
      [ div [] [ viewSource maybeOverSpan model.source ]
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
    |> List.map (\stackFrame ->
      li []
        [ text stackFrame.call.name
        , text ": ("
        , viewSubcallWidget stackFrame.call.subcalls stackFrame.selectedSubcall
        , text ")"
        , viewStackFrame model.overTrace stackFrame
        ]
    )
    |> ul []


viewStackFrame : Maybe Trace -> StackFrame -> Html Msg
viewStackFrame overTrace stackFrame =
  let
    viewVal val =
      div [style Style.viewValue] [viewValue overTrace val]
  in
    ul []
      [ li []
          [ text "args:"
          , ul [] (stackFrame.call.args |> List.map (\argVal -> li [] [viewVal argVal]))
          ]
      , li []
          [ text "result:"
          , div [style Style.viewValue] [viewVal stackFrame.call.result]
          ]
      ]


viewSubcallWidget : List CallId -> Maybe CallId -> Html Msg
viewSubcallWidget subcallIds maybeSelectedSubcall =
  let
    viewSubcallMarker subcallId =
      let
        theStyle =
          if (Just subcallId) == maybeSelectedSubcall then
            Style.selectedSubcall
          else
            Style.subcall
      in
        span
          [ style theStyle
          , onClick (PinCall subcallId)
          ]
          [ text "X" ]
  in
    subcallIds
    |> List.map viewSubcallMarker
    |> List.intersperse (text " ")
    |> span []


main =
  App.beginnerProgram
    { model =
        initialModel ExampleData.callTree ExampleData.funcDefinitionSpans ExampleData.source
    , view = view
    , update = update
    }
