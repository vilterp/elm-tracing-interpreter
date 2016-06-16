module Main exposing (..)
-- where

import Dict exposing (Dict)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as App

import Viewer exposing (..)
import ExampleData
import Style


update : Msg -> Model -> Model
update msg model =
  case msg of
    MouseOverTrace trace ->
      { model | overTrace = Just trace }

    MouseOutTrace ->
      { model | overTrace = Nothing }


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

    outputTVal =
      model.callTree.calls
      |> Dict.get (model.callTree.root)
      |> getMaybe "no root"
      |> .result
  in
    div [style [("display", "flex")]]
      [ div [] [viewSource overSpan model.source]
      , div [style Style.viewValue] [viewValue outputTVal]
      ]


main =
  App.beginnerProgram
    { model =
        initialModel ExampleData.callTree ExampleData.funcDefinitionSpans ExampleData.source
    , view = view
    , update = update
    }
