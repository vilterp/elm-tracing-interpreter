module Main exposing (..)
-- where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as App

import Viewer exposing (..)
import Style


source =
  [ "main = 2" ]


testSource =
  [ "a123456"
  , "b123456"
  , "c123456"
  , "d123456"
  , "e123456"
  ]


testSpan1 : SourceSpan
testSpan1 =
  { start = { line = 1, col = 1 }
  , end = { line = 1, col = 5 }
  }


testSpan2 : SourceSpan
testSpan2 =
  { start = { line = 2, col = 3 }
  , end = { line = 4, col = 3 }
  }


tVal =
  ( IntV 2
  , Literal
      { start = { line = 1, col = 7 }, end = { line = 1, col = 8 } }
  )


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
  in
    div [style [("display", "flex")]]
      [ div [] [viewSource overSpan source]
      , div [style Style.viewValue] [viewValue tVal]
      ]


main =
  App.beginnerProgram
    { model = initialModel source tVal
    , view = view
    , update = update
    }
