module Main exposing (..)
-- where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as App

import Viewer exposing (..)
import Style


source =
  [ "main = 2" ]


tVal =
  ( IntV 2
  , Literal
      { start = { line = 1, col = 7 }, end = { line = 1, col = 8 } }
  )


main =
  App.beginnerProgram
    { model = initialModel source tVal
    , view = view
    , update = \msg model -> model
    }


view : Model -> Html a
view model =
  div [style [("display", "flex")]]
    [ div [] [viewSource model.source]
    , div [style Style.viewValue] [viewValue tVal]
    ]
