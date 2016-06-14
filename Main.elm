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


main =
  App.beginnerProgram
    { model = initialModel source tVal
    , view = view
    , update = \msg model -> model
    }


view : Model -> Html a
view model =
  div [style [("display", "flex")]]
    [ div [] [viewSource testSpan2 testSource]
    , div [style Style.viewValue] [viewValue tVal]
    ]
