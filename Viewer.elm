module Viewer exposing (..)
-- where

import Dict exposing (Dict)

import Html exposing (..)
import Html.Attributes exposing (..)

import Style


type alias TVal =
  (Val, Trace)


type Val
  = IntV Int
  | StringV String
  | ADTV
      { constructorName : String
      , args : List TVal
      }
  | RecordV (Dict String TVal)


type Trace
  = FuncCall { name : String, args : List TVal }
  | Literal SourceSpan


type alias SourceSpan =
  { start : SourceLoc
  , end : SourceLoc
  }


type alias SourceLoc =
  { line : Int
  , col : Int
  }


type alias Model =
  { rootVal : TVal
  , stack : List StackFrame
  , source : Source
  }


type alias Source =
  List String


type alias StackFrame =
  { valuePath : ValuePath
  , tval : TVal
  }


type ValuePath
  = ConstructorArg Int
  | RecordField String
  | ListItem Int


--

-- TODO: source span
viewSource : Source -> Html a
viewSource source =
  source
  |> List.map (\line -> li [ style Style.sourceLine ] [text line])
  |> ol [ style Style.sourceLines ]


-- TODO: get this into list-extra
-- geez

mapWithIndex : (Int -> a -> b) -> List a -> List b
mapWithIndex f list =
  List.foldl
    (\item (idx, soFar) -> (idx + 1, f idx item :: soFar))
    (0, [])
    list
  |> snd


-- TODO: this'll emit onclick events...
viewValue : TVal -> Html a
viewValue (val, trace) =
  case val of
    IntV int ->
      span [style Style.intV] [text (toString val)]

    StringV str ->
      span [style Style.stringV] [text str] -- TODO escape?

    ADTV { constructorName, args } ->
      let
        argViews =
          args
          |> List.map viewValue
          |> List.intersperse (text " ")
      in
        List.concat
          [ [ span [style Style.constructorName] [text constructorName] ]
          , [ text " " ]
          , argViews
          ]
        |> span []

    RecordV attrs ->
      let
        comma =
          span [style Style.syntax] [text ", "]

        viewAttr (key, value) =
          span []
            [ span [style Style.recordKey] [text key]
            , span [style Style.syntax] [text " = "]
            , viewValue value
            ]

        attrViews =
          attrs
          |> Dict.toList
          |> List.map viewAttr
          |> List.intersperse comma
      in
        List.concat
          [ [ span [style Style.syntax] [text "{"] ]
          , attrViews
          , [ span [style Style.syntax] [text "}"] ]
          ]
        |> span []


initialModel : Source -> TVal -> Model
initialModel source tval =
  { source = source
  , rootVal = tval
  , stack = []
  }
