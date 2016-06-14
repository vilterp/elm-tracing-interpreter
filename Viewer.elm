module Viewer exposing (..)
-- where

import Dict exposing (Dict)
import String

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
viewSource : SourceSpan -> Source -> Html a
viewSource sourceSpan source =
  source
  |> mapWithIndex (\idx line ->
    li [ style Style.sourceLine ]
      [viewSourceLine (idx + 1) sourceSpan line]
  )
  |> ol [ style Style.sourceLines ]


viewSourceLine : Int -> SourceSpan -> String -> Html a
viewSourceLine lineNo sourceSpan line =
  let
    highlighted txt =
      span [style Style.highlightedSource] [text txt]

    normal txt =
      text txt

    length =
      String.length line

    sliceIt startIdx endIdx =
      span []
        [ normal (String.slice 0 startIdx line)
        , highlighted (String.slice startIdx endIdx line)
        , normal (String.slice endIdx length line)
        ]
  in
    case (compare sourceSpan.start.line lineNo, compare sourceSpan.end.line lineNo) of
      (LT, GT) -> -- XXXXXX
        sliceIt 0 length

      (LT, EQ) -> -- XXX...
        sliceIt 0 sourceSpan.end.col

      (EQ, EQ) -> -- ..XX..
        sliceIt sourceSpan.start.col sourceSpan.end.col

      (EQ, GT) -> -- ...XXX
        sliceIt sourceSpan.start.col length

      _ ->
        normal line


-- TODO: get this into list-extra
-- geez

mapWithIndex : (Int -> a -> b) -> List a -> List b
mapWithIndex f list =
  let
    go idx items =
      case items of
        [] ->
          []

        x::xs ->
          (f idx x) :: (go (idx + 1) xs)
  in
    go 0 list


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
