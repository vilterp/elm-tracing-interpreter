module Viewer exposing (..)
-- where

import Dict exposing (Dict)
import String

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Style

type Msg
  = MouseOverTrace Trace
  | MouseOutTrace


type alias Model =
  { callTree : CallTree
  , funcDefinitionSpans : FuncDefinitionSpans
  , source : Source
  , stack : List StackFrame
  , overTrace : Maybe Trace -- ...?
  }


type alias FuncDefinitionSpans =
  Dict FuncName SourceSpan


type alias CallTree =
  { calls : Dict CallId Call
  , root : CallId
  }


type alias CallId =
  Int


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
  | ClosureV
      { name : Maybe String
      , definition : SourceSpan
      , closure : Dict String TVal
      }


type Trace
  = FuncCall CallId
  | Literal SourceSpan


type alias Call =
  { name : FuncName
  , args : List TVal
  , result : TVal
  , calls : List CallId
  , caller : Maybe (CallId, SourceSpan) -- Nothing <=> this is main
  }


type alias FuncName =
  String


type alias SourceSpan =
  { start : SourceLoc
  , end : SourceLoc
  }


type alias SourceLoc =
  { line : Int
  , col : Int
  }


type alias Source =
  List String


type alias StackFrame =
  { valuePath : Maybe ValuePath
  , call : CallId
  }


type ValuePath
  = ConstructorArg Int
  | RecordField String
  | ListItem Int


-- TODO: source span
viewSource : Maybe SourceSpan -> Source -> Html a
viewSource maybeSourceSpan source =
  source
  |> mapWithIndex (\idx line ->
    li [ style Style.sourceLine ]
      [viewSourceLine (idx + 1) maybeSourceSpan line]
  )
  |> ol [ style Style.sourceLines ]


viewSourceLine : Int -> Maybe SourceSpan -> String -> Html a
viewSourceLine lineNo maybeSourceSpan line =
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
    case maybeSourceSpan of
      Nothing ->
        normal line

      Just sourceSpan ->
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
viewValue : TVal -> Html Msg
viewValue (val, trace) =
  case val of
    IntV int ->
      span
        [ style Style.intV
        , onMouseEnter (MouseOverTrace trace)
        , onMouseLeave MouseOutTrace
        ]
        [ text (toString int) ]

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

    ClosureV attrs ->
      text "ClosureV (TODO)"


getMaybe : String -> Maybe a -> a
getMaybe msg maybe =
  case maybe of
    Just x ->
      x

    Nothing ->
      Debug.crash msg


initialModel : CallTree -> FuncDefinitionSpans -> Source -> Model
initialModel callTree funcDefinitionSpans source =
  { callTree = callTree
  , funcDefinitionSpans = funcDefinitionSpans
  , source = source
  , stack = []
  , overTrace = Nothing
  }
