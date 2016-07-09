module Viewer exposing (..)


import Dict exposing (Dict)
import String

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Style
import Model exposing (..)
import Utils exposing (..)


-- TODO: source span
viewSource : Maybe (CallId, SourceSpan) -> Source -> Html a
viewSource maybeSourceSpan source =
  source
  |> mapWithIndex (\idx line ->
    li [ style Style.sourceLine ]
      [viewSourceLine (idx + 1) maybeSourceSpan line]
  )
  |> ol [ style Style.sourceLines ]


viewSourceLine : Int -> Maybe (CallId, SourceSpan) -> String -> Html a
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

      Just (callId, sourceSpan) ->
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


-- TODO: this'll emit onclick events...
viewValue : Maybe Trace -> TVal -> Html Msg
viewValue overTrace (val, trace) =
  let
    pinCall =
      case trace of
        FuncCall callId ->
          onClick (PinCall callId)

        Literal callId _ ->
          onClick (PinCall callId)
  in
    case val of
      IntV int ->
        span
          [ style Style.intV
          , onMouseEnter (MouseOverTrace trace)
          , onMouseLeave MouseOutTrace
          , pinCall
          ]
          [ text (toString int) ]

      StringV str ->
        span [style Style.stringV] [text str] -- TODO escape?

      ADTV { constructorName, args } ->
        let
          argViews =
            args
            |> List.map (viewValue overTrace)
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
              , viewValue overTrace value
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
