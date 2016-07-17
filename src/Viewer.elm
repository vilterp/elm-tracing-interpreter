module Viewer exposing (..)

import Dict exposing (Dict)
import String

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Style
import Model exposing (..)
import Elm.Trace exposing (..)
import Elm.AST exposing (Region)
import Utils exposing (..)


-- TODO: source span
viewSource : Maybe (CallId, Region) -> Source -> Html a
viewSource maybeRegion source =
  source
  |> mapWithIndex (\idx line ->
    li [ style Style.sourceLine ]
      [viewSourceLine (idx + 1) maybeRegion line]
  )
  |> ol [ style Style.sourceLines ]


viewSourceLine : Int -> Maybe (CallId, Region) -> String -> Html a
viewSourceLine lineNo maybeRegion line =
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
    case maybeRegion of
      Nothing ->
        normal line

      Just (callId, region) ->
        case (compare region.start.line lineNo, compare region.end.line lineNo) of
          (LT, GT) -> -- XXXXXX
            sliceIt 0 length

          (LT, EQ) -> -- XXX...
            sliceIt 0 (region.end.column - 1)

          (EQ, EQ) -> -- ..XX..
            sliceIt (region.start.column - 1) (region.end.column - 1)

          (EQ, GT) -> -- ...XXX
            sliceIt (region.start.column - 1) length

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

    literalAttrs litStyle =
      [ pinCall
      , onMouseEnter (MouseOverTrace trace)
      , onMouseLeave MouseOutTrace
      , style litStyle
      ]
  in
    case val of
      IntV int ->
        span
          (literalAttrs Style.intV)
          [ text (toString int) ]

      StringV str ->
        span
          (literalAttrs Style.stringV)
          [ text ("\"" ++ str ++ "\"") ] -- TODO escape?

      BoolV bool ->
        span
          (literalAttrs Style.boolV)
          [ text (toString bool) ]

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
        span
          (literalAttrs [])
          [ text "<Closure scope:{"
          , span []
              ( attrs.closureScope
                |> Dict.toList
                |> List.map
                    (\(name, tVal) ->
                      span []
                        [ text name
                        , text ": "
                        , viewValue overTrace tVal
                        ]
                    )
                |> List.intersperse (text ", ")
              )
          , text "}>"
          ]
