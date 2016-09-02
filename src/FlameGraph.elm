module FlameGraph exposing (..)

import Color
import Text as T
import Collage exposing (defaultLine)
import String
import Dict exposing (Dict)
import Html exposing (Html)

import Diagrams.Core as Diagrams exposing (..)
import Diagrams.Geom exposing (Point)
import Diagrams.Envelope as Envelope exposing (..)
import Diagrams.Debug exposing (..)
import Diagrams.Align exposing (..)
import Diagrams.FillStroke exposing (..)
import Diagrams.FullWindow exposing (..)
import Diagrams.Type exposing (..)
import Diagrams.Layout as Layout
import Diagrams.Pad exposing (..)
import Diagrams.Query as Query
import Diagrams.Svg

import Elm.Trace as Trace
import Viewer
import Utils


type CallTree
  = CallNode
      { name : String
      , id : Trace.CallId
      , args : List Trace.TVal
      , result : Trace.TVal
      , subcalls : List CallTree
      }


type Tag
  = ArgTag { callId : Trace.CallId, idx : Int }
  | ResultTag Trace.CallId
  | LiteralAreaTag Trace.CallId


-- TODO hover actions


boxHeight =
  20

spaceBetweenCalls =
  30


view : Trace.CallTree -> Html a
view traceCallTree =
  let
    callTree =
      traceCallTree
      |> fromTraceCallTree

    flameGraphDia =
      callTree
      |> flameGraph

    rootTrace =
      case callTree of
        CallNode attrs ->
          Trace.FuncCallT 0 (snd attrs.result)
          -- ... this is kinda weird ...

    traceDia =
      viewTrace traceCallTree flameGraphDia rootTrace

    fullDia =
      alignCenter (zcat [traceDia, flameGraphDia])
  in
    Diagrams.Svg.toHtml (Envelope.dims fullDia) fullDia


flameGraph : CallTree -> Diagram Tag a
flameGraph (CallNode node) =
  let
    callSpacer =
      hspace spaceBetweenCalls

    subcalls =
      hcatA BottomA ([callSpacer] ++ (List.intersperse callSpacer (List.map flameGraph node.subcalls)) ++ [callSpacer])
      |> alignCenter

    argsText =
      node.args
      |> List.map (fst >> Viewer.valueToString)

    argsDia =
      node.args
      |> List.indexedMap (\idx (arg, _) ->
        arg
        |> Viewer.valueToString
        |> text T.defaultStyle
        |> tag (ArgTag { callId = node.id, idx = idx })
      )
      |> List.intersperse (text T.defaultStyle ", ")
      |> hcat

    callTextDia =
      hcat
        [ text T.defaultStyle (node.name ++ "(")
        , argsDia
        , text T.defaultStyle ")"
        , hspace 5
        , tag (LiteralAreaTag node.id) empty
        , hspace 5
        ]

    resultDia =
      node.result
      |> fst
      |> Viewer.valueToString
      |> text T.defaultStyle
      |> tag (ResultTag node.id)

    callFlexRow =
      [ Layout.block callTextDia
      , Layout.spring
      , Layout.block (hcat [text T.defaultStyle " => ", resultDia])
      ]

    thisCall =
      Layout.layout
        [ [Layout.strut (width subcalls)]
        , callFlexRow
        ]
      |> alignCenter
      |> background (justSolidFill Color.orange)
  in
    subcalls `above` thisCall


traceColor =
  Color.red

traceLineWidth =
  3


viewTrace : Trace.CallTree -> Diagram Tag a -> Trace.Trace -> Diagram t a
viewTrace callTree flameGraphDia trace =
  case (Debug.log "VIEW_TRACE" trace) of
    Trace.FuncCallT callId innerTrace ->
      let
        args =
          callTree
          |> Dict.get callId
          |> Utils.getMaybe ("no such call: " ++ toString callId)
          |> .args
      in
        zcat
          [ circle 4 (justSolidFill traceColor)
            |> move (locForTrace trace flameGraphDia)
          , viewTrace callTree flameGraphDia innerTrace
          , path
              [ locForTrace trace flameGraphDia
              , locForTrace innerTrace flameGraphDia
              ]
              { defaultLine
                  | color = traceColor
                  , width = traceLineWidth
              }
          , args
            |> Debug.log "ARGS"
            |> List.map (snd >> viewTrace callTree flameGraphDia)
            |> zcat
          ]

    Trace.LiteralT callId _ ->
      rect 5 5 (justSolidFill traceColor)
      |> move (locForTrace trace flameGraphDia)

    Trace.IfT ifAttrs ->
      viewTrace callTree flameGraphDia ifAttrs.innerTrace

    Trace.CaseT caseAttrs ->
      viewTrace callTree flameGraphDia caseAttrs.innerTrace


locForTrace : Trace.Trace -> Diagram Tag a -> Point
locForTrace trace flameGraphDia =
  let
    locForTag tag =
      flameGraphDia
      |> Query.getCoords [tag]
      |> Utils.getMaybe ("tag not found:" ++ toString tag)
  in
    case trace of
      Trace.FuncCallT callId _ ->
        locForTag (ResultTag callId)

      Trace.LiteralT callId _ ->
        locForTag (LiteralAreaTag callId)

      Trace.IfT ifAttrs ->
        locForTrace ifAttrs.innerTrace flameGraphDia

      Trace.CaseT caseAttrs ->
        locForTrace caseAttrs.innerTrace flameGraphDia


fromTraceCallTree : Trace.CallTree -> CallTree
fromTraceCallTree trace =
  let
    recurse callId =
      trace
      |> Dict.get callId
      |> Utils.getMaybe ("no call with id" ++ toString callId)
      |> (\call ->
        CallNode
          { id = callId
          , name =
              call.name
              |> Maybe.withDefault (Viewer.valueToString (Trace.ClosureV (fst call.closure)))
          , args = call.args
          , result = call.result
          , subcalls = call.subcalls |> List.map (fst >> recurse)
          }
      )
  in
    Debug.log "fromTraceCallTree" (recurse 0)
