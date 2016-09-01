module FlameGraph exposing (..)

import Color
import Text as T
import String
import Dict exposing (Dict)
import Html exposing (Html)

import Diagrams.Core as Diagrams exposing (..)
import Diagrams.Envelope as Envelope exposing (..)
import Diagrams.Debug exposing (..)
import Diagrams.Align exposing (..)
import Diagrams.FillStroke exposing (..)
import Diagrams.FullWindow exposing (..)
import Diagrams.Type exposing (..)
import Diagrams.Layout as Layout
import Diagrams.Pad exposing (..)
import Diagrams.Svg

import Elm.Trace as Trace
import Viewer
import Utils


type CallTree
  = CallNode
      { name : String
      , args : List Trace.TVal
      , result : Trace.TVal
      , subcalls : List CallTree
      }


boxHeight =
  20


spaceBetweenCalls =
  30


view : Trace.CallTree -> Html a
view trace =
  let
    dia =
      trace
      |> fromTrace
      |> flameGraph
  in
    Diagrams.Svg.toHtml (Envelope.dims dia) (alignCenter dia)


flameGraph : CallTree -> Diagram t a
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

    callText =
      node.name ++ "(" ++ (String.join ", " argsText) ++ ")"

    callFlexRow =
      [ Layout.block (text T.defaultStyle callText)
      , Layout.spring
      , Layout.block (text T.defaultStyle (" => " ++ (node.result |> fst |> Viewer.valueToString)))
      ]

    thisCall =
      Layout.layout
        [ [Layout.strut (width subcalls)]
        , callFlexRow
        ]
      |> alignCenter
      |> background (justSolidFill Color.orange)
      --rect (width subcalls) boxHeight (justSolidFill Color.orange)
  in
    subcalls `above` thisCall


fromTrace : Trace.CallTree -> CallTree
fromTrace trace =
  let
    recurse callId =
      trace
      |> Dict.get callId
      |> Utils.getMaybe ("no call with id" ++ toString callId)
      |> (\call ->
        CallNode
          { name = "foo"
          , args = call.args
          , result = call.result
          , subcalls = call.subcalls |> List.map (fst >> recurse)
          }
      )
  in
    recurse 0
