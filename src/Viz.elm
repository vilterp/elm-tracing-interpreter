module Viz exposing (..)

import Dict exposing (Dict)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App

import Elm.Trace exposing (..)
import Model exposing (..)
import Viewer exposing (..)
import Style
import Utils exposing (..)
--import ExampleData


update : Msg -> Model -> Model
update msg model =
  case msg of
    MouseOverTrace trace ->
      { model | overTrace = Just trace }

    MouseOutTrace ->
      { model | overTrace = Nothing }

    PinCall callId ->
      { model | pinnedCall = callId }

    RequestEdit ->
      model

    NoOp ->
      model


view : Model -> CallTree -> TVal -> Source -> FuncDict -> Html Msg
view model callTree tVal source funcDict =
  let
    maybeOverSpan =
      model.overTrace
      `Maybe.andThen` (\trace ->
        case trace of
          LiteralT callId sourceSpan ->
            Just (callId, sourceSpan)

          _ ->
            Nothing
      )
  in
    div [style [("display", "flex")]]
      [ div []
          [ div [] [ viewSource maybeOverSpan source ]
          , button [ onClick RequestEdit ] [ text "Edit" ]
          ]
      , div [] [ viewStack model callTree ]
      , text (toString (Dict.keys callTree))
      ]


viewStack : Model -> CallTree -> Html Msg
viewStack model callTree =
  model.pinnedCall
  |> stackForCall callTree
  |> List.map (\stackFrame ->
    li []
      [ stackFrame.call.name
        |> Maybe.withDefault ""
        |> \name -> strong [] [text name]
      , viewValue Nothing (ClosureV (fst stackFrame.call.closure), snd stackFrame.call.closure)
      , text ": ("
      , viewSubcallWidget (List.map fst stackFrame.call.subcalls) stackFrame.selectedSubcall
      , text ")"
      , viewStackFrame model.overTrace stackFrame
      ]
  )
  |> ul []


viewStackFrame : Maybe Trace -> StackFrame -> Html Msg
viewStackFrame overTrace stackFrame =
  let
    viewVal val =
      div [style Style.viewValue] [viewValue overTrace val]
  in
    ul []
      [ li []
          [ text "args:"
          , ul [] (stackFrame.call.args |> List.map (\argVal -> li [] [viewVal argVal]))
          ]
      , li []
          [ text "result:"
          , div [style Style.viewValue] [viewVal stackFrame.call.result]
          ]
      ]


viewSubcallWidget : List CallId -> Maybe CallId -> Html Msg
viewSubcallWidget subcallIds maybeSelectedSubcall =
  let
    viewSubcallMarker subcallId =
      let
        theStyle =
          if (Just subcallId) == maybeSelectedSubcall then
            Style.selectedSubcall
          else
            Style.subcall
      in
        span
          [ style theStyle
          , onClick (PinCall subcallId)
          ]
          [ text "X" ]
  in
    subcallIds
    |> List.map viewSubcallMarker
    |> List.intersperse (text " ")
    |> span []


--main =
--  App.beginnerProgram
--    { model =
--        initialModel ExampleData.callTree ExampleData.funcDefinitionSpans ExampleData.source
--    , view = view
--    , update = update
--    }
