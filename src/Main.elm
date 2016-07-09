module ASTDecoder exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App
import Json.Decode as JsDec
import Json.Encode as JsEnc
import Dict exposing (Dict)
import Task
import Http
import HttpBuilder exposing (..)

import Elm.AST exposing (..)
import Elm.Decode exposing (..)
import Elm.Interpret as Interpret exposing (FuncIdent)
import Utils


type alias Model =
  { text : String
  , ast : Loading (Error String) (Dict FuncIdent Def)
  }


type Loading a b
  = NotStarted
  | InProgress
  | Returned (Result a b)


type Msg
  = UpdateText String
  | Compile
  | CompileResponse (Result (Error String) (Dict FuncIdent Def))


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UpdateText txt ->
      { model | text = txt } ! []

    Compile ->
      let
        body =
          model.text
          |> codeToJsonPayload
          |> JsEnc.encode 0
          |> Http.string

        decode =
          Elm.Decode.decodeModuleOfDefs
          |> JsDec.list
          |> JsDec.map Interpret.buildFunctionDict
      in
        ( { model | ast = InProgress }
        , HttpBuilder.post "/compile_elm"
          |> withJsonBody (codeToJsonPayload model.text)
          |> withHeader "Content-Type" "application/json"
          |> send (jsonReader decode) stringReader
          |> Task.perform
            (\err -> CompileResponse (Err err))
            (\res -> CompileResponse (Ok res.data))
        )

    CompileResponse resp ->
      { model | ast = Returned resp } ! []


codeToJsonPayload : String -> JsEnc.Value
codeToJsonPayload code =
  JsEnc.object [("code", JsEnc.string code)]


view : Model -> Html Msg
view model =
  div
    []
    [ textarea [ onInput UpdateText, rows 10, cols 50 ] [ text model.text ]
    , button [ onClick Compile ] [ text "Compile" ]
    , case model.ast of
        NotStarted ->
          p [] [ text "Write Elm code & hit 'Compile'" ]

        InProgress ->
          p [] [ text "Compiling..." ]

        Returned result ->
          case result of
            Ok ast ->
              ast
              |> Dict.toList
              |> List.map (\item -> li [] [text (toString item)])
              |> ul [ style [("font-family", "monospace")] ]

            Err err ->
              text <| toString err
    ]


main =
  App.program
    { init = ({ text = "", ast = NotStarted }, Cmd.none)
    , view = view
    , update = update
    , subscriptions = always Sub.none
    }
