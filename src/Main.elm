module ASTDecoder exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App
import Json.Decode as JsDec
import Json.Encode as JsEnc
import Task
import Http
import HttpBuilder exposing (..)

import Elm.AST exposing (..)
import Elm.Decode exposing (..)
import Utils


type alias ModuleDefs =
  List (Module (List Def))


type alias Model =
  { text : String
  , ast : Loading (Error String) ModuleDefs
  }


type Loading a b
  = NotStarted
  | InProgress
  | Returned (Result a b)


type Msg
  = UpdateText String
  | Compile
  | CompileResponse (Result (Error String) ModuleDefs)


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
      in
        ( { model | ast = InProgress }
        , HttpBuilder.post "/compile_elm"
          |> withJsonBody (codeToJsonPayload model.text)
          |> withHeader "Content-Type" "application/json"
          |> send (jsonReader (JsDec.list Elm.Decode.decodeModuleOfDefs)) stringReader
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
    , pre [ style [("white-space", "pre-wrap")] ]
        [ case model.ast of
            NotStarted ->
              text "Write Elm code & hit 'Compile'"

            InProgress ->
              text "Compiling..."

            Returned result ->
              case result of
                Ok ast ->
                  text <| toString ast

                Err err ->
                  text <| toString err
        ]
    ]


main =
  App.program
    { init = ({ text = "", ast = NotStarted }, Cmd.none)
    , view = view
    , update = update
    , subscriptions = always Sub.none
    }
