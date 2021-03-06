module Main exposing (..)

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
import String

import Elm.AST exposing (..)
import Elm.Decode exposing (..)
import Elm.Interpret as Interpret
import Elm.Trace exposing (..)
import Model exposing (..)
import Viz
import ViewCompileErrors
import Utils


type alias Model =
  { code : String
  , result : Loading String (Error String) ResultModel
  }


type alias ResultModel =
  { funcDict : FuncDict
  , source : Elm.Trace.Source
  , interpResult : Result Interpret.InterpError (CallTree, TVal)
  , vizModel : Model.Model
  }


type Loading r a b
  = NotStarted
  | InProgress r
  | Returned (Result a b)


type Msg
  = UpdateText String
  | Compile
  | CompileResponse (Result (Error String) FuncDict)
  | VizMsg Model.Msg


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UpdateText txt ->
      { model | code = txt } ! []

    Compile ->
      let
        body =
          model.code
          |> codeToJsonPayload
          |> JsEnc.encode 0
          |> Http.string

        decode =
          Elm.Decode.decodeModuleOfDefs
          |> JsDec.list
          |> JsDec.map Interpret.buildFunctionDict
      in
        ( { model | result = InProgress model.code }
        , HttpBuilder.post "/compile_elm"
          |> withJsonBody (codeToJsonPayload model.code)
          |> withHeader "Content-Type" "application/json"
          |> send (jsonReader decode) stringReader
          |> Task.perform
            (\err -> CompileResponse (Err err))
            (\res -> CompileResponse (Ok res.data))
        )

    CompileResponse resp ->
      case resp of
        Err err ->
          { model | result = Returned (Err err) } ! []

        Ok funcDict ->
          let
            code =
              case model.result of
                InProgress c ->
                  c

                _ ->
                  Debug.crash "should be InProgress"
          in
            { model | result =
                { funcDict = funcDict
                , source = String.split "\n" code
                , interpResult = Interpret.interpretMainYo funcDict
                , vizModel = Model.initialModel
                }
                |> Ok
                |> Returned
            } ! []

    VizMsg msg ->
      case msg of
        RequestEdit ->
          { model | result = NotStarted } ! []

        _ ->
          case model.result of
            Returned res ->
                case res of
                  Ok resultModel ->
                    let
                      newVizModel = 
                        Viz.update msg resultModel.vizModel
                    in
                      { model | result =
                        { resultModel | vizModel = Debug.log "newVizModel" newVizModel }
                        |> Ok
                        |> Returned
                      } ! []

                  _ ->
                    model ! []

            _ ->
              model ! []


codeToJsonPayload : String -> JsEnc.Value
codeToJsonPayload code =
  JsEnc.object [("code", JsEnc.string code)]


view : Model -> Html Msg
view model =
  let
    editor =
      textarea
        [ onInput UpdateText
        , rows 10
        , cols 50
        , style [("font-family", "monospace")]
        ]
        [ text model.code ]

    compileButton =
      button [ onClick Compile ] [ text "Compile & Run" ]
  in
    case model.result of
      NotStarted ->
        div []
          [ editor
          , compileButton
          , p [] [ text "Write Elm code & hit 'Compile'" ]
          ]

      InProgress _ ->
        div []
          [ editor
          , compileButton
          , p [] [ text "Compiling..." ]
          ]

      Returned result ->
        case result of
          Ok { funcDict, source, interpResult, vizModel } ->
            let
              astView =
                funcDict
                |> Dict.toList
                |> List.reverse
                |> List.map (\item -> li [] [text (toString item)])
                |> ul [ style [("font-family", "monospace")] ]
            in
              case interpResult of
                Ok (callTree, tVal) ->                  
                  div []
                    [ App.map
                        VizMsg
                        (Viz.view vizModel callTree tVal source funcDict)
                    --, astView
                    ]

                Err interpErr ->
                  div []
                    [ editor
                    , compileButton
                    , p []
                        [ text "Error during interpretation: "
                        , text <| toString interpErr
                        ]
                    ]

          Err err ->
            div []
              [ editor
              , compileButton
              , case err of
                  BadResponse resp ->
                    case resp.status of
                      400 ->
                        ViewCompileErrors.view resp.data

                      _ ->
                        p []
                          [ text "Bad Response:"
                          , text (toString resp)
                          ]

                  _ ->
                    p [] [ text (toString err) ]
              ]


initialModel =
  { code =
      """fac n =
  if n == 1 then
    1
  else
    n * (fac (n - 1))

mainYo =
  fac 3
"""
  , result = NotStarted
  }


loggingUpdater : (msg -> model -> (model, Cmd msg)) -> msg -> model -> (model, Cmd msg)
loggingUpdater updater =
  \msg model ->
    let
      after = updater msg model
      d = Debug.log "AFTER:" after
      c = Debug.log "MSG:" msg
      b = Debug.log "BEFORE:" model
      a = Debug.log "=====================" ()
    in
      after


main =
  App.program
    { init = (initialModel, Cmd.none)
    , view = view
    --, update = loggingUpdater update
    , update = update
    , subscriptions = always Sub.none
    }
