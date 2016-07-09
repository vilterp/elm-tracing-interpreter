module ASTDecoder exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App
import Json.Decode as JsDec

import Elm.AST exposing (..)
import Elm.Decode exposing (..)
import Utils


type alias Model =
  { text : String
  , decoded : Maybe (Result String (List (Module (List Def))))
  --, decoded : Maybe (Result String Int)
  }


type Msg
  = UpdateText String
  | Decode


update : Msg -> Model -> Model
update msg model =
  case msg of
    UpdateText txt ->
      { model | text = txt }

    Decode ->
      { model | decoded =
          JsDec.decodeString (JsDec.list Elm.Decode.decodeModuleOfDefs) model.text |> Just
          --Nothing
      }


view : Model -> Html Msg
view model =
  let
    result =
      model.decoded
      |> Maybe.map (Result.map toString >> Utils.getResult)
      |> Maybe.withDefault "Paste a JSON AST & hit 'Decode'"
  in
    div
      []
      [ textarea [ onInput UpdateText, rows 10, cols 50 ] [ text model.text ]
      , button [ onClick Decode ] [ text "Decode" ]
      , pre [ style [("white-space", "pre-wrap")] ] [ text result ]
      ]


main =
  App.beginnerProgram
    { model = { text = "", decoded = Nothing }
    , view = view
    , update = update
    }
