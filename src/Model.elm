module Model exposing (..)

import Dict exposing (Dict)

import Utils exposing (..)
import Elm.AST as AST
import Elm.Trace exposing (..)


type Msg
  = PinCall CallId
  | MouseOverTrace Trace
  | MouseOutTrace
  | NoOp
  | RequestEdit


type alias Model =
  { pinnedCall : CallId
  , overTrace : Maybe Trace
  }


initialModel : Model
initialModel =
  { overTrace = Nothing
  , pinnedCall = 0
  }
