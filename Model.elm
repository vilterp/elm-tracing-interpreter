module Model exposing (..)
-- where

import Dict exposing (Dict)
import Utils exposing (..)


type Msg
  = PinCall CallId
  | MouseOverTrace Trace
  | MouseOutTrace
  | NoOp


type alias Model =
  { callTree : CallTree
  , funcDefinitionSpans : FuncDefinitionSpans
  , source : Source
  , pinnedCall : CallId
  , overTrace : Maybe Trace
  }


initialModel : CallTree -> FuncDefinitionSpans -> Source -> Model
initialModel callTree funcDefinitionSpans source =
  { callTree = callTree
  , funcDefinitionSpans = funcDefinitionSpans
  , source = source
  , pinnedCall = callTree.root
  --, pinnedCall = 6
  , overTrace = Nothing
  }


type alias FuncDefinitionSpans =
  Dict FuncName SourceSpan


type alias CallTree =
  { calls : Dict CallId Call
  , root : CallId
  }


type alias CallId =
  Int


type alias TVal =
  (Val, Trace)


type Val
  = IntV Int
  | StringV String
  | ADTV
      { constructorName : String
      , args : List TVal
      }
  | RecordV (Dict String TVal)
  | ClosureV
      { name : Maybe String
      , definition : SourceSpan
      , closure : Dict String TVal
      }


type Trace
  = FuncCall CallId
  | Literal SourceSpan


type alias Call =
  { name : FuncName
  , args : List TVal
  , result : TVal
  , calls : List CallId
  , caller : Maybe (CallId, SourceSpan) -- Nothing <=> this is main
  }


type alias FuncName =
  String


type alias SourceSpan =
  { start : SourceLoc
  , end : SourceLoc
  }


type alias SourceLoc =
  { line : Int
  , col : Int
  }


type alias Source =
  List String


type alias StackFrame =
  { call : CallId
  --, valuePath : Maybe ValuePath
  }


type ValuePath
  = ConstructorArg Int
  | RecordField String
  | ListItem Int


-- most recent call last
stackForCall : CallTree -> CallId -> List Call
stackForCall callTree callId =
  let
    call =
      callTree.calls
      |> Dict.get callId
      |> getMaybe ("no such call " ++ toString callId)

    rest =
      call.caller
      |> Maybe.map (fst >> stackForCall callTree)
      |> Maybe.withDefault []
  in
    call :: rest
