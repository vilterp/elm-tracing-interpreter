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
  | Literal CallId SourceSpan -- the call in which the literal was used (?)
  -- maybe need Atom & Data?


type alias Call =
  { name : FuncName -- TODO: change to (ClosureV, Trace), so we can trace where this closure was defined whooooo!
  , args : List TVal
  , result : TVal
  , subcalls : List CallId -- TODO: change to `List (CallId, SourceSpan)`
  , caller : Maybe (CallId, SourceSpan) -- Nothing <=> this is main TODO: remove SourceSpan
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
  { call : Call
  , selectedSubcall : Maybe CallId
  --, valuePath : Maybe ValuePath
  }


type ValuePath
  = ConstructorArg Int
  | RecordField String
  | ListItem Int


-- most recent call last
stackForCall : CallTree -> CallId -> List StackFrame
stackForCall callTree callId =
  let
    go subcallId theCallId =
      let
        call =
          callTree.calls
          |> Dict.get theCallId
          |> getMaybe ("no such call " ++ toString theCallId)

        rest =
          call.caller
          |> Maybe.map (fst >> go (Just theCallId))
          |> Maybe.withDefault []
      in
        { call = call, selectedSubcall = subcallId } :: rest
  in
    go Nothing callId
