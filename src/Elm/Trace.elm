module Elm.Trace exposing (..)

import Dict exposing (Dict)

import Utils exposing (getMaybe)
import Elm.AST as AST


type alias FuncDict =
  Dict FuncIdent AST.Def


type alias Scope =
  Dict String TVal


type alias FuncIdent =
  (AST.PackageName, AST.ModuleName, String)


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
  | BoolV Bool
  | ADTV
      { constructorName : String
      , args : List TVal
      }
  | RecordV (Dict String TVal)
  | ClosureV
      { sourceRegion : AST.Region
      , closureScope : Scope
      , lambda :
          { varName : String
          , expr : AST.Expr
          }
      -- , TODO: name
      }


type Trace
  = FuncCall CallId Trace -- inner trace...
  | Literal CallId AST.Region -- the call in which the literal was used (?)
  -- maybe need Atom & Data?


type alias Call =
  { name : FuncName -- TODO: change to (ClosureV, Trace), so we can trace where this closure was defined whooooo!
  , args : List TVal
  , result : TVal
  , subcalls : List CallId -- TODO: change to `List (CallId, AST.Region)`
  , caller : Maybe (CallId, AST.Region) -- Nothing <=> this is main TODO: remove AST.Region
  }


type alias FuncName =
  String


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
