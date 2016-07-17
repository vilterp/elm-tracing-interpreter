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
  Dict CallId Call


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
  | ClosureV ClosureAttrs


type alias ClosureAttrs =
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
  -- TODO: case branch & if branch


type alias Call =
  { closure : (ClosureAttrs, Trace)
  , name : Maybe String -- TODO: could find name based on closure's lambda's expression...
  , args : List TVal
  , result : TVal
  , subcalls : List (CallId, AST.Region)
  , caller : Maybe CallId
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
          callTree
          |> Dict.get theCallId
          |> getMaybe ("no such call " ++ toString theCallId)

        rest =
          call.caller
          |> Maybe.map (go (Just theCallId))
          |> Maybe.withDefault []
      in
        { call = call, selectedSubcall = subcallId } :: rest
  in
    go Nothing callId
