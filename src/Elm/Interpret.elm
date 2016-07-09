module Elm.Interpret exposing (..)

import Dict exposing (Dict)

import Elm.AST as AST exposing (..)
import Elm.Trace as Trace exposing (..)
import Model exposing (..)


buildFunctionDict : ModuleDefs -> Dict FuncIdent Def
buildFunctionDict modules =
  let
    getModuleDefs : Module (List Def) -> List (FuncIdent, Def)
    getModuleDefs { name, info } =
      info
      |> List.map (\def ->
        let
          funcName =
            case def of
              Def _ (A _ (VarPattern name)) _ _ ->
                name

              _ ->
                Debug.crash "couldn't find function name"

          funcIdent =
            (name.package, name.modul, funcName)
        in
          (funcIdent, def)
      )
  in
    modules
    |> List.map getModuleDefs
    |> List.concat
    |> Dict.fromList


interpretExpr : CallId -> Expr -> TVal
interpretExpr currentCallId (A region expr) =
  case expr of
    AST.Literal literal ->
      case literal of
        IntNum x ->
          (IntV x, Trace.Literal currentCallId region)

        Str str ->
          (StringV str, Trace.Literal currentCallId region)

        Boolean b ->
          (BoolV b, Trace.Literal currentCallId region)

        _ ->
          Debug.crash "TODO"

    _ ->
      Debug.crash "TODO"
