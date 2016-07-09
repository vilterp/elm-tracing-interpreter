module Elm.Interpret exposing (..)

import Dict exposing (Dict)

import Utils
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


interpretExpr : FuncDict -> CallId -> Expr -> TVal
interpretExpr funcDict currentCallId (A region expr) =
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

    --AST.Binop op leftExpr rightExpr ->
    --  -- TODO: fake regions
    --  interpretExpr funcDict 0 (App (App op leftExpr) rightExpr)

    --  -- TODO: move to app
    --  case home of
    --    ModuleHome { package, modul } ->
    --      let
    --        funcIdent =
    --          (package, modul, name)

    --        func =
    --          funcDict
    --          |> Dict.get funcIdent
    --          |> Utils.getMaybe "couldn't find function"
    --      in
    --        Debug.crash (toString func)

    --    _ ->
    --      Debug.crash "TODO"

    _ ->
      Debug.crash "TODO"
