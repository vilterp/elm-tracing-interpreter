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


interpretExpr : FuncDict -> Scope -> CallId -> Expr -> TVal
interpretExpr funcDict scope currentCallId (A region expr) =
  --let d = Debug.log "INTERPRETeXPR" (scope, expr) in
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

    AST.Var { home, name } ->
      case home of
        TopLevelHome moduleName ->
          let
            funcIdent =
              (moduleName.package, moduleName.modul, name)
          in
            funcDict
            |> Dict.get funcIdent
            |> Utils.getMaybe "func not found"
            |> (\(AST.Def _ _ expr _) -> expr)
            |> interpretExpr funcDict Dict.empty currentCallId

        Local ->
          scope
          |> Dict.get name
          |> Utils.getMaybe "not in scope"

        _ ->
          Debug.crash "TODO: scopes"

    AST.Let defs innerExpr ->
      let
        -- It seems the compiler sorts everything for us
        -- although it seems like Canonicalize.Sort is not getting called
        -- will have to fix that
        letScope =
          defs
          |> List.foldl
            (\(Def _ (AST.A _ pattern) defExpr _) scopeStep ->
              let d = Debug.log "inserting" (getVarName pattern |> Utils.getMaybe "not a var pattern") in
              scopeStep
              |> Dict.insert
                  (getVarName pattern |> Utils.getMaybe "not a var pattern")
                  (interpretExpr funcDict scopeStep currentCallId defExpr)
            )
            scope
      in
        interpretExpr funcDict letScope currentCallId innerExpr

    AST.If branches otherwiseExpr ->
      let
        tryBranches branches =
          case branches of
            (condExpr, bodyExpr)::rest ->
              let
                condValue =
                  interpretExpr funcDict scope currentCallId condExpr
              in
                case condValue of
                  (BoolV True, _) ->
                    interpretExpr funcDict scope currentCallId bodyExpr

                  (BoolV False, _) ->
                    tryBranches rest

                  _ ->
                    Debug.crash "cond value not true or false"

            [] ->
              interpretExpr funcDict scope currentCallId otherwiseExpr
      in
        tryBranches branches

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


getVarName : AST.Pattern' a b -> Maybe String
getVarName pattern =
  case pattern of
    VarPattern name ->
      Just name

    _ ->
      Nothing
