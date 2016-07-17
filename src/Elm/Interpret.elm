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


type alias InterpState =
  { currentCallId : CallId
  , callTree : CallTree
  , unlinked : List (CallId, AST.Region)
  }


initialState : InterpState
initialState =
  { currentCallId = 0
  , callTree = Dict.empty
  , unlinked = []
  }


addCall : CallId -> Region -> Call -> InterpState -> InterpState
addCall theCallId region call state =
  { state |
      callTree =
        state.callTree |> Dict.insert theCallId call,
      unlinked =
        (theCallId, region) :: state.unlinked
  }


interpretExpr : FuncDict -> Scope -> InterpState -> Expr -> (TVal, InterpState)
interpretExpr funcDict scope state (A region expr) =
  let
    --d = Debug.log "INTERPRETeXPR" (scope, expr)
    sameState tVal =
      (tVal, state)

    interpretSubexpr : InterpState -> Expr -> (TVal, InterpState)
    interpretSubexpr stateAlready subExpr =
      interpretExpr funcDict scope stateAlready subExpr
  in
    case expr of
      AST.Literal literal ->
        ( case literal of
          IntNum x ->
            (IntV x, Trace.Literal state.currentCallId region)

          Str str ->
            (StringV str, Trace.Literal state.currentCallId region)

          Boolean b ->
            (BoolV b, Trace.Literal state.currentCallId region)

          _ ->
            Debug.crash "TODO"
        )
        |> sameState

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
              |> interpretExpr funcDict Dict.empty state
              -- should this cound as a call? urgh

          Local ->
            scope
            |> Dict.get name
            |> Utils.getMaybe "not in scope"
            |> sameState

          _ ->
            Debug.crash "TODO: more homes"

      AST.Let defs innerExpr ->
        let
          -- It seems the compiler sorts everything for us
          -- although it seems like Canonicalize.Sort is not getting called
          -- will have to fix that
          saveDefinition : Def -> (Scope, InterpState) -> (Scope, InterpState)
          saveDefinition (Def _ (AST.A _ pattern) defExpr _) (scopeStep, stateAlready) =
            let
              (tVal, newState) =
                interpretExpr funcDict scopeStep stateAlready defExpr

              varName =
                getVarName pattern
                |> Utils.getMaybe "not a var pattern"

              newScope =
                scopeStep
                |> Dict.insert varName tVal
            in
              ( newScope, newState )

          (letScope, letState) =
            defs
            |> List.foldl saveDefinition (scope, state)

          (result, bodyState) =
            interpretExpr funcDict letScope letState innerExpr
        in
          (result, bodyState)

      AST.If branches otherwiseExpr ->
        let
          tryBranches : InterpState -> List (Expr, Expr) -> (TVal, InterpState)
          tryBranches stateAlready branches =
            case branches of
              (condExpr, bodyExpr)::rest ->
                let
                  (condValue, newState) =
                    interpretSubexpr stateAlready condExpr
                in
                  case condValue of
                    (BoolV True, _) ->
                      interpretSubexpr newState bodyExpr

                    (BoolV False, _) ->
                      tryBranches newState rest

                    _ ->
                      Debug.crash "cond value not true or false"

              [] ->
                interpretSubexpr stateAlready otherwiseExpr
        in
          tryBranches state branches

      AST.App funExpr argExpr ->
        let
          (fun, newState) =
            interpretSubexpr state funExpr

          (arg, newNewState) =
            interpretSubexpr newState argExpr

          freshCallId =
            state.currentCallId + 1

          d = 
            Debug.log "freshCallId" freshCallId
        in
          case fun of
            (ClosureV closureAttrs, _) ->
              let
                paramScope =
                  Dict.fromList [(closureAttrs.lambda.varName, arg)]

                totalScope =
                  Dict.union paramScope closureAttrs.closureScope

                ((result, innerTrace), newNewNewState) =
                  interpretExpr
                    funcDict
                    totalScope
                    { newNewState | currentCallId = freshCallId, unlinked = [] }
                    closureAttrs.lambda.expr

                newCall =
                  { name = "<lol dunno what name is>"
                  , args = [arg]
                  , result = (result, innerTrace)
                  , subcalls = newNewNewState.unlinked
                  , caller = Just state.currentCallId
                  }

                -- make a new call, return it in subcalls
              in
                ( (result, FuncCall freshCallId innerTrace)
                , newNewNewState
                  |> addCall freshCallId region newCall
                  |> Debug.log "newnewnewnewstate"
                )

            _ ->
              Debug.crash "app of a non-function"

      AST.Lambda (AST.A _ pattern) bodyExpr ->
        ( ClosureV
            { sourceRegion = region
            , closureScope = scope
            , lambda =
                { varName =
                    getVarName pattern |> Utils.getMaybe "not a var pattern"
                , expr = bodyExpr
                }
            }
        , Trace.Literal state.currentCallId region
        )
        |> sameState

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
