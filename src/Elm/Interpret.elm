module Elm.Interpret exposing (..)

import Dict exposing (Dict)

import Utils
import Elm.AST as AST exposing (..)
import Elm.Trace as Trace exposing (..)
import Model exposing (..)


type InterpError
  = NoMainYo


interpretMainYo : FuncDict -> Result InterpError (CallTree, TVal)
interpretMainYo funcDict =
  case Dict.get ("user/project", ["Main"], "mainYo") funcDict of
    Just (Def _ pattern expr _) ->
      let
        (tVal, finalState) =
          interpretExpr funcDict Dict.empty initialState expr

        (A region _) =
          expr

        rootCall =
          { func =
              ( ClosureV
                  { sourceRegion = region
                  , closureScope = Dict.empty
                  , lambda = { varName = "", expr = expr }
                  }
              , LiteralT -1 region -- behind the bubbles of spacetime...
              )
          , name = Just "mainYo"
          , args = []
          , result = tVal
          , subcalls = finalState.subcallsAtThisLevel
          , caller = Nothing
          }

        stateWithRootCall =
          { finalState | callTree =
              finalState.callTree
              |> Dict.insert 0 rootCall
          }
      in
        Ok (stateWithRootCall.callTree, tVal)

    Nothing ->
      Err NoMainYo


type alias InterpState =
  { currentCallId : CallId
  , callTree : CallTree
  , subcallsAtThisLevel : List (CallId, AST.Region)
  }


initialState : InterpState
initialState =
  { currentCallId = 0
  , callTree = Dict.empty
  , subcallsAtThisLevel = []
  }


-- whoooo giant case expr
interpretExpr : FuncDict -> Scope -> InterpState -> Expr -> (TVal, InterpState)
interpretExpr funcDict scope state locatedExpr =
  let
    --d = Debug.log "INTERPRETeXPR" (scope, expr)
    sameState tVal =
      (tVal, state)

    (A region expr) =
      locatedExpr

    interpretSubexpr : InterpState -> Expr -> (TVal, InterpState)
    interpretSubexpr stateAlready subExpr =
      interpretExpr funcDict scope stateAlready subExpr
  in
    case expr of
      AST.Literal literal ->
        ( case literal of
          IntNum x ->
            (IntV x, LiteralT state.currentCallId region)

          Str str ->
            (StringV str, LiteralT state.currentCallId region)

          Boolean b ->
            (BoolV b, LiteralT state.currentCallId region)

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

      AST.If condPairs ifFalseExpr ->
        case condPairs of
          [(condExpr, ifTrueExpr)] ->
            let
              (condValue, newState) =
                interpretSubexpr state condExpr

              mkResult chosenExpr =
                let
                  ((val, trace), newNewState) =
                    interpretSubexpr newState chosenExpr
                in
                  ( ( val
                    , IfT
                        { callId = state.currentCallId
                        , ifExpr = locatedExpr
                        , decidingValue = condValue
                        , innerTrace = trace
                        }
                    )
                  , newNewState
                  )
            in
              case condValue of
                (BoolV True, _) ->
                  mkResult ifTrueExpr

                (BoolV False, _) ->
                  mkResult ifFalseExpr

                _ ->
                  Debug.crash "cond value not true or false"

          _ ->
            Debug.crash "I thought multi-way ifs were no longer a thing"

      AST.App funExpr argExpr ->
        let
          (fun, newState) =
            interpretSubexpr state funExpr

          (arg, newNewState) =
            interpretSubexpr newState argExpr

          freshCallId =
            newNewState.currentCallId + 1

          d = 
            Debug.log "freshCallId" freshCallId
        in
          case fun of
            (ClosureV closureAttrs, closureTrace) ->
              let
                paramScope =
                  Dict.fromList [(closureAttrs.lambda.varName, arg)]

                totalScope =
                  Dict.union paramScope closureAttrs.closureScope

                ((result, innerTrace), newNewNewState) =
                  interpretExpr
                    funcDict
                    totalScope
                    { newNewState | currentCallId = freshCallId, subcallsAtThisLevel = [] }
                    closureAttrs.lambda.expr

                newCall =
                  { func = (ClosureV closureAttrs, closureTrace)
                  , name = Nothing
                  , args = [arg]
                  , result = (result, innerTrace)
                  , subcalls = newNewNewState.subcallsAtThisLevel
                  , caller = Just state.currentCallId
                  }

                -- make a new call, return it in subcalls
              in
                ( (result, FuncCallT freshCallId innerTrace)
                , { currentCallId = newNewNewState.currentCallId
                  , callTree =
                      newNewNewState.callTree
                      |> Dict.insert freshCallId newCall
                  , subcallsAtThisLevel =
                      newNewState.subcallsAtThisLevel ++ [(freshCallId, region)]
                  }
                )

            _ ->
              Debug.crash ("uknown function " ++ (toString fun))

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
        , LiteralT state.currentCallId region
        )
        |> sameState

      Binop { home, name } leftExpr rightExpr ->
        case home of
          ModuleHome { package, modul } ->
            case (package, modul) of
              ("elm-lang/core", ["Basics"]) ->
                let
                  (leftTVal, state') =
                    interpretExpr funcDict scope state leftExpr

                  (rightTVal, state'') =
                    interpretExpr funcDict scope state' rightExpr

                  resultVal =
                    case (name, fst leftTVal, fst rightTVal) of
                      ("*", IntV leftInt, IntV rightInt) ->
                        IntV (leftInt * rightInt)

                      ("+", IntV leftInt, IntV rightInt) ->
                        IntV (leftInt + rightInt)

                      ("-", IntV leftInt, IntV rightInt) ->
                        IntV (leftInt - rightInt)

                      ("==", leftVal, rightVal) ->
                        BoolV (leftVal == rightVal)

                      ("&&", BoolV leftB, BoolV rightB) ->
                        BoolV (leftB && rightB)

                      ("||", BoolV leftB, BoolV rightB) ->
                        BoolV (leftB || rightB)

                      _ ->
                        Debug.crash ("unknown binop " ++ toString (name, fst leftTVal, fst rightTVal))

                  freshCallId =
                    state''.currentCallId + 1

                  resultTVal =
                    (resultVal, FuncCallT freshCallId BuiltinT)

                  newCall =
                    { func = (BuiltinFun { home = home, name = name }, BuiltinT)
                    , name = Just name
                    , args = [leftTVal, rightTVal]
                    , result = (resultVal, BuiltinT)
                    , subcalls = []
                    , caller = Just state.currentCallId
                    }
                in
                  ( resultTVal
                  , { state''
                        | currentCallId = freshCallId
                        , subcallsAtThisLevel = state''.subcallsAtThisLevel ++ [(freshCallId, region)]
                        , callTree = state''.callTree |> Dict.insert freshCallId newCall
                    }
                  )

              _ ->
                Debug.crash ("unknown binop" ++ toString { home=home, name=name })

          _ ->
            Debug.crash ("unknown home: " ++ (toString home))

      _ ->
        Debug.crash "TODO"


getVarName : AST.Pattern' a b -> Maybe String
getVarName pattern =
  case pattern of
    VarPattern name ->
      Just name

    _ ->
      Nothing


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
