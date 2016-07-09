module Elm.Interpret exposing (..)

import Dict exposing (Dict)

import Elm.AST exposing (..)


type alias FuncIdent =
  (PackageName, ModuleName, String)


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
