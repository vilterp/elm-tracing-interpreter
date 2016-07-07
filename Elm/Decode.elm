module Elm.Decode exposing (..)

import Json.Decode as JsDec
import Json.Decode exposing ((:=))
import Json.Encode exposing (Value)
-- The following module comes from bartavelle/json-helpers
import Json.Helpers exposing (..)
import Dict
import Set
import String

import Elm.AST exposing (..)


decodeModuleOfDefs : JsDec.Decoder (Module (List Def))
decodeModuleOfDefs =
   jsonDecModule (JsDec.list jsonDecDef)
   --jsonDecModule (jsonDecRaw)


jsonDecFilePath =
   JsDec.string


jsonDecModule : JsDec.Decoder phase -> JsDec.Decoder ( Module phase )
jsonDecModule localDecoder_phase =
   ("name" := jsonDecCanonicalModuleName) `JsDec.andThen` \pname ->
   ("path" := jsonDecFilePath) `JsDec.andThen` \ppath ->
   ("info" := localDecoder_phase) `JsDec.andThen` \pinfo ->
   JsDec.succeed {name = pname, path = ppath, info = pinfo}


jsonDecCanonicalModuleName : JsDec.Decoder ( CanonicalModuleName )
jsonDecCanonicalModuleName =
   ("_package" := jsonDecName) `JsDec.andThen` \p_package ->
   ("_module" := jsonDecRaw) `JsDec.andThen` \p_module ->
   JsDec.succeed {package = p_package, modul = p_module}


jsonDecName =
   JsDec.string


jsonDecRaw =
   JsDec.list JsDec.string


jsonDecDef : JsDec.Decoder ( Def )
jsonDecDef =
    JsDec.tuple4 Def (jsonDecFacts) jsonDecCanonicalPattern jsonDecExpr (JsDec.maybe (jsonDecLocated (jsonDecCanonicalType)))


jsonDecFacts : JsDec.Decoder ( Facts )
jsonDecFacts =
   ("dependencies" := JsDec.list (jsonDecTopLevel)) `JsDec.andThen` \pdependencies ->
   JsDec.succeed {dependencies = pdependencies}


jsonDecTopLevel : JsDec.Decoder ( TopLevel )
jsonDecTopLevel =
   ("topHome" := jsonDecCanonicalModuleName) `JsDec.andThen` \ptopHome ->
   ("topName" := JsDec.string) `JsDec.andThen` \ptopName ->
   JsDec.succeed {topHome = ptopHome, topName = ptopName}


jsonDecCanonicalVar : JsDec.Decoder ( CanonicalVar )
jsonDecCanonicalVar =
   ("home" := jsonDecHome) `JsDec.andThen` \phome ->
   ("name" := JsDec.string) `JsDec.andThen` \pname ->
   JsDec.succeed {home = phome, name = pname}


jsonDecHome : JsDec.Decoder ( Home )
jsonDecHome =
   let jsonDecDictHome = Dict.fromList
            [ ("BuiltIn", JsDec.succeed BuiltIn)
            , ("Module", JsDec.map ModuleHome (jsonDecCanonicalModuleName))
            , ("TopLevel", JsDec.map TopLevelHome (jsonDecCanonicalModuleName))
            , ("Local", JsDec.succeed Local)
            ]
   in  decodeSumObjectWithSingleField  "Home" jsonDecDictHome


jsonDecExpr : JsDec.Decoder Expr
jsonDecExpr =
   jsonDecAnnotated jsonDecRegion (jsonDecExpr' jsonDecRegion (JsDec.succeed ()) jsonDecCanonicalVar jsonDecCanonicalType)


region =
   {
      start = {
          line = 3,
          column = 5
      },
      end = {
          line = 3,
          column = 6
      }
   }


jsonDecParamExpr  : JsDec.Decoder ann -> JsDec.Decoder def -> JsDec.Decoder var -> JsDec.Decoder typ -> JsDec.Decoder ( ParamExpr ann def var typ )
jsonDecParamExpr localDecoder_ann localDecoder_def localDecoder_var localDecoder_typ =
  jsonDecAnnotated localDecoder_ann (jsonDecExpr' localDecoder_ann localDecoder_def localDecoder_var localDecoder_typ)


jsonDecExpr' : JsDec.Decoder ann -> JsDec.Decoder def -> JsDec.Decoder var -> JsDec.Decoder typ -> JsDec.Decoder ( Expr' ann def var typ )
jsonDecExpr' localDecoder_ann localDecoder_def localDecoder_var localDecoder_typ =
   let decExpr =
         lazy (\_ -> jsonDecParamExpr localDecoder_ann localDecoder_def localDecoder_var localDecoder_typ)
       decPattern =
         jsonDecPattern localDecoder_ann localDecoder_var
       jsonDecDictExpr' = Dict.fromList
           [
             ("Literal", JsDec.map Literal (jsonDecLiteral))
           , ("Var", JsDec.map Var (localDecoder_var))
           , ("Range", JsDec.tuple2 Range decExpr decExpr)
           , ("ExplicitList", JsDec.map ExplicitList (JsDec.list decExpr))
           , ("Binop", JsDec.tuple3 Binop (localDecoder_var) decExpr decExpr)
           , ("Lambda", JsDec.tuple2 Lambda decPattern decExpr)
           , ("App", JsDec.tuple2 App decExpr decExpr)
           , ("If", JsDec.tuple2 If (JsDec.list (JsDec.tuple2 (,) decExpr decExpr)) decExpr)
           , ("Let", JsDec.tuple2 Let (JsDec.list (localDecoder_def)) decExpr)
           , ("Case", JsDec.tuple2 Case decExpr (JsDec.list (JsDec.tuple2 (,) decPattern decExpr)))
           , ("Data", JsDec.tuple2 Data (JsDec.string) (JsDec.list decExpr))
           , ("Access", JsDec.tuple2 Access decExpr (JsDec.string))
           , ("Update", JsDec.tuple2 Update decExpr (JsDec.list (JsDec.tuple2 (,) (JsDec.string) decExpr)))
           , ("Record", JsDec.map Record (JsDec.list (JsDec.tuple2 (,) (JsDec.string) decExpr)))
           , ("Cmd", JsDec.map Cmd (jsonDecCanonicalModuleName))
           , ("Sub", JsDec.map Sub (jsonDecCanonicalModuleName))
           , ("OutgoingPort", JsDec.tuple2 OutgoingPort (JsDec.string) (localDecoder_typ))
           , ("IncomingPort", JsDec.tuple2 IncomingPort (JsDec.string) (localDecoder_typ))
           , ("Program", JsDec.tuple2 Program (jsonDecMain (localDecoder_typ)) decExpr)
           --, ("SaveEnv", JsDec.tuple2 SaveEnv (jsonDecCanonical) (jsonDecCanonical))
           --, ("GLShader", JsDec.tuple3 GLShader (JsDec.string) (JsDec.string) (jsonDecGLShaderTipe))
           ]
   in
      decodeSumObjectWithSingleField "Expr'" jsonDecDictExpr'


lazy : (() -> JsDec.Decoder a) -> JsDec.Decoder a
lazy thunk =
  JsDec.customDecoder JsDec.value
      (\js -> JsDec.decodeValue (thunk ()) js)


jsonDecMain : JsDec.Decoder typ -> JsDec.Decoder ( Main typ )
jsonDecMain localDecoder_typ =
    let jsonDecDictMain = Dict.fromList
            [ ("VDom", JsDec.succeed VDom)
            , ("NoFlags", JsDec.succeed NoFlags)
            , ("Flags", JsDec.map Flags (localDecoder_typ))
            ]
    in  decodeSumObjectWithSingleField  "Main" jsonDecDictMain


jsonDecAliased : JsDec.Decoder t -> JsDec.Decoder ( Aliased t )
jsonDecAliased localDecoder_t =
    let jsonDecDictAliased = Dict.fromList
            [ ("Holey", JsDec.map Holey (localDecoder_t))
            , ("Filled", JsDec.map Filled (localDecoder_t))
            ]
    in  decodeSumObjectWithSingleField  "Aliased" jsonDecDictAliased


jsonDecCanonicalPattern : JsDec.Decoder CanonicalPattern
jsonDecCanonicalPattern =
   jsonDecPattern jsonDecRegion jsonDecCanonicalVar


jsonDecPattern : JsDec.Decoder ann -> JsDec.Decoder var -> JsDec.Decoder (Pattern ann var)
jsonDecPattern decAnn decVar =
   jsonDecAnnotated decAnn (jsonDecPattern' decAnn decVar)


jsonDecPattern' : JsDec.Decoder ann -> JsDec.Decoder var -> JsDec.Decoder ( Pattern' ann var )
jsonDecPattern' localDecoder_ann localDecoder_var =
    let
        decPattern =
          lazy (\_ -> jsonDecPattern localDecoder_ann localDecoder_var)
        jsonDecDictPattern' = Dict.fromList
            [ ("Data", JsDec.tuple2 DataPattern (localDecoder_var) (JsDec.list decPattern))
            , ("Record", JsDec.map RecordPattern (JsDec.list (JsDec.string)))
            , ("Alias", JsDec.tuple2 Alias (JsDec.string) decPattern)
            , ("Var", JsDec.map VarPattern (JsDec.string))
            , ("Anything", JsDec.succeed Anything)
            , ("Literal", JsDec.map LiteralPattern (jsonDecLiteral))
            ]
    in
      --decodeSumTaggedObject "Pattern'" "tag" "contents" jsonDecDictPattern' Set.empty
      decodeSumObjectWithSingleField "Pattern'" jsonDecDictPattern'


jsonDecLiteral : JsDec.Decoder ( Literal )
jsonDecLiteral =
    let jsonDecDictLiteral = Dict.fromList
            [ ("IntNum", JsDec.map IntNum (JsDec.int))
            , ("FloatNum", JsDec.map FloatNum (JsDec.float))
            , ("Chr", JsDec.map Chr jsonDecChar)
            , ("Str", JsDec.map Str (JsDec.string))
            , ("Boolean", JsDec.map Boolean (JsDec.bool))
            ]
    in  decodeSumObjectWithSingleField  "Literal" jsonDecDictLiteral


jsonDecChar : JsDec.Decoder Char
jsonDecChar =
   JsDec.string
   `JsDec.andThen` (\str ->
      case String.uncons str of
         Just (chr, "") ->
            JsDec.succeed chr

         _ ->
            JsDec.fail "didn't see a char"
   )


jsonDecCanonicalType : JsDec.Decoder ( CanonicalType )
jsonDecCanonicalType =
    let jsonDecDictCanonical = Dict.fromList
            [ ("Lambda", JsDec.tuple2 LambdaType (jsonDecCanonicalType) (jsonDecCanonicalType))
            , ("Var", JsDec.map VarType (JsDec.string))
            , ("Type", JsDec.map Type (jsonDecCanonicalType))
            , ("App", JsDec.tuple2 AppType (jsonDecCanonicalType) (JsDec.list (jsonDecCanonicalType)))
            , ("Record", JsDec.tuple2 RecordType (JsDec.list (JsDec.tuple2 (,) (JsDec.string) (jsonDecCanonicalType))) (JsDec.maybe (jsonDecCanonicalType)))
            , ("Aliased", JsDec.tuple3 Aliased (jsonDecCanonicalType) (JsDec.list (JsDec.tuple2 (,) (JsDec.string) (jsonDecCanonicalType))) (jsonDecAliased (jsonDecCanonicalType)))
            ]
    in  decodeSumObjectWithSingleField  "Canonical" jsonDecDictCanonical


jsonDecLocated : JsDec.Decoder a -> JsDec.Decoder ( Located a )
jsonDecLocated decA =
    jsonDecAnnotated jsonDecRegion decA


jsonDecAnnotated : JsDec.Decoder annotation -> JsDec.Decoder a -> JsDec.Decoder ( Annotated annotation a )
jsonDecAnnotated localDecoder_annotation localDecoder_a =
    JsDec.tuple2 A (localDecoder_annotation) (localDecoder_a)


jsonDecRegion : JsDec.Decoder ( Region )
jsonDecRegion =
   ("start" := jsonDecPosition) `JsDec.andThen` \pstart ->
   ("end" := jsonDecPosition) `JsDec.andThen` \pend ->
   JsDec.succeed {start = pstart, end = pend}


jsonDecPosition : JsDec.Decoder ( Position )
jsonDecPosition =
   ("line" := JsDec.int) `JsDec.andThen` \pline ->
   ("column" := JsDec.int) `JsDec.andThen` \pcolumn ->
   JsDec.succeed {line = pline, column = pcolumn}
