module Elm.AST exposing (..)


type alias ModuleDefs =
   List (Module (List Def))


-- shit, maybe I'll have to send the whole source code of
-- core across.
type alias FilePath =
   String


type alias Module phase =
   { name: CanonicalModuleName
   , path: FilePath
   , info: phase
   }


-- canonical module name
type alias CanonicalModuleName  =
   { package: PackageName
   , modul: ModuleName
   }


-- user/project
type alias PackageName =
    String


-- raw module name (?)
type alias ModuleName =
    List String


-- def need this (ha)
type Def =
   -- canonical pattern, then canonical type
   Def Facts CanonicalPattern Expr (Maybe (Located CanonicalType))


type alias Facts  =
   { dependencies: (List TopLevel)
   }


-- top level var
type alias TopLevel  =
   { topHome: CanonicalModuleName
   , topName: String
   }


-- var

type alias CanonicalVar =
   { home: Home
   , name: String
   }


type Home  =
   BuiltIn 
   | ModuleHome CanonicalModuleName
   | TopLevelHome CanonicalModuleName
   | Local 


-- expr


type alias Expr =
  Annotated Region (Expr' Region () CanonicalVar CanonicalType)


type alias ParamExpr ann def var typ =
  Annotated ann (Expr' ann def var typ)


type Expr' ann def var typ =
    Literal Literal
    | Var var
    | Range (ParamExpr ann def var typ) (ParamExpr ann def var typ)
    | ExplicitList (List (ParamExpr ann def var typ))
    | Binop var (ParamExpr ann def var typ) (ParamExpr ann def var typ)
    | Lambda (Pattern ann var) (ParamExpr ann def var typ)
    | App (ParamExpr ann def var typ) (ParamExpr ann def var typ)
    | If (List ((ParamExpr ann def var typ), (ParamExpr ann def var typ))) (ParamExpr ann def var typ)
    | Let (List def) (ParamExpr ann def var typ)
    | Case (ParamExpr ann def var typ) (List ((Pattern ann var), (ParamExpr ann def var typ)))
    | Data String (List (ParamExpr ann def var typ))
    | Access (ParamExpr ann def var typ) String
    | Update (ParamExpr ann def var typ) (List (String, (ParamExpr ann def var typ)))
    | Record (List (String, (ParamExpr ann def var typ)))
    | Cmd CanonicalModuleName
    | Sub CanonicalModuleName
    | OutgoingPort String typ
    | IncomingPort String typ
    | Program (Main typ) (ParamExpr ann def var typ)
    --| SaveEnv Canonical Canonical
    --| GLShader String String GLShaderTipe


type Main typ =
    VDom 
    | NoFlags 
    | Flags typ


type Aliased t =
    Holey t
    | Filled t


-- Pattern

type alias CanonicalPattern =
   Pattern Region CanonicalVar


type alias Pattern ann var =
   Annotated ann (Pattern' ann var)


type Pattern' ann var =
    DataPattern var (List (Pattern ann var))
    | RecordPattern (List String)
    | Alias String (Pattern ann var)
    | VarPattern String
    | Anything 
    | LiteralPattern Literal


-- literal

type Literal  =
    IntNum Int
    | FloatNum Float
    | Chr Char
    | Str String
    | Boolean Bool


-- type

type CanonicalType  =
    LambdaType CanonicalType CanonicalType
    | VarType String
    | Type CanonicalType
    | AppType CanonicalType (List CanonicalType)
    | RecordType (List (String, CanonicalType)) (Maybe CanonicalType)
    | Aliased CanonicalType (List (String, CanonicalType)) (Aliased CanonicalType)


-- reporting stuff

type alias Located a =
   Annotated Region a


type Annotated annotation a =
  A annotation a


type alias Region  =
   { start: Position
   , end: Position
   }


type alias Position  =
   { line: Int
   , column: Int
   }
