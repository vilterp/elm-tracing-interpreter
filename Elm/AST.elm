module Elm.AST exposing (..)

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
   { package: Name
   , modul: Raw
   }


-- user/project
type alias Name =
    String


-- raw module name (?)
type alias Raw =
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


type Expr' ann def var typ =
    Literal Literal
    | Var var
    | Range (Expr' ann def var typ) (Expr' ann def var typ)
    | ExplicitList (List (Expr' ann def var typ))
    | Binop var (Expr' ann def var typ) (Expr' ann def var typ)
    | Lambda (Pattern ann var) (Expr' ann def var typ)
    | App (Expr' ann def var typ) (Expr' ann def var typ)
    | If (List ((Expr' ann def var typ), (Expr' ann def var typ))) (Expr' ann def var typ)
    | Let (List def) (Expr' ann def var typ)
    | Case (Expr' ann def var typ) (List ((Pattern ann var), (Expr' ann def var typ)))
    | Data String (List (Expr' ann def var typ))
    | Access (Expr' ann def var typ) String
    | Update (Expr' ann def var typ) (List (String, (Expr' ann def var typ)))
    | Record (List (String, (Expr' ann def var typ)))
    | Cmd CanonicalModuleName
    | Sub CanonicalModuleName
    | OutgoingPort String typ
    | IncomingPort String typ
    | Program (Main typ) (Expr' ann def var typ)
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
