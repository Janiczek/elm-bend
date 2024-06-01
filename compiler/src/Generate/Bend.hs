{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Generate.Bend
  ( generate,
  )
where

import qualified AST.Optimized as Opt
import qualified Data.ByteString.Builder as B
import qualified Data.List as List
import Data.Map ((!))
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Name (Name)
import qualified Data.Name as Name
import qualified Data.Set as Set
import qualified Data.Utf8 as Utf8
import Debug.Trace
import qualified Elm.Float as Float
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg

-- GENERATE

type Graph = Map.Map Opt.Global Opt.Node

type Mains = Map.Map ModuleName.Canonical Opt.Main

generate :: Opt.GlobalGraph -> Mains -> B.Builder
generate (Opt.GlobalGraph graph _) mains =
  let !_ = Debug.Trace.trace (show graph) ()
      state = Map.foldrWithKey (addMain graph) emptyState mains
   in stateToBuilder state
        <> "\n\n# wassup!"

addMain :: Graph -> ModuleName.Canonical -> Opt.Main -> State -> State
addMain graph home _ state =
  addGlobal graph state (Opt.Global home "main")

-- GRAPH TRAVERSAL STATE

data State = State
  { _revBuilders :: [B.Builder],
    _seenGlobals :: Set.Set Opt.Global
  }

emptyState :: State
emptyState =
  State
    initBuilders
    Set.empty

stateToBuilder :: State -> B.Builder
stateToBuilder (State revBuilders _) =
  prependBuilders revBuilders mempty

prependBuilders :: [B.Builder] -> B.Builder -> B.Builder
prependBuilders revBuilders monolith =
  List.foldl' (\m b -> b <> m) monolith revBuilders

initBuilders :: [B.Builder]
initBuilders =
  tupleGetters

tupleGetters :: [B.Builder]
tupleGetters =
  -- TODO newlines?
  -- TODO munging rules ... //, / etc. instead of $
  [ -- tuples
    "_Elm.GetTuple.el0 (a,*) = a",
    "_Elm.GetTuple.el1 (*,b) = b",
    -- triples
    "_Elm.GetTriple.el0 (a,*)     = a",
    "_Elm.GetTriple.el1 (*,(b,*)) = b",
    "_Elm.GetTriple.el2 (*,(*,c)) = c",
    -- unit
    "data _Elm.Unit = Unit",
    -- bool
    "data _Elm.Bool = True | False"
  ]

-- ADD DEPENDENCIES

addGlobal :: Graph -> State -> Opt.Global -> State
addGlobal graph state@(State revBuilders seen) global =
  if Set.member global seen
    then state
    else
      addGlobalHelp graph global $
        State revBuilders (Set.insert global seen)

addGlobalHelp :: Graph -> Opt.Global -> State -> State
addGlobalHelp graph global state =
  let addDeps deps someState =
        Set.foldl' (addGlobal graph) someState deps
   in case graph ! global of
        Opt.Define expr deps ->
          let stateWithDeps = addDeps deps state
           in addValueDecl global expr stateWithDeps
        Opt.DefineTailFunc argNames body deps ->
          let stateWithDeps = addDeps deps state
           in addFunctionDecl global argNames body stateWithDeps
        Opt.Ctor index arity ->
          -- addStmt
          --   state
          --   ( var global (Expr.generateCtor global index arity)
          --   )
          error "TODO Opt.Ctor"
        Opt.Link linkedGlobal ->
          -- addGlobal graph state linkedGlobal
          error "TODO Opt.Link"
        Opt.Cycle names values functions deps ->
          -- addStmt
          --   (addDeps deps state)
          --   ( generateCycle global names values functions
          --   )
          error "TODO Opt.Cycle"
        Opt.Enum index ->
          -- addStmt
          --   state
          --   ( generateEnum global index
          --   )
          error "TODO Opt.Enum"
        Opt.Box ->
          -- = newtype, most likely
          error "TODO Opt.Box"

addBuilder :: B.Builder -> State -> State
addBuilder builder (State revBuilders seenGlobals) =
  State (builder : revBuilders) seenGlobals

-- foo = (...)
addValueDecl :: Opt.Global -> Opt.Expr -> State -> State
addValueDecl global expr state =
  addBuilder
    ( globalToBuilder global
        <> " = "
        <> exprToBuilder expr
    )
    state

-- foo x y = (...)
addFunctionDecl :: Opt.Global -> [Name] -> Opt.Expr -> State -> State
addFunctionDecl global argNames body state =
  addBuilder
    ( globalToBuilder global
        <> " "
        <> argsToBuilder argNames
        <> " = "
        <> exprToBuilder body
    )
    state

globalToBuilder :: Opt.Global -> B.Builder
globalToBuilder (Opt.Global home name) =
  homeToBuilder home <> "/" <> Name.toBuilder name

argsToBuilder :: [Name] -> B.Builder
argsToBuilder args =
  joinWith " " Name.toBuilder args

joinWith :: B.Builder -> (a -> B.Builder) -> [a] -> B.Builder
joinWith _ _ [] = mempty
joinWith _ fn [a] = fn a
joinWith delim fn (a : as) = fn a <> delim <> joinWith delim fn as

delim :: B.Builder
delim =
  "//"

homeToBuilder :: ModuleName.Canonical -> B.Builder
homeToBuilder (ModuleName.Canonical (Pkg.Name author project) home) =
  delim
    <> Utf8.toBuilder author
    <> delim
    <> Utf8.toBuilder project
    <> delim
    <> Utf8.toBuilder home

exprToBuilder :: Opt.Expr -> B.Builder
exprToBuilder expr =
  let f = exprToBuilder
   in case expr of
        Opt.Bool b ->
          case b of
            True -> "(_Elm.Bool/True)"
            False -> "(_Elm.Bool/False)"
        Opt.Chr str -> error "TODO exprToBuilder Chr"
        Opt.Str str ->
          "\"" <> Utf8.toBuilder str <> "\""
        Opt.Int i ->
          B.stringUtf8 $ show i
        Opt.Float f ->
          Float.toBuilder f
        Opt.VarLocal name -> error "TODO exprToBuilder VarLocal"
        Opt.VarGlobal name -> error "TODO exprToBuilder VarGlobal"
        Opt.VarEnum name i -> error "TODO exprToBuilder VarEnum"
        Opt.VarBox name -> error "TODO exprToBuilder VarBox"
        Opt.VarCycle moduleName name -> error "TODO exprToBuilder VarCycle"
        Opt.VarDebug _ _ _ _ -> error "TODO exprToBuilder VarDebug"
        Opt.List list ->
          "[" <> joinWith "," f list <> "]"
        Opt.Function args body -> error "TODO exprToBuilder Function"
        Opt.Call fn args -> error "TODO exprToBuilder Call"
        Opt.TailCall a as -> error "TODO exprToBuilder TailCall"
        Opt.If a1 a2 -> error "TODO exprToBuilder If"
        Opt.Let def expr_ -> error "TODO exprToBuilder Let"
        Opt.Destruct d expr -> error "TODO exprToBuilder Destruct"
        Opt.Case n1 n2 decider cases -> error "TODO exprToBuilder Case"
        Opt.Accessor name -> error "TODO exprToBuilder Accessor"
        Opt.Access expr_ name -> error "TODO exprToBuilder Access"
        Opt.Update expr_ fields -> error "TODO exprToBuilder Update"
        Opt.Record fields -> error "TODO exprToBuilder Record"
        Opt.Unit ->
          "(_Elm.Unit/Unit)"
        Opt.Tuple t1 t2 mt3 ->
          case mt3 of
            Nothing ->
              "(" <> f t1 <> "," <> f t2 <> ")"
            Just t3 ->
              "(" <> f t1 <> ",(" <> f t2 <> "," <> f t3 <> "))"
