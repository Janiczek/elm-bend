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
generate (Opt.GlobalGraph graph _ adts) mains =
  let maybeMain =
        case Map.keys mains of
         [] -> Nothing
         main : _ -> Just main
      stateWithAdts = addAdts maybeMain adts emptyState
      finalState = Map.foldrWithKey (addMain graph) stateWithAdts mains
   in stateToBuilder finalState

addAdts :: Maybe ModuleName.Canonical -> Map.Map Opt.Global Opt.BendADT -> State -> State
addAdts maybeMain adts state =
  Map.foldlWithKey (addAdt maybeMain) state adts

addAdt :: Maybe ModuleName.Canonical -> State -> Opt.Global -> Opt.BendADT -> State
addAdt maybeMain state name (Opt.BendADT constructors) =
  -- type Name = (A) | (B arg1) | (C arg1 arg2)
  let constructorToBuilder (name, args) =
        let arg i = B.stringUtf8 $ "arg" <> show i in
        if args == 0
          then Name.toBuilder name
          else "(" <> Name.toBuilder name <> " " <> joinWith " " arg [1..args] <> ")"
      cs = joinWith " | " constructorToBuilder constructors
      builder =
        "type " 
        <> qualifiedNameToBuilder maybeMain name
        <> " = " 
        <> cs
  in
  addBuilder builder state

addMain :: Graph -> ModuleName.Canonical -> Opt.Main -> State -> State
addMain graph home _ state =
  addGlobal (Just home) graph state (Opt.Global home "main")

-- GRAPH TRAVERSAL STATE

data State = State
  { _revBuilders :: [B.Builder],
    _seenGlobals :: Set.Set Opt.Global
  }
  deriving (Show)

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
  List.foldl' (\m b -> b <> "\n" <> m) monolith revBuilders

initBuilders :: [B.Builder]
initBuilders =
  -- TODO munging rules ... //, / etc. instead of $
  reverse
  [ "### Elm->Bend prelude",
    -- tuples
    "_Elm.GetTuple.el0 (a,*) = a",
    "_Elm.GetTuple.el1 (*,b) = b",
    -- triples
    "_Elm.GetTriple.el0 (a,*)     = a",
    "_Elm.GetTriple.el1 (*,(b,*)) = b",
    "_Elm.GetTriple.el2 (*,(*,c)) = c",
    -- unit
    "type Basics/Unit = Unit",
    "###",
    ""

  ]

-- ADD DEPENDENCIES

addGlobal :: Maybe ModuleName.Canonical -> Graph -> State -> Opt.Global -> State
addGlobal maybeMain graph state@(State revBuilders seen) global =
  if Set.member global seen
    then state
    else
      addGlobalHelp maybeMain graph global $
        State revBuilders (Set.insert global seen)

addGlobalHelp :: Maybe ModuleName.Canonical -> Graph -> Opt.Global -> State -> State
addGlobalHelp maybeMain graph global state =
  let -- !_ = Debug.Trace.trace ("XXX2: from graph: " ++ show graph) ()
      addDeps deps someState =
        Set.foldl' (addGlobal maybeMain graph) someState deps
      node = graph ! Debug.Trace.traceShowId global
      -- !_ = Debug.Trace.trace ("XXX1: node: " ++ show node) ()
   in case node of
        Opt.Define expr deps ->
          let stateWithDeps = addDeps deps state
           in addValueDecl maybeMain global expr stateWithDeps
        Opt.DefineTailFunc argNames body deps ->
          let stateWithDeps = addDeps deps state
           in addFunctionDecl maybeMain global argNames body stateWithDeps
        Opt.Ctor ->
          state
        Opt.Link linkedGlobal ->
          -- addGlobal graph state linkedGlobal
          error ("TODO Opt.Link: " ++ show linkedGlobal)
        Opt.Cycle names values functions deps ->
          -- addStmt
          --   (addDeps deps state)
          --   ( generateCycle global names values functions
          --   )
          error "TODO Opt.Cycle"

addBuilder :: B.Builder -> State -> State
addBuilder builder (State revBuilders seenGlobals) =
  State (builder : revBuilders) seenGlobals

-- foo = (...)
addValueDecl :: Maybe ModuleName.Canonical -> Opt.Global -> Opt.Expr -> State -> State
addValueDecl maybeMain name expr state =
  addBuilder
    ( qualifiedNameToBuilder maybeMain name
        <> " = "
        <> exprToBuilder maybeMain expr
    )
    state

-- foo x y = (...)
addFunctionDecl :: Maybe ModuleName.Canonical -> Opt.Global -> [Name] -> Opt.Expr -> State -> State
addFunctionDecl maybeMain name argNames body state =
  addBuilder
    ( qualifiedNameToBuilder maybeMain name
        <> " "
        <> argsToBuilder argNames
        <> " = "
        <> exprToBuilder maybeMain body
    )
    state

-- if main module, don't qualify
qualifiedNameToBuilder :: Maybe ModuleName.Canonical -> Opt.Global -> B.Builder
qualifiedNameToBuilder maybeMain (Opt.Global home@(ModuleName.Canonical _ module_) name) =
  case maybeMain of
    Nothing -> qualified
    Just main ->
      if home == main
      then Name.toBuilder name
      else qualified
  where qualified = Name.toBuilder module_ <> delim <> Name.toBuilder name

argsToBuilder :: [Name] -> B.Builder
argsToBuilder args =
  joinWith " " Name.toBuilder args

joinWith :: B.Builder -> (a -> B.Builder) -> [a] -> B.Builder
joinWith _ _ [] = mempty
joinWith _ fn [a] = fn a
joinWith delim fn (a : as) = fn a <> delim <> joinWith delim fn as

delim :: B.Builder
delim =
  "/"

exprToBuilder :: Maybe ModuleName.Canonical -> Opt.Expr -> B.Builder
exprToBuilder maybeMain expr =
  let f = exprToBuilder maybeMain
   in case expr of
        Opt.Chr str -> error "TODO exprToBuilder Chr"
        Opt.Str str ->
          "\"" <> Utf8.toBuilder str <> "\""
        Opt.Int i ->
          B.stringUtf8 $ show i
        Opt.Float f ->
          Float.toBuilder f
        Opt.VarLocal name -> error "TODO exprToBuilder VarLocal"
        Opt.VarGlobal name ->
          -- (Basics/foo) (if in non-Main module)
          -- (foo)        (if in Main module)
          "(" <> qualifiedNameToBuilder maybeMain name <> ")"
        Opt.VarCtor adtName global@(Opt.Global home@(ModuleName.Canonical _ module_) ctorName) -> 
          -- Basics/Bool/True (if in non-Main module)
          -- MyType/MyCtor    (if in Main module)
          case maybeMain of
            Nothing -> qualified
            Just main ->
              if home == main
              then unqualified
              else qualified
          where
            qualified =
              "("
              <> Name.toBuilder module_ 
              <> delim
              <> Name.toBuilder adtName
              <> delim
              <> Name.toBuilder ctorName
              <> ")"
            unqualified =
              "("
              <> Name.toBuilder adtName
              <> delim
              <> Name.toBuilder ctorName
              <> ")"
        Opt.VarCycle moduleName name -> error "TODO exprToBuilder VarCycle"
        Opt.DebugTodo ->
          error "Elm->Bend: encountered Debug.todo"
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
          "(Basics/Unit/Unit)"
        Opt.Tuple t1 t2 mt3 ->
          case mt3 of
            Nothing ->
              "(" <> f t1 <> "," <> f t2 <> ")"
            Just t3 ->
              "(" <> f t1 <> ",(" <> f t2 <> "," <> f t3 <> "))"

