{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Optimize.Module
  ( optimize
  )
  where


import Prelude hiding (cycle)
import Control.Monad (foldM)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Name as Name
import qualified Data.Set as Set
import Data.Map ((!))

import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
import qualified Elm.ModuleName as ModuleName
import qualified Optimize.Expression as Expr
import qualified Optimize.Names as Names
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Main as E
import qualified Reporting.Result as Result
import qualified Reporting.Warning as W



-- OPTIMIZE


type Result i w a =
  Result.Result i w E.Error a

type Annotations =
  Map.Map Name.Name Can.Annotation


optimize :: Annotations -> Can.Module -> Result i [W.Warning] Opt.LocalGraph
optimize annotations (Can.Module home _ _ decls unions aliases _) =
  addDecls home annotations decls $
    addUnions home unions $
      addAliases home aliases $
        Opt.LocalGraph Nothing Map.empty Map.empty Map.empty



-- UNION


addUnions :: ModuleName.Canonical -> Map.Map Name.Name Can.Union -> Opt.LocalGraph -> Opt.LocalGraph
addUnions home unions (Opt.LocalGraph main nodes fields adts) =
  Opt.LocalGraph
    main
    (Map.foldr (addUnionCtors home) nodes unions)
    fields
    (Map.foldrWithKey (addUnionADT home) adts unions)

addUnionCtors :: ModuleName.Canonical -> Can.Union -> Map.Map Opt.Global Opt.Node -> Map.Map Opt.Global Opt.Node
addUnionCtors home (Can.Union _ ctors _) nodes =
  List.foldl' (addUnionCtor home) nodes ctors

addUnionCtor :: ModuleName.Canonical -> Map.Map Opt.Global Opt.Node -> Can.Ctor -> Map.Map Opt.Global Opt.Node
addUnionCtor home nodes (Can.Ctor ctorName _ _ _) =
  Map.insert (Opt.Global home ctorName) Opt.Ctor nodes


addUnionADT :: ModuleName.Canonical -> Name.Name -> Can.Union -> Map.Map Opt.Global Opt.BendADT -> Map.Map Opt.Global Opt.BendADT
addUnionADT home adtName (Can.Union _ ctors _) adts =
  let toCtor (Can.Ctor ctorName _ ctorLength _) = (ctorName,ctorLength)
      global = Opt.Global home adtName
      adt = Opt.BendADT (map toCtor ctors)
  in
  Map.insert global adt adts



-- ALIAS


addAliases :: ModuleName.Canonical -> Map.Map Name.Name Can.Alias -> Opt.LocalGraph -> Opt.LocalGraph
addAliases home aliases graph =
  Map.foldrWithKey (addAlias home) graph aliases


addAlias :: ModuleName.Canonical -> Name.Name -> Can.Alias -> Opt.LocalGraph -> Opt.LocalGraph
addAlias home name (Can.Alias _ tipe) graph@(Opt.LocalGraph main nodes fieldCounts adts) =
  case tipe of
    Can.TRecord fields Nothing ->
      let
        function =
          Opt.Function (map fst (Can.fieldsToList fields)) $ Opt.Record $
            Map.mapWithKey (\field _ -> Opt.VarLocal field) fields

        node =
          Opt.Define function Set.empty
      in
      Opt.LocalGraph
        main
        (Map.insert (Opt.Global home name) node nodes)
        (Map.foldrWithKey addRecordCtorField fieldCounts fields)
        adts

    _ ->
      graph


addRecordCtorField :: Name.Name -> Can.FieldType -> Map.Map Name.Name Int -> Map.Map Name.Name Int
addRecordCtorField name _ fields =
  Map.insertWith (+) name 1 fields



-- HELPER


addToGraph :: Opt.Global -> Opt.Node -> Map.Map Name.Name Int -> Opt.LocalGraph -> Opt.LocalGraph
addToGraph name node fields (Opt.LocalGraph main nodes fieldCounts adts) =
  Opt.LocalGraph
    main
    (Map.insert name node nodes)
    (Map.unionWith (+) fields fieldCounts)
    adts



-- ADD DECLS


addDecls :: ModuleName.Canonical -> Annotations -> Can.Decls -> Opt.LocalGraph -> Result i [W.Warning] Opt.LocalGraph
addDecls home annotations decls graph =
  case decls of
    Can.Declare def subDecls ->
      addDecls home annotations subDecls =<< addDef home annotations def graph

    Can.DeclareRec d@(Can.Def _ _ _) ds subDecls ->
      handleDefNormally d ds subDecls

    Can.DeclareRec d@(Can.TypedDef name _ _ _ _) ds subDecls ->
      if A.toValue name == "todo"
      then error "somehow handle Debug.todo"
      else handleDefNormally d ds subDecls

    Can.SaveTheEnvironment ->
      Result.ok graph
  where
    handleDefNormally d ds subDecls =
      let defs = d:ds in
      case findMain defs of
        Nothing ->
          addDecls home annotations subDecls (addRecDefs home defs graph)

        Just region ->
          Result.throw $ E.BadCycle region (defToName d) (map defToName ds)


findMain :: [Can.Def] -> Maybe A.Region
findMain defs =
  case defs of
    [] ->
      Nothing

    def:rest ->
      case def of
        Can.Def (A.At region name) _ _ ->
          if name == Name._main then Just region else findMain rest

        Can.TypedDef (A.At region name) _ _ _ _ ->
          if name == Name._main then Just region else findMain rest


defToName :: Can.Def -> Name.Name
defToName def =
  case def of
    Can.Def (A.At _ name) _ _          -> name
    Can.TypedDef (A.At _ name) _ _ _ _ -> name



-- ADD DEFS


addDef :: ModuleName.Canonical -> Annotations -> Can.Def -> Opt.LocalGraph -> Result i [W.Warning] Opt.LocalGraph
addDef home annotations def graph =
  case def of
    Can.Def (A.At region name) args body ->
      do  let (Can.Forall _ tipe) = annotations ! name
          Result.warn $ W.MissingTypeAnnotation region name tipe
          addDefHelp home name args body graph

    Can.TypedDef (A.At _ name) _ typedArgs body _ ->
      addDefHelp home name (map fst typedArgs) body graph


addDefHelp :: ModuleName.Canonical -> Name.Name -> [Can.Pattern] -> Can.Expr -> Opt.LocalGraph -> Result i w Opt.LocalGraph
addDefHelp home name args body graph@(Opt.LocalGraph _ nodes fieldCounts adts) =
  if name /= Name._main then
    Result.ok (addDefNode home name args body Set.empty graph)
  else
    let
      addMain (deps, fields, main) =
        addDefNode home name args body deps $
          Opt.LocalGraph (Just main) nodes (Map.unionWith (+) fields fieldCounts) adts
    in
    Result.ok $ addMain $ Names.run Names.noop


addDefNode :: ModuleName.Canonical -> Name.Name -> [Can.Pattern] -> Can.Expr -> Set.Set Opt.Global -> Opt.LocalGraph -> Opt.LocalGraph
addDefNode home name args body mainDeps graph =
  let
    (deps, fields, def) =
      Names.run $
        case args of
          [] ->
            Expr.optimize Set.empty body

          _ ->
            do  (argNames, destructors) <- Expr.destructArgs args
                obody <- Expr.optimize Set.empty body
                pure $ Opt.Function argNames $
                  foldr Opt.Destruct obody destructors
  in
  addToGraph (Opt.Global home name) (Opt.Define def (Set.union deps mainDeps)) fields graph



-- ADD RECURSIVE DEFS


data State =
  State
    { _values :: [(Name.Name, Opt.Expr)]
    , _functions :: [Opt.Def]
    }
  deriving (Show)


addRecDefs :: ModuleName.Canonical -> [Can.Def] -> Opt.LocalGraph -> Opt.LocalGraph
addRecDefs home defs (Opt.LocalGraph main nodes fieldCounts adts) =
  let
    names = reverse (map toName defs)
    cycleName = Opt.Global home (Name.fromManyNames names)
    cycle = foldr addValueName Set.empty defs
    links = foldr (addLink home (Opt.Link cycleName)) Map.empty defs

    (deps, fields, State values funcs) =
      Names.run $
        foldM (addRecDef cycle) (State [] []) defs
  in
  Opt.LocalGraph
    main
    (Map.insert cycleName (Opt.Cycle names values funcs deps) (Map.union links nodes))
    (Map.unionWith (+) fields fieldCounts)
    adts


toName :: Can.Def -> Name.Name
toName def =
  case def of
    Can.Def      (A.At _ name) _ _     -> name
    Can.TypedDef (A.At _ name) _ _ _ _ -> name


addValueName :: Can.Def -> Set.Set Name.Name -> Set.Set Name.Name
addValueName def names =
  case def of
    Can.Def      (A.At _ name)   args _   -> if null args then Set.insert name names else names
    Can.TypedDef (A.At _ name) _ args _ _ -> if null args then Set.insert name names else names


addLink :: ModuleName.Canonical -> Opt.Node -> Can.Def -> Map.Map Opt.Global Opt.Node -> Map.Map Opt.Global Opt.Node
addLink home link def links =
  case def of
    Can.Def (A.At _ name) _ _ ->
      Map.insert (Opt.Global home name) link links

    Can.TypedDef (A.At _ name) _ _ _ _ ->
      Map.insert (Opt.Global home name) link links



-- ADD RECURSIVE DEFS


addRecDef :: Set.Set Name.Name -> State -> Can.Def -> Names.Tracker State
addRecDef cycle state def =
  case def of
    Can.Def (A.At _ name) args body ->
      addRecDefHelp cycle state name args body

    Can.TypedDef (A.At _ name) _ args body _ ->
      addRecDefHelp cycle state name (map fst args) body


addRecDefHelp :: Set.Set Name.Name -> State -> Name.Name -> [Can.Pattern] -> Can.Expr -> Names.Tracker State
addRecDefHelp cycle (State values funcs) name args body =
  case args of
    [] ->
      do  obody <- Expr.optimize cycle body
          pure $ State ((name, obody) : values) funcs

    _:_ ->
      do  odef <- Expr.optimizePotentialTailCall cycle name args body
          pure $ State values (odef : funcs)
