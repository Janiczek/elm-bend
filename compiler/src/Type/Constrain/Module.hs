{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Type.Constrain.Module
  ( constrain
  )
  where


import qualified Data.Map.Strict as Map

import qualified AST.Canonical as Can
import qualified Type.Constrain.Expression as Expr
import Type.Type (Constraint(..))



-- CONSTRAIN


constrain :: Can.Module -> IO Constraint
constrain (Can.Module _ _ _ decls _ _ _) =
  constrainDecls decls CSaveTheEnvironment



-- CONSTRAIN DECLARATIONS


constrainDecls :: Can.Decls -> Constraint -> IO Constraint
constrainDecls decls finalConstraint =
  case decls of
    Can.Declare def otherDecls ->
      Expr.constrainDef Map.empty def =<< constrainDecls otherDecls finalConstraint

    Can.DeclareRec def defs otherDecls ->
      Expr.constrainRecursiveDefs Map.empty (def:defs) =<< constrainDecls otherDecls finalConstraint

    Can.SaveTheEnvironment ->
      return finalConstraint
