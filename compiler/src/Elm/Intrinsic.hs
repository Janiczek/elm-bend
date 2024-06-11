{-# LANGUAGE OverloadedStrings #-}
module Elm.Intrinsic (Intrinsic(..), name, parse, toBuilder) where

import Data.Binary (Binary, get, put, getWord8, putWord8)

import qualified Data.Map as Map
import qualified Data.Name as Name
import qualified Data.ByteString.Builder as B

import qualified AST.Canonical as Can
import qualified Elm.ModuleName as ModuleName

data Intrinsic
  = Add
  | Sub
  | Mul
  | Fdiv
  | Idiv
  | Pow
  | Append
  deriving (Show, Bounded, Enum)

instance Binary Intrinsic where
  put item =
    case item of
      Add    -> putWord8 0
      Sub    -> putWord8 1
      Mul    -> putWord8 2
      Fdiv   -> putWord8 3
      Idiv   -> putWord8 4
      Pow    -> putWord8 5
      Append -> putWord8 6
  get = do 
    word <- getWord8
    case word of
      0 -> return Add
      1 -> return Sub
      2 -> return Mul
      3 -> return Fdiv
      4 -> return Idiv
      5 -> return Pow
      6 -> return Append
      _ -> fail "problem getting Intrinsic binary"

name :: Intrinsic -> Name.Name
name intrinsic =
  Name.fromChars $ 
    case intrinsic of
      Add    -> "add"
      Sub    -> "sub"
      Mul    -> "mul"
      Fdiv   -> "fdiv"
      Idiv   -> "idiv"
      Pow    -> "pow"
      Append -> "append"

mapping :: Map.Map Name.Name Intrinsic
mapping =
  let all = enumFrom (minBound :: Intrinsic)
   in Map.fromList (map (\item -> (name item, item)) all)
  
parse :: Name.Name -> Maybe Intrinsic
parse name =
  Map.lookup name mapping


toBuilder :: Can.Type -> Intrinsic -> B.Builder
toBuilder tipe intrinsic =
  case intrinsic of
    Add    -> "(@a @b (+ a b))"
    Sub    -> "(@a @b (- a b))"
    Mul    -> "(@a @b (* a b))"
    Fdiv   -> "(@a @b (/ a b))"
    Idiv   -> "(@a @b (/ a b))"
    Pow    -> error "TODO Intrinsic.toBuilder - Pow"
    Append ->
      case tipe of
        Can.TLambda appendable (Can.TLambda _ _) -> 
          case appendable of
            Can.TType moduleName typeName typeVars ->
              if moduleName == ModuleName.string && typeName == Name.string && typeVars == [] then
                error "TODO Intrinsic.toBuilder - Append String"
              else if moduleName == ModuleName.list && typeName == Name.list then
                error "TODO Intrinsic.toBuilder - Append List"
              else
                error $ "Elm->Bend: unknown named appendable (not String.String, not List.List): " ++ show appendable
            _ -> 
              error $ "Elm->Bend: unknown appendable (not a named type): " ++ show appendable
        _ ->
          error $ "Elm->Bend: unknown append usage (not appendable -> appendable -> appendable): " ++ show tipe
