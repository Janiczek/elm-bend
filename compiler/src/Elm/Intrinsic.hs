module Elm.Intrinsic (Intrinsic(..), name, parse) where

import Data.Binary (Binary, get, put, getWord8, putWord8)

import qualified Data.Map as Map
import qualified Data.Name as Name

data Intrinsic
  = Add
  | Sub
  | Mul
  | Fdiv
  | Idiv
  | Pow
  deriving (Show, Bounded, Enum)

instance Binary Intrinsic where
  put item =
    case item of
      Add  -> putWord8 0
      Sub  -> putWord8 1
      Mul  -> putWord8 2
      Fdiv -> putWord8 3
      Idiv -> putWord8 4
      Pow  -> putWord8 5
  get = do 
    word <- getWord8
    case word of
      0 -> return Add
      1 -> return Sub
      2 -> return Mul
      3 -> return Fdiv
      4 -> return Idiv
      5 -> return Pow
      _ -> fail "problem getting Intrinsic binary"

name :: Intrinsic -> Name.Name
name intrinsic =
  Name.fromChars $ 
    case intrinsic of
      Add -> "add"
      Sub -> "sub"
      Mul -> "mul"
      Fdiv -> "fdiv"
      Idiv -> "idiv"
      Pow -> "pow"

mapping :: Map.Map Name.Name Intrinsic
mapping =
  let all = enumFrom (minBound :: Intrinsic)
  in
  Map.fromList (map (\item -> (name item, item)) all)
  
parse :: Name.Name -> Maybe Intrinsic
parse name =
  Map.lookup name mapping
