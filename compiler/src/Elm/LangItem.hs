module Elm.LangItem (LangItem(..), name, parse) where

import Data.Binary (Binary, get, put, getWord8, putWord8)

import qualified Data.Map as Map
import qualified Data.Name as Name

data LangItem
  = Add
  | Sub
  | Mul
  | Fdiv
  | Idiv
  | Pow
  deriving (Show, Bounded, Enum)

instance Binary LangItem where
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
      _ -> fail "problem getting LangItem binary"

name :: LangItem -> Name.Name
name langItem =
  Name.fromChars $ 
    case langItem of
      Add -> "add"
      Sub -> "sub"
      Mul -> "mul"
      Fdiv -> "fdiv"
      Idiv -> "idiv"
      Pow -> "pow"

mapping :: Map.Map Name.Name LangItem
mapping =
  let all = enumFrom (minBound :: LangItem)
  in
  Map.fromList (map (\item -> (name item, item)) all)
  
parse :: Name.Name -> Maybe LangItem
parse name =
  Map.lookup name mapping
