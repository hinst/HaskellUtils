module MyArrayUtils (NormalArray, Normal2DArray, toArray, matrixArrayToString) where

import Data.Array
import MyCountUtils

type NormalArray elementType = Array Int elementType
type Normal2DArray elementType = Array (Int, Int) elementType

toArray :: [elementType] -> NormalArray elementType
toArray list =
	listArray (0, length list - 1) list

instance Countable (NormalArray elementType) where --here is an important note: -XFlexibleInstances command line argument must be used in order to compile this code
	count theArray = fst (bounds theArray) + 1
	
matrixArrayToString :: NormalArray (NormalArray elementType) -> String
matrixArrayToString matrix = ""