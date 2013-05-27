module MyArrayUtils 
(
	NormalArray, 
	Normal2DArray, 
	toArray, 
	count, 
	extractArrayElement, 
	normal2DArrayToString
) 
where

import Data.Array
import MyCountUtils
import MyTrace

type NormalArray elementType = Array Int elementType
type Normal2DArray elementType = Array (Int, Int) elementType

toArray :: [elementType] -> NormalArray elementType
toArray list =
	listArray (0, length list - 1) list

instance Countable (NormalArray elementType) where --here is an important note: -XFlexibleInstances command line argument must be used in order to compile this code
	count theArray = snd (bounds theArray) + 1

enableExtractArrayElementDebugTrace = False
extractArrayElement :: NormalArray elementType -> Int -> elementType
extractArrayElement theArray index =
	if 
		enableExtractArrayElementDebugTrace
	then
		trace 
			(
				"<Now extracting element #" ++ show index
				 ++ " count of elements in array: " ++ show (count theArray) ++ ">"
			)
			result
	else
		result
	where 
		result = theArray ! index
		
normal2DArrayToStringIterate :: (Show elementType) => 
	Normal2DArray elementType -> Int -> Int -> Int -> Int -> String
normal2DArrayToStringIterate theArray x y w h =
	if 
		(x < w) && (y < h)
	then
		show (theArray ! (y, x) ) 
		++ 
		following
	else
		""
	where
		following =
			if 
				(x + 1) < w
			then
				' ' : normal2DArrayToStringIterate theArray (x + 1) y w h
			else
				'\n' : normal2DArrayToStringIterate theArray 0 (y + 1) w h

normal2DArrayToString :: (Show elementType) => Normal2DArray elementType -> String
normal2DArrayToString theArray = 
	normal2DArrayToStringIterate theArray 0 0 width height
	where
		dimension = snd (bounds theArray)
		height = fst dimension + 1
		width = snd dimension + 1
