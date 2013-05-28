module MyArrayUtils 
(
	NormalArray, 
	Normal2DArray, 
	Normal2DArray',
	Normal2DArrayIndexedElement,
	Normal2DArrayIndexedElement'(Normal2DArrayIndexedElement', NoNormal2DArrayIndexedElement),
	Normal2DArrayGenerationList,
	toArray, 
	to2DArray,
	generate2DArray,
	count, 
	extractArrayElement, 
	normal2DArrayToString,
	normal2DArrayIndexedElement,
	normal2DArrayExtractSpecificElement,
	normal2DArrayMinimum
) 
where

import Data.Array
import MyCountUtils
import MyTrace

type NormalArray elementType = Array Int elementType
type Normal2DArray elementType = Array (Int, Int) elementType
data Normal2DArray' elementType = Normal2DArray' (Array (Int, Int) elementType)
type Normal2DArrayIndexedElement valueType = ((Int, Int), valueType)
data Normal2DArrayIndexedElement' valueType = 
	Normal2DArrayIndexedElement' (Int, Int) valueType | NoNormal2DArrayIndexedElement deriving Eq
type Normal2DArrayGenerationList elementType = [Normal2DArrayIndexedElement elementType]

toArray :: [elementType] -> NormalArray elementType
toArray list =
	listArray (0, length list - 1) list
	
to2DArray :: [[elementType]] -> Normal2DArray elementType
to2DArray list =
	array ((0, 0), (height - 1, width - 1)) values
	where
		height = length list
		width = length (list !! 0)
		getValue y x = (list !! y) !! x
		values = [ ((y, x), getValue y x) | y <- [0 .. height - 1], x <- [0.. width - 1] ]
		
 
generate2DArray :: Normal2DArrayGenerationList t -> Int -> Int -> t -> Normal2DArray t
generate2DArray list height width defaultElement = 
	array 
		((0, 0), (height - 1, width - 1))
		[ ((y, x), defaultElement) | y <- [0 .. height - 1], x <- [0 .. width - 1] ]
	//
	list

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
	Normal2DArray elementType -> Int -> Int -> Int -> Int -> Int -> String
normal2DArrayToStringIterate theArray x y w h elementLength =
	if 
		(x < w) && (y < h)
	then
		alignLength (show (theArray ! (y, x) ) )
		++ 
		following
	else
		""
	where
		following =
			if 
				(x + 1) < w
			then
				' ' : normal2DArrayToStringIterate theArray (x + 1) y w h elementLength
			else
				'\n' : normal2DArrayToStringIterate theArray 0 (y + 1) w h elementLength
		alignLength text = 
			if 
				textLength < elementLength
			then
				take (elementLength - textLength) (repeat ' ') ++ text
			else
				text
			where
				textLength = length text
		
			

normal2DArrayToString :: (Show elementType) => Normal2DArray elementType -> Int -> String
normal2DArrayToString theArray elementLength = 
	normal2DArrayToStringIterate theArray 0 0 width height elementLength
	where
		dimension = snd (bounds theArray)
		height = fst dimension + 1
		width = snd dimension + 1
		
getYOf2DArrayIndexedElement :: Normal2DArrayIndexedElement valueType -> Int
getYOf2DArrayIndexedElement element = fst (fst element)

getXOf2DArrayIndexedElement :: Normal2DArrayIndexedElement valueType -> Int
getXOf2DArrayIndexedElement element = snd (fst element)

getValueOf2DArrayIndexedElement :: Normal2DArrayIndexedElement valueType -> valueType
getValueOf2DArrayIndexedElement element = snd element

normal2DArrayIndexedElement :: Normal2DArrayIndexedElement t -> Normal2DArrayIndexedElement' t
normal2DArrayIndexedElement element@(index, value) = (Normal2DArrayIndexedElement' index value)

instance (Show elementType) => Show (Normal2DArrayIndexedElement' elementType) where
	show (Normal2DArrayIndexedElement' index value) = 
		"at line " ++ show (fst index) ++ " column " ++ show (snd index) ++ ": " ++ show value

normal2DArrayExtractSpecificElement :: 
	Normal2DArray t
	-> 
	(
		Normal2DArrayIndexedElement' t
		->
		Normal2DArrayIndexedElement' t
		->
		Normal2DArrayIndexedElement' t
	)
	-> Normal2DArrayIndexedElement' t
normal2DArrayExtractSpecificElement theArray choose =
	go 0 0
	where
		traceEnabled = False
		traceMe message x = 
			if 
				traceEnabled
			then
				trace message x
			else
				x
		size = snd (bounds theArray)
		height = fst size + 1
		width = snd size + 1
		go y x =
			choose current next
			where
				current = Normal2DArrayIndexedElement' (y, x) (theArray ! (y, x))
				next = 
					if 
						(x == width - 1) && (y == height - 1)
					then
						NoNormal2DArrayIndexedElement
					else
						if 
							(x + 1 <= width - 1)
						then
							go y (x + 1)
						else
							go (y + 1) 0
	

normal2DArrayMinimum :: 
	(Ord elementType, Show elementType) => 
		Normal2DArray elementType -> Normal2DArrayIndexedElement' elementType
normal2DArrayMinimum theArray = 
	normal2DArrayExtractSpecificElement theArray minimum
	where 
		minimum 
			a@(Normal2DArrayIndexedElement' _ aValue)
			b@(Normal2DArrayIndexedElement' _ bValue)
			= 
			if 
				aValue <= bValue
			then
				a
			else
				b
		minimum 
			a@(Normal2DArrayIndexedElement' _ aValue)
			(NoNormal2DArrayIndexedElement)
			= 
			a
