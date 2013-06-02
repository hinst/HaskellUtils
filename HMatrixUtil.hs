module HMatrixUtil
(
	sampleVectors,
	searchVector,
	visualTestMinMaxVectorElementSearching
)
where 

import Numeric.LinearAlgebra
import Foreign.Storable
import MyTrace

sampleVectors = 
	[ 
		5 |> [-16, 82, 14, -5, -100::Int],
		10 |> [11, 13, 16, -17, 0, 19, 19, 19, 12, -6::Int]
	]

type FindElementSelectorFunction t = (t -> t -> Int)

searchVector :: 
	(Storable t) => Vector t -> FindElementSelectorFunction t -> Int
searchVector theVector compareFunction = 
	go (-1) 0
	where
		traceEnabled = False
		trace logMessage expression = 
			if 
				traceEnabled 
			then
				MyTrace.trace logMessage expression
			else
				expression
		n = dim theVector
		go :: Int -> Int -> Int
		go bestIndex index = 
			trace
				(
					"go " 
					++ show bestIndex 
					++ " " ++ show index 
					++ " = " ++ show (performCompare bestIndex nextIndex)
				)
				(
					performCompare bestIndex nextIndex
				)
			where
				performCompare :: Int -> Int -> Int
				performCompare (-1) (-1) = -1
				performCompare (-1) x = x
				performCompare x (-1) = x
				performCompare aIndex bIndex = 
					if
						result == -1
					then
						-1
					else
						if 
							result == 0
						then
							aIndex
						else
							if
								result == 1
							then
								bIndex
							else
								result --erroneous result
					where
						result = compareFunction (theVector @> aIndex) (theVector @> bIndex)
				nextIndex =
					if 
						index < n
					then
						if
							index == (n - 1)
						then
							index
						else
							go index (index + 1)
					else
						-1

vectorMinIndex :: (Ord t, Storable t) => Vector t -> Int
vectorMinIndex theVector = searchVector theVector minFunction
	where
		minFunction aValue bValue =
			if 
				aValue <= bValue 
			then
				0
			else
				1

vectorChooseMax :: (Ord t) => t -> t -> Int
vectorChooseMax aValue bValue =
	if 
		aValue >= bValue
	then
		0
	else
		1
		
vectorChooseMin :: (Ord t) => t -> t -> Int
vectorChooseMin aValue bValue =
	if
		aValue <= bValue
	then
		0
	else
		1
				
showVectorElementAtIndex :: (Storable t, Show t) => Vector t -> Int -> String
showVectorElementAtIndex theVector index =
	if 
		index /= -1
	then
		show (theVector @> index)
	else
		"No"

visualTestMinMaxVectorElementSearching :: (Storable t, Ord t, Show t) => Vector t -> String
visualTestMinMaxVectorElementSearching theVector = 
	show theVector ++ ": " ++ min ++ " ... " ++ max
	where
		min :: String
		min = 
			showVectorElementAtIndex 
				theVector 
				(searchVector theVector vectorChooseMin)
		max :: String
		max = 
			showVectorElementAtIndex
				theVector
				(searchVector theVector vectorChooseMax)




