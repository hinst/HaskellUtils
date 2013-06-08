module VectorUtil
(
	searchVectorIndexed,
	searchVector,
	vectorChooseMin,
	vectorChooseMax
)
where

import qualified Data.Vector as V
import MyTrace

type Vector t = V.Vector t

sampleVectors = 
	[ 
		V.fromList [-16, 82, 14, -5, -100::Double],
		V.fromList [11, 13, 16, -17, 0, 19, 19, 19, 12, -6::Double]
	]

type SelectIndexedVectorElementFunction t = ((Int, t) -> (Int, t) -> Int)

searchVectorIndexed ::
	Vector t -> SelectIndexedVectorElementFunction t -> Int
searchVectorIndexed theVector compareFunction =
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
		n = V.length theVector
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
				performCompare (-1) i = performCompare i i
				performCompare i (-1) = performCompare i i
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
						result = 
							compareFunction
								compareFunctionFirstArgument
								compareFunctionSecondArgument
						compareFunctionFirstArgument = (aIndex, (theVector V.! aIndex))
						compareFunctionSecondArgument = (bIndex, (theVector V.! bIndex))
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


type SelectVectorElementFunction t = (t -> t -> Int)
searchVector :: Vector t -> SelectVectorElementFunction t -> Int
searchVector theVector theChooserS = 
	searchVectorIndexed theVector theChooser
	where
		theChooser (aIndex, a) (bIndex, b) = theChooserS a b

vectorChooseMin :: (Ord t) => t -> t -> Int
vectorChooseMin aValue bValue =
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
		
