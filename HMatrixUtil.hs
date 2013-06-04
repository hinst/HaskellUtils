module HMatrixUtil
(
	sampleVectors,
	searchVector,
	searchVectorS,
	vectorChooseMax,
	vectorChooseMin,
	visualTestMinMaxVectorElementSearching,
	getRow,
	getColumn,
	replaceRow
)
where 

import Numeric.LinearAlgebra
import Foreign.Storable
import MyTrace
import ListUtil

sampleVectors = 
	[ 
		fromList [-16, 82, 14, -5, -100::Double],
		fromList [11, 13, 16, -17, 0, 19, 19, 19, 12, -6::Double]
	]

type SelectVectorElementFunction t = (t -> t -> Int)
type SelectIndexedVectorElementFunction t = ((Int, t) -> (Int, t) -> Int)

searchVector :: 
	(Storable t) => Vector t -> SelectIndexedVectorElementFunction t -> Int
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
						result = 
							compareFunction
								compareFunctionFirstArgument
								compareFunctionSecondArgument
						compareFunctionFirstArgument = (aIndex, (theVector @> aIndex))
						compareFunctionSecondArgument = (bIndex, (theVector @> bIndex))
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

searchVectorS :: (Storable t) => Vector t -> SelectVectorElementFunction t -> Int
searchVectorS theVector theChooserS = 
	searchVector theVector theChooser
	where
		theChooser (aIndex, a) (bIndex, b) = theChooserS a b

--searchVectorIndexed :: (Storable t) => Vector t -> ()

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
		
showVectorElementAtIndex :: (Storable t, Show t) => Vector t -> Int -> String
showVectorElementAtIndex theVector index =
	if 
		index /= -1
	then
		show (theVector @> index)
	else
		"No"

visualTestMinMaxVectorElementSearching :: Vector Double -> String
visualTestMinMaxVectorElementSearching theVector = 
	show theVector ++ ": " ++ min ++ " ... " ++ max
	where
		min :: String
		min = 
			showVectorElementAtIndex 
				theVector 
				(searchVectorS theVector vectorChooseMin)
		max :: String
		max = 
			showVectorElementAtIndex
				theVector
				(searchVectorS theVector vectorChooseMax)
		show = vecdisp (dispf 2)

getRow :: Element a => Int -> Matrix a -> Vector a
getRow rowNum = flatten . extractRows [rowNum]
 
getColumn :: Element a => Int -> Matrix a -> Vector a
getColumn colNum mat = flatten $ subMatrix (0, colNum) (rows mat, 1) mat

replaceRow :: Element t => Matrix t -> Int -> Vector t -> Matrix t
replaceRow sourceMatrix rowNumber newRow =
	resultMatrix
	where
		sourceMatrixAsRows = toRows sourceMatrix
		resultMatrixAsRows = listReplace sourceMatrixAsRows rowNumber newRow
		resultMatrix = fromRows resultMatrixAsRows



