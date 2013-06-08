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



