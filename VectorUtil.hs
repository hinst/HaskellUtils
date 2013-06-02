module VectorUtil
(
	matrixFromList
)
where
 
import Data.Matrix

matrixFromList :: [[t]] -> Vector (Vector t)
matrixFromList list =
	generate (Data.List.length list) getRow
	where
		getRow rowIndex = 
			generate (Data.List.length (list !! rowIndex)) getCellValue
			where
				getCellValue columnIndex = (list !! rowIndex) !! columnIndex
