module MatrixUtil
(
	MatrixUtil.fromList
)
where

import Data.Matrix
import Data.Vector
import Data.List

fromList :: [[t]] -> Matrix t
fromList list = matrix (Data.List.length list) (Data.List.length (list !! 0)) getCellValue
	where
		getCellValue (rowIndex, columnIndex) = 
			list !! (rowIndex - 1) !! (columnIndex - 1)
	
