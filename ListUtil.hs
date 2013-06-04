module ListUtil
(
	listReplace,
	sampleLists,
	visualTestListReplace
)
where

import Data.List

listReplace :: [t] -> Int -> t -> [t]
listReplace (x:xs) index value
     | index == 0 = value : xs
     | otherwise = x : listReplace xs (index - 1) value

sampleLists =
	[
		[1, 2, 3, 4, 5]
	]
	
visualTestListReplace :: Show t => [t] -> Int -> t -> String
visualTestListReplace list index value =
	show list ++ "[" ++ show index ++ "] = " ++ show value ++ "\n" ++ show result
	where
		result = listReplace list index value

