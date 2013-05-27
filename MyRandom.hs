module MyRandom
(
)
where

import System.Random

randomInteger :: Int
randomInteger = 
	do
		randomNumberGenerator <- newStdGen
		return$ fst (next randomNumberGenerator)

randomIntegerConstrained lowerBound higherBound -- inclusive
	= lowerBound + (randomInteger `mod` (higherBound - lowerBound + 1))
