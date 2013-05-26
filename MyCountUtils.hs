module MyCountUtils (Countable, count) where

class Countable object where
	count :: object -> Int
