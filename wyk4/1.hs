allPairs xs ys = do
	x <- xs
	y <- ys
	return [x,y]

allCombinations::[[a]] -> [[a]]
allCombinations = sequence
