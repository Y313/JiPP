positions :: (Eq a) => a -> [a] -> [Int]
positions element list = positions element list 0 where
	positions _ [] _ = []
	positions a (h:t) offset
		| a == h = offset:x
		| otherwise = x
		where x = positions a t (offset+1)
