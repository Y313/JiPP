indexOf :: (Eq a) => a -> [a] -> Maybe Int
indexOf element list = indexOf element list 0 where
	indexOf _ [] _ = Nothing
	indexOf a (h:t) offset
		| a == h = Just offset
		| otherwise = indexOf a t (offset+1)
