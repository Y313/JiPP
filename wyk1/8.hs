indexOf:: Char -> String -> Maybe Int
indexOf c [] = Nothing
indexOf c s = indOf c 0 s where
		indOf c pos [] = Nothing
		indOf c pos s = if head s == c then Just pos else indOf c (pos+1) (tail s)

positions:: Char -> String -> [Int]
positions c [] = []
positions c s = [y | (x,y) <- zip s [0..length s], x == c]
