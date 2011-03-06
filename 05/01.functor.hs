
instance Functor (Either e) where
	fmap _ (Left x) = Left x
	fmap f (Right x) = Right (f x)

reverseRightA (Left x) = Left x
reverseRightA (Right x) = Right (reverse x)

type Result a = Either String a

reverseRightB :: Result [a] -> Result [a]
reverseRight = fmap reverse

readInts :: String -> [Int]
readInts s = map read . filter (all isDigit) . words
