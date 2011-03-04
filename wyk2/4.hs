import Data.Char
readInts:: String -> [Int]
readInts s = [read w | w <- words s, all isDigit w]

readInts2:: String -> Either String [Int]
readInts2 s = let l = filter (not . all isDigit) (words s)
	      in if l /= [] then Left "Error" else Right (readInts s)

instance Functor (Either e) where
	--fmap :: (a -> b) -> Either e a -> Either e b
	fmap f (Right a) = Right $ f a
	fmap f (Left a) = Left a


sumInts:: String -> String
sumInts s = case f s of Left s -> s
			Right num -> show num
	    where f s = fmap (sum) $ readInts2 s

main = interact sumInts
