import Data.Char
import Control.Monad.Instances

instance Monad (Either error) where
	return = Right
	(Left e)  >>= _ = Left e
	(Right x) >>= k = k x

readInts2::String -> Either String [Int]
readInts2 s = readInts2' $ words s

readInts2'::[String] -> Either String [Int]
readInts2' [] = return []

readInts2' (x:xs) = do 
	    tail <- readInts2' xs
	    result <- if all isDigit x then Right (read x:tail) else Left "error"
	    return result
        
sumInts::String -> String
sumInts s = case f s of Left s -> s
                        Right num -> show num
            where f s = fmap (sum) $ readInts2 s
