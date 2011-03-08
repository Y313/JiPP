import Data.Char

readInts:: String -> [Int]
readInts s = map read ( filter (all isDigit) (words s) )

readInts2 :: String -> Either String [Int]
readInts2 s = let l = filter (not . all isDigit) (words s)
				in if l /= [] then Left "Error" else Right (readInts s)
				
sumInts :: String -> String
sumInts s = let l = filter(not . all isDigit) (words s)
			in  if l /= [] then "Error" else show (sum (readInts s))
			
main = intract sumInts
