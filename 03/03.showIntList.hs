showIntList :: [Int] -> String
showIntList [] = "\n"
showIntList [a] = showInt(a) ++ "\n"
showIntList (a:t) = showInt(a) ++ " " ++ showIntList(t)

showInt :: Int -> String
showInt a
	| a < 10 = decDigit(a):[]
	| otherwise = showInt(a `div` 10) ++ [decDigit(a `mod` 10)]

decDigit :: Int -> Char
decDigit 0 = '0'
decDigit (n+1) = succ (decDigit n)

