import System.Environment(getArgs)
main = do
	(f:_) <- getArgs
	file <- readFile f
	putStrLn $ "linie:" ++ (show $ length $ lines file)
	putStrLn $ "słowa:" ++ (show $ length $ words file)
	putStrLn $ "znaki:" ++ (show $ length file)
