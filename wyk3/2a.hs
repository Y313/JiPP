import System.Environment(getArgs)

main = do
	args <- getArgs
	mapM_ putStrLn args 
