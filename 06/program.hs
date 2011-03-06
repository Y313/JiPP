import System (getArgs, getProgName)
import System.IO

main :: IO ()
main = do
	args <- getArgs
	mapM_ putStrLn args
	case args of
		[] -> putStrLn "za malo"
		[a] -> wc a
		n -> usage

wc :: String -> IO ()
wc s = do
	print "Dane o pliku:"
	c <- readFile s
	print $ length $ c
	print $ length $ words c
	print $ (length $ filter (\c -> c == '\n') c) + 1

usage :: IO ()
usage = do
	print ""
