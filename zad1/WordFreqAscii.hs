import Dict
import List
import System (getArgs, getProgName)
import System.IO

min_length = 2
max_words = 10

main :: IO ()
main = do
	args <- getArgs
	case args of
		[] -> usage
		[file] -> catch (wordStat file) (\_ -> readError file)
		n -> usage


-- produce raport about max_words most often words in file
wordStat :: String -> IO ()
wordStat file = do
	fileContent <- readFile file
	mapM_ (\(c,f) -> putStrLn $ f ++ ' ':(show c)) $ calculateWordStat fileContent

-- return max_words most often ASCII words in string with counts
calculateWordStat :: String -> [(Integer, String)]
calculateWordStat text = topK max_words $ map swap $ countObjects getWords where
	getWords = filter (\w -> min_length < (length w) && (all isAscii w)) $ words text
	swap (a,b) = (b,a)

-- return objects with number of occurrences in array
countObjects :: Ord a => [a] -> [(a,Integer)]
countObjects words =  toList $ foldl increaseCounter (fromList []) words where
	increaseCounter d w = Dict.insert w (succ (lookupOr0 w d)) d
	lookupOr0 k d = defaultIfNothing 0 $ Dict.lookup k d
	defaultIfNothing _ (Just a) = a
	defaultIfNothing a Nothing = a

-- return true if Char is a-zZ-A (implementation from Haskell lib is not uset because of exercise restriction)
isAscii :: Char -> Bool
isAscii c = (('a' <= c) && (c <= 'z')) || (('A' <= c) && (c <= 'Z'))

-- sort object DESC and return k greatest in order
topK :: Ord a => Int -> [a] -> [a]
topK k = (take k).reverse.sort

-- put message on stderr
errPutStr = hPutStr stderr

-- put message on stderr with EOL
errPutStrLn = hPutStrLn stderr

-- show program usage info
usage :: IO ()
usage = do
	progName <- getProgName
	errPutStr "Usage: "
	errPutStr progName
	errPutStrLn " fileName - show most common words in file."

-- show file read error
readError :: String -> IO ()
readError file = do
	progName <- getProgName
	errPutStr progName
	errPutStr ": can't read file "
	errPutStr file
	errPutStrLn "."
