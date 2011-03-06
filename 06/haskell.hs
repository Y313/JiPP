import System.IO

main :: IO ()
main = do
	haskell

haskell :: IO ()
haskell = do
	line <- getLine
	if line == "Haskell" then return () else haskell
