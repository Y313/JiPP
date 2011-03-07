import Control.Monad.Error
import Data.Char
data ParseError = Err {location::Int, reason::String} deriving (Show)

instance Error ParseError where
	strMsg s = Err 0 s

type ParseMonad = Either ParseError

parseHexDigit :: Char -> Int -> ParseMonad Integer
parseHexDigit c d
	| isHexDigit c = Right (toInteger $ digitToInt c)
	| otherwise = throwError $ strMsg "to nie jest cyfra szesnastkowa"
parseHex :: String -> ParseMonad Integer
parseHex [] = return 0
parseHex [c] = parseHexDigit c 0
parseHex (x:xs) = do
		first <- parseHexDigit x 0
		rest <- parseHex xs
		return $ first * 16 + rest
		
toString :: Integer -> ParseMonad String
toString i
	| i < 0 = do
		rest <- toString (-i)
		return $ '-' : rest
	| i < 16 = return [intToDigit $ fromInteger i]
	| otherwise = do
		rest <- toString $ i `div` 16
		last <- toString $ i `mod` 16
		return $ rest ++ last

--convert zamienia napis z liczba szesnastkowa na napis z liczba dziesietna
convert :: String -> String
convert s = str where
	(Right str) = tryParse s `catchError` printError

tryParse s = do {n <- parseHex s; toString n}
printError (Err loc msg) = return $ concat ["At index ", show loc,":", msg]
