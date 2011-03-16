module SimpleParsers where
import Data.Char (isDigit, digitToInt)
import StateTParser
import Control.Monad
pNat :: Parser Int 
pNat = fmap (foldl (\x y -> 10*x + y) 0) pDigits

pDigits :: Parser [Int]
pDigits = many pDigit

pDigit = sat isDigit >>= return . digitToInt

sat :: (Char -> Bool) -> Parser Char
sat p = do
	x <- item
	if p x then return x else mzero

many:: Parser a -> Parser [a]
many p = many1 p `mplus` return []
many1 p = do
	a <- p
	as <- many p
	return (a:as)

char :: Char -> Parser Char
char x = sat (== x)
