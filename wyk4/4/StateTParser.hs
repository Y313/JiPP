module StateTParser (Parser, runParser, item) where
import Control.Monad.State

type Parser a = (StateT [Char] Maybe) a
pmap :: (a -> b) -> Parser a -> Parser b
pmap = liftM

runParser :: StateT s m a -> s -> m (a, s)
runParser = runStateT
item :: Parser Char
item = do
	input <- get
	case input of
		[] -> mzero
		(x:xs) -> put xs >> return x
