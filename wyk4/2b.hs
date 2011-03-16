import qualified Data.Map as M
import Control.Monad.Reader
data Exp = IntE Int
	| OpE Op Exp Exp
	| VarE String
	| LetE String Exp Exp -- let var = e1 in e2

type Op = Int -> Int -> Int

evalExp :: Exp -> Int
evalExp expr = runReader (evalExp' expr) M.empty
	where
	evalExp' :: Exp -> Reader (M.Map String Int) Int
	evalExp' (IntE n) = return n
	evalExp' (VarE x) = do
		val <- asks (M.findWithDefault 0 x)
		return val
	evalExp' (LetE x e1 e2) = do
		xval <- evalExp' e1
		val <- local (M.insert x xval) (evalExp' e2)
		return val
	evalExp' (OpE op e1 e2) = do
		l <- evalExp' e1
		r <- evalExp' e2
		return $ op l r

--
----      let x =
----          let y = 5 + 6
----          in y / 5
----      in x * 3
---- 
---- ==>  6
----
test = LetE "x" (LetE "y" (OpE (+) (IntE 5) (IntE 6))
          (OpE div y (IntE 5)))
	  (OpE (*) x (IntE 3))
  where x = VarE "x"
	y = VarE "y"
