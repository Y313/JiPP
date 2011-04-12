import IntLambda
import qualified Data.Map as M
import Data.Maybe(fromJust)

type Env = M.Map Name Type

typeOf0 :: Env -> Exp -> Type
typeOf0 env (EInt _) = TInt
typeOf0 env (EVar name) = fromJust $ M.lookup name env
typeOf0 env (ELam name t exp) = (t :-> (typeOf0 (M.insert name t env) exp))
typeOf0 env (EApp e1 e2) =
		case (typeOf0 env e1) of
			t2 :-> t0 | t2 == (typeOf0 env e2) -> t0
			t -> error $ "błędna aplikacja " ++ show e1 ++ " do " ++ show e2

typeOf :: Exp -> Type
typeOf = typeOf0 M.empty 

