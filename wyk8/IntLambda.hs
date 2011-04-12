module IntLambda where

infixr 5 :->
data Type = TInt | Type :-> Type 
  deriving (Eq, Show)

type Name = String
data Exp = EInt Int
         | EVar Name
         | ELam Name Type Exp
         | EApp Exp Exp
	deriving (Eq, Show)

-- Przykładowe lambda-termy
type Exp1 = Type -> Exp
type Exp2 = Type -> Exp1
type Exp3 = Type -> Exp2

int :: Type
int = TInt

mkI :: Exp1
mkI a = ELam "x" a $ EVar "x"

mkK :: Exp2
mkK a b = ELam "x" a $ ELam "y" b $ EVar "x"

intK = mkK int int

mkS :: Exp3
mkS a b c = ELam "x" a $ ELam "y" b $ ELam "z" c
      $ EApp 
     (EApp (EVar "x") (EVar "z")) 
     (EApp (EVar "y") (EVar "z")) 

intS = mkS (int:->int:->int) (int:->int) int

-- kombinator omega nie typuje sie w prostym rachunku lambda
mkOmega :: Exp1
mkOmega t = ELam "x" t $ EApp (EVar "x") (EVar "x")

intOmega = mkOmega TInt

-----------------------------------------------------------------
-- Koniec IntLambda
-----------------------------------------------------------------
