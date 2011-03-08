data Exp = IntE Int            -- stała całkowita
        | OpE  Op Exp Exp      -- operacja, np e1 + e2
        | VarE String          -- zmienna
        | LetE String Exp Exp  -- let var = e1 in e2
        
type Op = (Int -> Int -> Int, String) -- (działanie, nazwa)

instance Eq Exp where
	(IntE n1) == (IntE n2) = n1 == n2
	(VarE x1) == (VarE x2) = x1 == x2
	(OpE p1 e11 e12) == (OpE p2 e21 e22) = snd p1 == snd p2 && e11 == e21 && e12 == e22
	(LetE x1 e11 e12) == (LetE x2 e21 e22) = (x1==x2 && e11 == e21 && e12 == e22)

instance Show Exp where
	show (IntE n) = show n
	show (VarE x) = show x
	show (OpE p e1 e2) = (show e1) ++ (snd p) ++ (show e2)
	show (LetE v e1 e2) = "let " ++ show v ++ " = " ++ show e1 ++ " in " ++ show e2
	
instance Num Exp where
	a + b = OpE ((+), "+") a b 
	a - b = OpE ((-), "-") a b
	a * b = OpE ((*), "*") a b
	negate a = -a
	abs a = undefined
	signum a = undefined
	fromInteger a = IntE (fromInteger a)

testExp2 :: Exp
testExp2 = (2 - 2) * 3
