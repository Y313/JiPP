data Exp = IntE Int            -- stała całkowita
        | OpE  Op Exp Exp      -- operacja, np e1 + e2
        | VarE String          -- zmienna
        | LetE String Exp Exp  -- let var = e1 in e2

type Op = (Int -> Int -> Int, String) -- (działanie, nazwa)

instance Eq Exp where
	(IntE n1) == (IntE n2) = n1 == n2
	(VarE s1) == (VarE s2) = s1 == s2
	(OpE op1 e1 e2) == (OpE op2 e3 e4) = snd op1 == snd op2 && e1 == e3 && e2 == e4
	(LetE s1 e1 e2) == (LetE s2 e3 e4) = s1 == s2 && e1 == e2 && e3 == e4

instance Show Exp where
	show (IntE n) = show n
	show (VarE s) = show s
	show (OpE op e1 e2) = "(" ++ show e1 ++ snd op ++ show e2 ++ ")"
	show (LetE s e1 e2) = "let "++ s ++ " " ++ show e1 ++ " in " ++ show e2

instance Num Exp where
	a + b = OpE ((+), "+") a b

	a - b = OpE ((-), "-") a b

	a * b = OpE ((*), "*") a b

	negate a = undefined

	abs a = undefined

	signum a = undefined

	fromInteger a = IntE (fromInteger a)
