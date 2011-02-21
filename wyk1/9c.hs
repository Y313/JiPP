compose :: (Ord t, Num t, Enum c) => (c -> c) -> t -> c -> c
compose f 0 = (\x -> x)
compose f 1 = f
compose f n = f . (compose f (n-1))

showInt a = if a < 0 then  '-':(reverse $ showInt' (-a)) else
			        reverse $ showInt' a  where
		    showInt' n
			| n < 10 = [(compose succ n) '0']
			| otherwise = showInt' (mod n 10) ++ showInt' (div n 10)

showLst :: (a -> String) -> [a] -> String
showLst s a = '[':(showLst' a) where
		showLst' [] = [']']
		showLst' [a] = s a++showLst' []
		showLst' (x:xs) = s x++',':showLst' xs

showIntLst l = showLst showInt l
