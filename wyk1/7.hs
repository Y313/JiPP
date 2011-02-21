fac n = fac' 1 n where
	fac' acc 1 = acc
	fac' acc n = fac' (acc*n) (n-1)

fac2 n = foldl1 (*) [1..n]

fac3 n = product [1..n]

fib n = fib' n 1 1 where
	fib' 0 a b = a
	fib' n a b = fib' (n-1) b (a+b)

reverse' [] = []
reverse' (x:xs) = rev' [] (x:xs) where
	rev' acc [] = acc
	rev' acc (x:xs) = rev' (x:acc) xs
