mytake 0 x = []

mytake n (h:xs) = h:(mytake (n-1) xs)

myhead x = x !! 0

mytail x = x !! (length x - 1)

mydrop 0 x = x

mydrop n (h:xs) = (mydrop (n-1) xs)

myInits [] = [[]]

myInits (h:xs) = []:map ( h: ) (myInits xs)

partitions [] = [] 

partitions (h:xs) = ([],h:xs):map(\(x,y)->(h:x,y)) (partitions xs)

permutations :: [Int] -> [[Int]]
permutations [] = [[]]
permutations l = [x:y | x <- l, y <- permutations [z | z <- l, z /= x]]

sieve [] = []
sieve (h:xs) = h : sieve [x | x <- xs, mod x h /= 0]

splitBy :: Int -> [Int] -> ( [Int], [Int] )
splitBy x [] = ([], [])
splitBy x (h:xs) = 
	let 
			(l, r) = splitBy x xs
		in if h <= x then (h:l, r) else (l, h:r)

nub :: [Int] -> [Int]
nub [] = []
nub (h:xs) = h:nub [x | x <- xs, x /= h]

triads :: Int -> [(Int, Int, Int)]
triads 0 = []
triads n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2, x <= y]

fibo :: Int -> Int
fibo n = fibo' n 1 1 where
				 fibo' 0 a b = a
				 fibo' n a b = fibo'(n-1) b (a+b)

silnia :: Int -> Int
silnia n = sil n 1 where
					 sil 0 a = a
					 sil n a = sil(n-1) a*n

myReverse :: [Int] -> [Int]
myReverse [] = []
myReverse (h:xs) = h:myReverse(xs)

