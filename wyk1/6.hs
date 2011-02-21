triads n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

triadsOrdered n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2, x < y && y < z]

triadsHelper [] = []
triadsHelper (t@(a,b,c):ts) = t:(triadsHelper [ (d,e,f) | (d,e,f) <- ts, not (mod d a == 0 && mod e b == 0 && mod f c == 0)])
triadsNonTrivial n = triadsHelper $ triadsOrdered n


primes = [ p | p <- sieve [2..]]
sieve l@(x:xs) = x:[ x | x <- sieve xs, mod x (head l) /= 0]
