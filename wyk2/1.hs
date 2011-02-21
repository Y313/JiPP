incAll :: [[Int]] -> [[Int]]
incAll l = map (map succ) l

fact n = foldl1 (*) [1..n]
concat' l = foldl1 (++) l

nub [] = []
nub (x:xs) = x:nub (filter ((/=) x) xs)
