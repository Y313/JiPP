{- a) - funkcja bierze liste list i zwraca liste list z wszyskimi elemenami zwiększonymi o 1 -}
incAll :: [[Int]] -> [[Int]]
incAll list = [map (\x->x+1) x | x<-list]

{- b) silnia i konkatenacja przy pomocy foldl i foldr -}
silnial n = foldl (*) 1 [1..n]
silniar n = foldr (*) 1 [1..n]

concatl :: [[Int]] -> [Int]
concatl list = foldl1 (++) list

concatr :: [[Int]] -> [Int]
concatr list = foldr1 (++) list

{- c) usuwa dublikaty za pomocą filter -}
nubb :: [Int] -> [Int]
nubb [] = []
nubb (h:list) = h:nubb(filter (/=h ) list)

