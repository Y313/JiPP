permutations [] = []
permutations [a] = [[a]]
permutations l = let n = length l
		 in let perm m = (take n $ drop m $ cycle l)
		 in foldl (\acc x -> acc++(map ((:) (head $ perm x)) (permutations (tail $ perm x)))) [] [0..n-1]


permutations2 [] = [[]]
permutations2 l = [x:y | x <- l, y <- permutations [z | z <- l, z /= x]]
