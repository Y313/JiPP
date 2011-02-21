inits a = [take n a | n <- [0..(length a)]]

inits2 [] = [[]]
inits2 a = a:(inits2 (init a))

inits3 l = fst (foldl (\(a,b) x -> (a ++ [b ++ [x]], b ++ [x])) ([[]], []) l)

inits4 l = scanl (\acc x -> acc ++ [x]) [] l
