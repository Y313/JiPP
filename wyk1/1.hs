myhead (x:xs) = x

mytail [a] = []
mytail (x:xs) = mytail xs

infixl 6 +++
[] +++ b = b
(x:xs) +++ b = x:(xs +++ b)

mytake n [] = []
mytake n (x:xs)
	| n > 0 = x:(take (n-1) xs)
	| otherwise = []

mydrop n [] = []
mydrop n (x:xs)
	| n > 0 = mydrop (n-1) xs
	| otherwise = (x:xs)
