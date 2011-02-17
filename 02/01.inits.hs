myInits l = myInitsHelper l []

myInitsHelper [] a = []:a
myInitsHelper l a = myInitsHelper (init l) (l:a)

myInits2 l = myInits2 [] l where
	myInits2 a [] = []:a
	myInits2 a l = myInits2 (l:a) (init l)

myInits3 [] = [[]]
myInits3 (k:t) = []:(map ((:)k) (myInits3 t))

myInits4 [] = [[]]
myInits4 (k:t) = []:dodaj k (myInits4 t) where
	dodaj k [] = []
	dodaj k (l:t) = (k:l):(dodaj k t)
