incAll = map (map (+1))

myElemL :: Eq a => a -> [a] -> Bool
myElemL a = foldl (\ b x -> x == a || b) False

myElemR :: Eq a => a -> [a] -> Bool
myElemR a = foldr (\ x b -> x == a || b) False

mapL :: (a -> b) -> [a] -> [b]
mapL f = foldl (\ result x -> result ++ [f(x)]) []

mapR :: (a -> b) -> [a] -> [b]
mapR f = foldl (\ x result -> f(x):result) []
