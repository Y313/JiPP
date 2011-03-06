module Dict (Dict, toList, fromList, insert, Dict.lookup) where

data Dict k v = Empty | Node (Dict k v) (k,v) (Dict k v)

-- return dictionary as key-value list
toList :: Dict k v -> [(k,v)]
toList d = toList d [] where
	toList Empty a = a
	toList (Node l v r) a = toList l (v:(toList r a))

-- create dictionary from key-value list
fromList :: Ord k => [(k, v)] -> Dict k v
fromList = foldl (\a (k,v) -> insert k v a) Empty

-- insert/replace element to dictionary
insert :: (Ord k) => k -> v -> Dict k v -> Dict k v
insert k v Empty = Node Empty (k,v) Empty
insert k v (Node l (nk, nv) r)
	| k == nk = Node l (nk, v) r
	| k < nk = Node (insert k v l) (nk, nv) r
	| otherwise = Node l (nk, nv) (insert k v r)

-- search element in dictionary by key
lookup :: (Ord k) => k -> Dict k v -> Maybe v
lookup _ Empty = Nothing
lookup n (Node l (k,v) r)
	| n == k = Just v
	| n < k = Dict.lookup n l
	| otherwise = Dict.lookup n r
