uniq l = uniq l where
	uniq [] = []
	uniq (h:t) = let res = uniq t in
		if elem h t then res else h:res

uniq2 l = uniq l [] where
	uniq [] ignore = []
	uniq (h:t) ignore = let res = uniq t (h:ignore) in
		if elem h ignore then res else h:res
