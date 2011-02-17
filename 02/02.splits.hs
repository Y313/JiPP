splits l = splits (reverse l) [] where
	splits [] postfix = [([], postfix)]
	splits (h:t) postfix = (reverse(h:t), postfix):(splits t (h:postfix))
