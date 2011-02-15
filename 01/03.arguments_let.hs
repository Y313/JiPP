head1 [] = error "empty"
head1 (h:_) = h

head2 list = if list == [] then error "empty" else let h:_ = list in h
head3 list = if null list then error "empty" else let h:_ = list in h
