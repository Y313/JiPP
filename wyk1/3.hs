partitions [] = [([],[])]
partitions (x:xs) = ([],(x:xs)):[(x:z,v) | (z,v) <- partitions xs]
