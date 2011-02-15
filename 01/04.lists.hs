join [] list = list
join (h:t) list = h:(join t list)

my_take 0 list = []
my_take k [] = []
my_take k (h:t) = h:(my_take (k-1) t)

last 0 list = []
last k [] = []

