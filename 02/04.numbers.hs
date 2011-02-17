power n = power n 1 where
	power 1 multiply = multiply
	power a multiply = power (a-1) (a * multiply)

fib n = fib n 0 1 where
	fib 0 f1 f2 = f1
	fib a f1 f2 = fib (a-1) f2 (f1 + f2)
