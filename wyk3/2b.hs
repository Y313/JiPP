main = do
	putStrLn "Jaki jest Twój ulubiony język programowania?"
	jakiUlubiony <- getLine
	if jakiUlubiony == "Haskell" then putStrLn "Ok!"
				     else main			
