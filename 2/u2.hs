{- Aufgabe 1 -}

clockdiff :: (Int, Int) -> (Int, Int) -> (Int, Int)
clockdiff (h1, m1) (h2, m2)	| h1*60+m1 > h2*60+m2	= maketime (-((h1*60+m1)-(h2*60+m2)))
				| otherwise 		= maketime ((h2*60+m2)-(h1*60+m1))


maketime :: Int -> (Int, Int)
maketime mins | mins >= 0 = ((div mins 60), (mod mins 60))
              | otherwise = (-(div (-mins) 60), -(mod (-mins) 60))

{- Aufgabe 2 -}

timeformat :: (Int, Int) -> String
timeformat (h, m) | h > 23 || m > 59 || m < 0 = "ungueltig"
		  | h >= 13		 = show (h-12) ++ ":" ++ show m ++ " p.m."
		  | otherwise 		 = show h ++ ":" ++ show m ++ " a.m."
