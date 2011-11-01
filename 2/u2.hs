{- Aufgabe 1 -}

clockdiff :: (Int, Int) -> (Int, Int) -> (Int, Int)
clockdiff (h1, m1) (h2, m2)	| h1*60+m1 > h2*60+m2	= maketime (-((h1*60+m1)-(h2*60+m2)))
				| otherwise 		= maketime ((h2*60+m2)-(h1*60+m1))


maketime :: Int -> (Int, Int)
maketime mins | mins >= 0 = ((div mins 60), (mod mins 60))
              | otherwise = (-(div (-mins) 60), -(mod (-mins) 60))

{- Aufgabe 2 -}

timeformat :: (Int, Int) -> String
timeformat (h, m) | h > 23 || m > 59 || m < 0 = "ungültig"
		  | h >= 13		 = show (h-12) ++ ":" ++ show m ++ " p.m."
		  | otherwise 		 = show h ++ ":" ++ show m ++ " a.m.h"

timeformatio :: (Int, Int) -> IO()
timeformatio (h, m) = putStr ((timeformat (h,m))++"\n")

{- Aufgabe 3 -}
maketables :: (Int, Int) -> IO()
maketables (a,b) | a < 0 || b > 31 || b < a = putStr "Zu große Zahlen\n"
		 | otherwise 		    = putStr (tablesrender [a..b] 0 (-1) "")

tablesrender :: [Int] -> Int -> Int -> String -> String
tablesrender [xn] i j s | i == 0 && j < 0    = tablesrender [xn] i (j+1) (s++"   |")
			| j == (length [xn]) = tablesrender [xn] (i+1) 0 (s++"\n")
		        | i == 0 && j >= 0   = tablesrender [xn] i (j+1) (s++(getchars ( 3 - ( length( show( [xn]!!j)))) " " "" ) ++ (show ([xn]!!j)))
			| i == 1 && j >= 0   = tablesrender [xn] (i+1) j (s ++ "---+" ++ (getchars (length s) "-" ""))
			| i > 1 && j == 0    = tablesrender [xn] i (j+1) (s ++ (getchars ( 3 - (length( show( [xn]!!j)))) " " "" )  ++ (show ([xn]!!j)) ++ "|" )
			| i > 1 && j > 0     = tablesrender [xn] i (j+1) (s ++ (getchars ( 3 - ( length( show(([xn]!!j)*([xn]!!j))))) " " "" ) ++ (show (([xn]!!j)*([xn]!!j))))
			| otherwise = s

getchars :: Int -> String -> String -> String
getchars 0 c s = s
getchars l c s = getchars (l-1) c (s++c)




