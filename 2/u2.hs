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
		 | otherwise 		    = putStr (tablesrender [a..b] 0 (0) "")

tablesrender :: [Int] -> Int -> Int -> String -> String
tablesrender xn i j s | i == 0 && j == 0      = tablesrender xn i (j+1) (s++"   |")
			| i >= (length xn)+2 = s
			| j >= (length xn)+1 = tablesrender xn (i+1) 0 (s++"\n")
		        | i == 0 && j >= 0   = tablesrender xn i (j+1) (s++(getchars ( 3 - ( length( show( xn!!(j-1))))) " " "" ) ++ (show (xn!!(j-1))))
			| i == 1 && j >= 0   = tablesrender xn (i+1) j (s ++ "---+" ++ (getchars (length s) "-" "") ++ "\n" )
			| i > 1 && j == 0    = tablesrender xn i (j+1) (s ++ (getchars ( 3 - (length( show( xn!!(i-2))))) " " "" )  ++ (show (xn!!(i-2))) ++ "|" )
			| i > 1 && j > 0     = tablesrender xn i (j+1) (s ++ (getchars ( 3 - ( length( show((xn!!(i-2))*(xn!!(j-1)))))) " " "" ) ++ (show ((xn!!(i-2))*(xn!!(j-1)))))


getchars :: Int -> String -> String -> String
getchars 0 c s = s
getchars l c s = getchars (l-1) c (s++c)

{- Aufgabe 4 -}

test x y z 	| x <= y 	= True
		| y <= z 	= False
		| otherwise	= x<z


{- Aufgabe 5 -}

-- a)

ndivw :: Int -> [Int]
ndivw n = ndivs n (div n 2) []

ndivs :: Int -> Int -> [Int] -> [Int]
ndivs n 1 xs = (1:xs)
ndivs n c xs = ndivs n (c-1) (if ((mod n c) == 0) then (c:xs) else xs)

-- b)

btsumdivs :: Int -> Bool
btsumdivs n = sum (ndivw n) > n 

listbtsums :: Int -> [Int] -> [Int]
listbtsums 1 xs = xs
listbtsums n xs = listbtsums (n-1) (if (btsumdivs(n)) then (n:xs) else xs)

-- c)

eqsumdivs :: Int -> Bool
eqsumdivs n = sum (ndivw n) == n

listeqsums :: Int -> [Int] -> [Int]
listeqsums 1 xs = xs
listeqsums n xs = listeqsums (n-1) (if (eqsumdivs(n)) then (n:xs) else xs)
