{- Aufgabe 12 


f x y =
	let n = 3 in take n (g y) ++ take n (g x)
		where g x = take n xys
			where
				xys = [x] ++ yxs
				yxs = [y] ++ xys
			n = 10
-}

{- Aufgabe 13 -}

wsmthanav :: [Integer] -> Int
wsmthanav xs = smthanav xs (length xs) 0

smthanav :: [Integer] -> Int -> Int  -> Int
smthanav xs 0 r = r
smthanav xs i r = smthanav xs (i-1) (if (fromInteger(xs!!(i-1))) < ((fromInteger (sum xs)) /  (fromIntegral (length xs))) then (r+1) else r)

{- Aufgabe 14 -}

-- type Artikel = (Eq s)=> [(s, Float)]

-- preis :: Artikel -> Artikel -> (Float,[String])	
-- preis p e = (1, [(filter (\x -> fst (elem p) == fst(elem x))) e])s :: Artikel -> Artikel -> (Float,[String]) 

preisliste :: [(String, Float)]
preisliste = [("Kartoffeln", 15.00), ("Mikrocontroller", 0.05), ("Ekliger Nudeltopf", 10000), ("Pastinaken", 4.00), ("Tierkot", 0.50)]
einkaufsliste :: [(String, Float)]
einkaufsliste = [("Kartoffeln", 3), ("Mikrocontroller", 16000), ("Ekliger Nudeltopf", 0), ("Pastinaken", 12), ("Schokoladen", 15000)]

preis:: [(String, Float)] -> [(String, Float)] -> (Float,[String]) 
preis p e = ((sum (inListe p e)),(nichtInListe p e))  

inListe:: [(String, Float)] -> [(String, Float)] -> [Float] 
inListe p e= [(head (produktPreis p (fst x)))*snd x | x<-e, produktPreis p (fst x) /= []]

nichtInListe:: [(String, Float)] ->[(String, Float)] -> [String] 
nichtInListe p e = [fst x|x<-e, produktPreis p (fst x) == []]  

produktPreis:: [(String, Float)] -> String -> [Float] 
produktPreis p e = [snd x | x<-p ,(fst x)==e]

{- Aufgabe 15 -}

iter :: (Num a, Ord a) => a -> (x -> x) -> x -> x
iter n f x
	| n==0 = x
	| n>0 = f (iter (n-1) f x)

-- iters :: (Num a, Ord a) => a -> (x -> x)
iters n f
	| n==0 = f 
	| n>0 = iter (n-1) f


{- Aufgabe 16 -}
minLog :: Float
minLog = head [x| x <- [0.0..], (logBase 2 x) >= 5]

minLog2 :: Float
minLog2 = head [x| x <- [0.0..], (iter 3 (logBase 2) x) >= 2]

{- Aufgabe 17 -}

potenz :: Float -> Float -> Float
potenz x n = iter n (x*) 1

turm :: Double -> Double -> Double
turm x k = iter k (potenz x) x


