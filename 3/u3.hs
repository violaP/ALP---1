{- Übung 3 -}

{- Aufgabe 13 -}

wsmthanav :: [Integer] -> Int
wsmthanav xs = smthanav xs (length xs) 0

smthanav :: [Integer] -> Int -> Int  -> Int
smthanav xs 0 r = r
smthanav xs i r = smthanav xs (i-1) (if (fromInteger(xs!!(i-1))) < ((fromInteger (sum xs)) /  (fromIntegral (length xs))) then (r+1) else r)