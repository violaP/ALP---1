module Main where
import Data.List

main::IO()
main = undefined



{- Aufgabe 1 -}



tri::Float->Float->Float->Bool
tri x y z = maximum [x,y,z] < sum( init( sort( [x,y,z])))


{- Aufgabe 2 

c)

Wenn (nur) der Dividend negativ ist, liefert mod immer postitive Reste. Haskell geht hier so vor, dass so oft durch den Divisor geteilt wird bis der "Rest" positiv wird.

Div verhält sich in diesem Fall äquivalent, da es eine Gegenoperation darstellen soll. Ergo ist mit "Überstand" also um eins verringert (also im Betrag erhöht).

-}

{- Aufgabe 3 -}

inttest:: Int
inttest = 200000000000000000000000000000000000
{-

a)

Da "Int" als höchsten Wert die Maschinenwortgröße hat, wird falls ein höherer Wert gespeichert werden soll zunächst das Sign-Bit überschrieben und damit entsteht eine negative Zahl.


b) 

Die minimale Größe eines Int beträgt -2^31, da im Zweierkomplement die Null nur einmal dargestellt wird. Somit gewinnt man eine Zahl im negativen Bereich.

-}

{- Aufgabe 4 -}

zinsen::Float->Float->Float
zinsen kapital zinssatz = kapital * zinssatz * 0.01

{-a)-}

endwert::Float->Float->Float->Float
endwert kapital zinssatz jahre = jahre*(zinsen kapital zinssatz) + kapital

{-b)-}

endwert2::Float->Float->Float->Float
endwert2 kapital zinssatz jahre = (zinsen kapital zinssatz)**jahre+kapital

endwertrec::Float->Float->Float->Float
endwertrec kapital zinssatz 0 = kapital
endwertrec kapital zinssatz jahre = endwert2 ((zinsen kapital zinssatz)+kapital) (zinssatz) (jahre-1)


{-c)-}

zinsenalt::Float->Float->Float
zinsenalt kapital zinssatz = kapital * zinssatz / 100

{- Ja, diese berechnung ist äquivalent -}


{- Aufgabe 5 -}

zero:: Float->Float
zero x = (1/x)*x-1

{- 

Übergibt man als Argument eine 1.0e-39 kommt anstatt von 0, Infinity als Ergebnis. Das liegt daran, dass die Zahl nicht verarbeitet werden kann, aber nicht null ist.

-}


{- Aufgabe 6 -}



d ::(Integral x) => x -> x -> x -> Float
d a b c = fromIntegral(a + b + c)/3

vergleich::(Integral a, Ord b, Num b) => a -> b -> Int
vergleich p q =
	  if (fromIntegral p) > q then 1
	  else 0

anzahl:: Int -> Int -> Int -> Int
anzahl a b c = (vergleich a (d a b c)) + (vergleich b (d a b c)) + (vergleich c (d a b c))

{- alternative solution by marian -}

mean :: [Int] -> Float
mean l = fromIntegral (sum l) / fromIntegral (length l)

gtmean :: [Int] -> [Int]
gtmean l = filter (\x -> fromIntegral x > mean l) l

gtmeanc :: [Int] -> Int
gtmeanc = length.gtmean

gtmeanc3 :: Int -> Int -> Int -> Int
gtmeanc3 x y z = gtmeanc [x,y,z]
