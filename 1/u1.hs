module Main where

import Data.List

main::IO()
main = undefined



{- Aufgabe 1 -}

tri::Float->Float->Float->Bool
tri x y z = last (sort ([x,y,z])) < sum( init( sort( [x,y,z])))


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
endwert2 kapital zinssatz 0 = kapital
endwert2 kapital zinssatz jahre = endwert2 ((zinsen kapital zinssatz)+kapital) (zinssatz) (jahre-1)

{-c)-}

zinsenalt::Float->Float->Float
zinsenalt kapital zinssatz = kapital * zinssatz / 100

{- Ja, diese berechnung ist äquivalent -}


{- Aufgabe 5 -}

zero:: Float->Float
zero x = (1/x)*x-1


{- Aufgabe 6 -}


d (a, b, c)= (a + b + c)/3


vergleich (p, q)=
	  if p > q then 1
	  else 0


anzahl (a, b, c) = ( vergleich (a, d(a, b, c)) + vergleich (b, d(a,b, c))+vergleich (c, d(a,b,c)))


