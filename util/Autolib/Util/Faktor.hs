module Autolib.Util.Faktor 

( zerlege 
, primfaktoren
)


where


type Zerlegung = [(Integer, Int)]
-- in Primzahlpotenzen

zerlege :: Integer -> Zerlegung
zerlege n | n <= 0 = error "Util.Zerlegung.zerlege"
zerlege n = filter ( \ (p,e) -> e > 0 ) 
	  $ helper 2 n 

helper p 1 = []
helper p n |  p*p > n  = [ (n,1) ]
helper p n = 
    let nrs = iterate ( \ (n, r) -> divMod n p ) ( n, 0 )
	pre = takeWhile ( \ (n,r) -> r == 0 ) nrs 
	(m, s) = last pre
	d = if p == 2 then 1 
	    else if 1 == p `mod` 6 then 4
	    else 2
    in ( p, length pre - 1 ) : helper (p+d) m

primfaktoren :: Integer -> [ Integer ]
primfaktoren = map fst . zerlege