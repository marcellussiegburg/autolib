module NFA.Equiv where

-- $Id$

import NFA

import Reporter
import ToDoc
import Util.Fix

import Set
import FiniteMap
import List (partition, tails)
import Maybe (fromMaybe,maybeToList)


type Klassen s = Set (Set s)
type Mappe   s = FiniteMap s Int

type Trenner s = ( s, s, Char )

----------------------------------------------------------------------


toMappe :: Ord s => Klassen s -> Mappe s
toMappe xss = listToFM $ do
    ( k, xs ) <- zip [1 .. ] $ setToList xss
    x <- setToList xs
    return ( x, k )

toKlassen :: Ord s => Mappe s -> Klassen s
toKlassen fm = mkSet $ eltsFM $ addListToFM_C union emptyFM $ do
    ( x, k ) <- fmToList fm
    return ( k, unitSet x )

----------------------------------------------------------------------

refine :: Ord s => (s -> s -> Bool) -> Klassen s -> Klassen s
-- verfeinert äquivalenzklassen
refine eq xss = mkSet $ do xs <- setToList xss 
			   split eq $ setToList xs 

split :: Ord s => (s -> s -> Bool) -> [s] -> [Set s]
split eq [] = []
split eq (x : xs) =
    let (yeah, noh) = partition (eq x) xs
    in	mkSet (x : yeah) : split eq noh


mkTafel :: Ord s => [ Trenner s ] -> Set (s,s) 
-- wer hier vorkommt, ist nicht äquivalent
mkTafel ts = mkSet $ do
    (p,q,c) <- ts
    [(p,q),(q,p)]

anwende :: Ord s => Klassen s -> [ Trenner s ] -> Klassen s
anwende xss ts = 
    let tafel = mkTafel ts
	eq x y = not (elementOf (x,y) tafel)
    in	refine eq xss

----------------------------------------------------------------------

compact :: Ord s => NFA s -> Klassen s -> NFA (Set s)
compact a xss =
    let fm = listToFM $ do
	     xs <- setToList xss
	     x <- setToList xs
	     return ( x, xs )
        fun = fromMaybe (error "NFA.Equiv.compact") . lookupFM fm
    in  amap fun a

----------------------------------------------------------------------

trenner :: Ord s => Set Char -> NFA s -> Klassen s -> [ Trenner s ]
-- das berechnet alle
trenner sigma a xss = do
    let fm = toMappe xss
    xs <- setToList xss
    x : ys <- tails $ setToList xs
    y <- ys
    c <- setToList sigma
    guard $ not $ eqqu c a fm x y
    return ( x, y, c )
    
eqqu :: Ord s => Char -> NFA s -> Mappe s -> (s -> s -> Bool)
eqqu c a fm p q = 
    let x = nachfolger a p c
	y = nachfolger a q c
    in lookupFM fm x == lookupFM fm y

nachfolger :: Ord s => NFA s -> s -> Char -> s
nachfolger a p c = 
    let qs = fromMaybe ( error "NFA.Equiv: nicht in trans" ) 
	   $  lookupFM (trans a) (p,c)
    in	case setToList qs of
	     [ q ] -> q
	     qs	 -> error "NFA.Equiv: nicht genau ein Nachfolger"

-----------------------------------------------------------------------------

check_trenner :: ( Ord s , ToDoc s )
	      => NFA s -> Mappe s -> Trenner s 
	      -> Reporter ()
check_trenner a fm (p,q,c) = do
    let p' = nachfolger a p c
    let q' = nachfolger a q c
    let st p c p' = fsep [ text "f"
			 , parens $ fsep $ punctuate comma
					 [ toDoc p, toDoc c ]
			 , equals, toDoc p' 
			 ]
    let flag = lookupFM fm p' == lookupFM fm q'
    let msg = fsep [ text $ if flag then "falsch :" else "richtig:"
		   , st p c p'
		   , text "und"
		   , st q c q'
		   , text "sind"
		   , if flag then empty else text "nicht"
		   , text "äquivalent"
		   ]
    if flag
       then do reject $ msg
       else do inform $ msg


check_trenners :: ( Ord s , ToDoc s )
	      => NFA s -> Mappe s -> [ Trenner s ]
	      -> Reporter ()
check_trenners a fm ts = mapM_ (check_trenner a fm) ts

----------------------------------------------------------------------

schritt :: ( Ord s , ToDoc s, ToDoc [s] )
	=> Set Char -> NFA s 
	-> Klassen s  -> (Int, [ Trenner s ])
	-> Reporter ( Klassen s )
-- prüfe einen schritt
schritt sigma a xss (k, ts) = do
    let fm = toMappe xss
    inform $ text $ unwords [ "Schritt", show k ]
    inform $ text "Die Äquivalenzklassen sind:" <+> toDoc xss
    inform $ text "Ich prüfe Ihre trennenden Tupel:" <+> toDoc ts

    inform $ text "... sind alle Tupel korrekt?"
    check_trenners a fm ts

    inform $ text "... sind alle für diesen Schritt nötigen Tupel vorhanden?"
    let ts' = trenner sigma a xss
    if isEmptySet (mkTafel ts' `minusSet` mkTafel ts)
       then do inform $ text "Ja." 
	       return $ anwende xss ts
       else do reject $ text "Nein. Sie müssen noch mehr Zustände trennen."

----------------------------------------------------------------------------

equiv :: ( Ord s , ToDoc s, ToDoc [s] )
	=> String
	 -> Set Char -> NFA s 
	-> [[ Trenner s ]]
	-> Reporter (Klassen s)
equiv url sigma a tss = do
    inform $ vcat $ map text 
	          [ "Sie sollen die Äquivalenzklassen"
		  , "für diesen Automaten bestimmen:"
		  , url
		  ]
    foldM ( schritt sigma a ) ( start a ) $ zip [0..] $ tss ++ [[]]


start :: Ord s => NFA s -> Klassen s
start a = mkSet [ finals a, states a `minusSet` finals a ]

zerlege :: Ord s => Set Char -> NFA s -> [Klassen s]
zerlege sigma a = 
    let f xss = anwende xss $ trenner sigma a xss
    in	fixes f $ start a

-------------------------------------------------------------------





