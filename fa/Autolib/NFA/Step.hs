-- | apply (one-step) rewriting to automata

module Autolib.NFA.Step where

--  $Id$

import Autolib.NFA.Type
import Autolib.NFA.Trim
import Autolib.NFA.Shortest
import Autolib.NFA.Normalize
import Autolib.NFA.Epsilon

import Autolib.Set
import Autolib.FiniteMap

import Control.Monad (guard)
import Autolib.ToDoc
import Autolib.Hash
import Autolib.Reader

type Rule c = ([c], [c])
type SRS c = [ Rule c ]

letters :: Ord c => SRS c -> Set c
letters srs = mkSet $ do (l,r) <- srs ; l ++ r

data Zustand a = Links ! a
	 | Mitte ! (Int, Int) -- ^ regelnr, buchstabenpos
	 | Rechts ! a
     deriving ( Eq, Ord, Show, Read )

instance Hash a => Hash ( Zustand a ) where
    hash ( Links q ) = hash ( 23 :: Int , q ) 
    hash ( Mitte ( p, q ) ) = hash ( 65 :: Int, p, q )
    hash ( Rechts q ) = hash ( 32 :: Int , q ) 

instance Reader ( Zustand a ) -- dummy

instance Show a => ToDoc (Zustand a) where
    toDoc = text . show

instance (Show a, NFAC c a) => NFAC c (Zustand a)

-- | genau einen rewrite-step anwenden
step :: ( Show a, NFAC c a, NFAC c Int, ToDoc [c] )
     => SRS c -> NFA c a -> NFA c Int
step srs a = informed ( funni "step"  [ toDoc srs, info a ] )
	   $ trim $ normalize $ raw_step srs a


-- | viele schritte, oder auch gar keinen
-- benutzt bei substitutionen
multi_step ::  ( Show a, NFAC c a, NFAC c Int , ToDoc [c] )
     => SRS c -> NFA c a -> NFA c Int
multi_step srs a = informed ( funni "multi_step"  [ toDoc srs, info a ] )
		 $ trim 
	         $ normalize 
	         $ klatsch $ raw_step srs a

klatsch ::  ( Show a, NFAC c a, NFAC c ( Zustand a ) )
        => NFA c ( Zustand a) -> NFA c ( Zustand a )
klatsch = statemap $ \ p -> case p of
    Rechts q -> Links q
    _	     -> p	



-- | algorithmus: für jeden redex zur regel (l,r)
-- (= pfad in a von zustand p nach q mit beschriftung l)
-- konstruiere pfad von (Left p) nach (Right q)
-- über [ Mitte (i,1), .. ] mit Beschriftung r
-- bei r = leer ist das ein epsilon-übergang


raw_step ::   ( Show a, NFAC c a, NFAC c ( Zustand a ), ToDoc [c] )
         => SRS c -> NFA c a -> NFA c ( Zustand a )
-- genau einen Reduktionsschritt anwenden
raw_step srs a = 
    let links  = statemap Links  a
	rechts = statemap Rechts a
	beide  = NFA { nfa_info = funni "raw_step" [ info srs, info a ]
		     , alphabet = union (alphabet a) (letters srs)
		     , states = union ( states links ) ( states rechts )
		     , starts = starts links
		     , finals = finals rechts
		     , trans  = plusFM_C ( error "Step.beide" ) 
		                         ( trans rechts ) ( trans links )
		     }
        bridges = do
	    ( l, r ) <- srs
	    ( p, q ) <- pairs a l
	    return ( p, r, q )
	epsilons = do
	    ( p, [], q ) <- bridges
	    return ( Links p, Rechts q )
	arrows = do
	    ( k, ( p, r, q )) <- zip [0..] bridges
	    let zs =  [ Links p ] 
		   ++ [ Mitte (k,i) | i <- [1 .. length r - 1] ] 
		   ++ [ Rechts q ]
	    (c, ( s, t )) <- zip r $ zip zs ( tail zs )
	    return ( s, c, t )
	mitte = mkSet $ do ( s, c, t ) <- arrows ; [ s, t ]
            
        b = beide { states = union ( states beide ) mitte
		  , trans = plusFM_C union ( trans beide ) $ collect arrows 
		  }

    in  foldl add_epsilon b epsilons

-- | alle (p,q), für die p ->> q mit w beschriftet ist.
pairs :: NFAC c a => NFA c a -> [c] -> [(a,a)]
pairs a w = do
    p <- lstates a
    pairs_from a w p

pairs_from a w p = do
    q <- setToList $ reachables ( a { starts = unitSet p } ) w
    return ( p, q )

paths :: NFAC c a => NFA c a -> [c] -> a -> [[(a,c,a)]]
paths a [] p = return []
paths a (c : cs) p = do
    q <- setToList $ lookupset (trans a) (p, c)
    rest <- paths a cs q
    return $ (p,c,q) : rest

type Bridge c a = ( a, [c], a )

-- | alle redexes im automaten
redexes :: NFAC c a
	=> SRS c -> NFA c a
	-> [ Bridge c a ]
redexes srs a = do
    (l, r) <- srs
    (p, q) <- pairs a l
    return (p, l, q)

-- |  all redex matches that have no reduct match
uncovered :: NFAC c a
	     => SRS c -> NFA c a
	     -> [ ( a, Rule c, a )  ]
uncovered srs a = do
    rule @ (l, r) <- srs
    (p, q) <- pairs a l
    guard $ not $ or $ do
        (_, q') <- pairs_from a r p
	return $ q == q'
    return (p, rule, q)
