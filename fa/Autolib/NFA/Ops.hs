{-# OPTIONS -fglasgow-exts #-}

module Autolib.NFA.Ops where

import Autolib.NFA.Type hiding ( cross, union )

import qualified Autolib.Set  as Sets
import Autolib.FiniteMap

import Autolib.NFA.Trim
import Autolib.NFA.Basic
import Autolib.NFA.Normalize
import Autolib.Letters

import Control.Monad (guard)

import Autolib.ToDoc ( ToDoc, toDoc )


normalize_cross :: (NFAC c s, NFAC c t, NFAC c Int)
      => NFA c s -> NFA c t -> NFA c Int
normalize_cross a b = normalize $ cross a b

-- | cross product construction
-- note: final states are undefined
cross :: (NFAC c s, NFAC c t, NFAC c (s, t))
      => NFA c s -> NFA c t -> NFA c (s, t)
cross a b =
    NFA { nfa_info = funni "cross" [ info a, info a ]
	, alphabet = Sets.intersect (letters a) (letters b)
	, states = Sets.cross (states a) (states b)
	, starts = Sets.cross (starts a) (starts b)
	, finals = error "cross.finals unspecified"
	, trans = addListToFM_C Sets.union emptyFM $ do 
	     ((pa, ca), qas) <- ltrans a
	     ((pb, cb), qbs) <- ltrans b
	     guard $ ca == cb
	     return (((pa, pb), ca), Sets.cross qas qbs)	
	}
  

normalize_union :: (NFAC c s, NFAC c t, NFAC c Int)
      => NFA c s -> NFA c t -> NFA c Int
normalize_union a b = normalize $ union a b

-- | union
union :: (NFAC c s, NFAC c t, NFAC c (Either s t))
      => NFA c s -> NFA c t -> NFA c (Either s t)
union a b = 
    let a' = statemap Left a
	b' = statemap Right b
    in	NFA { nfa_info = funni "union" [ info a, info b ]
	    , alphabet = Sets.union ( letters a ) ( letters b )
	    , states = states a' `Sets.union` states b'
	    , starts = starts a' `Sets.union` starts b'
	    , finals = finals a' `Sets.union` finals b'
	    , trans = plusFM_C (error "union") (trans a') (trans b')
	    }

normalize_intersection :: (NFAC c s, NFAC c t, NFAC c Int)
             => NFA c s -> NFA c t -> NFA c Int
normalize_intersection a b = normalize $ intersection a b

-- | intersection
intersection :: (NFAC c s, NFAC c t, NFAC c (s, t))
             => NFA c s -> NFA c t -> NFA c (s, t)
intersection a b = 
    let c = cross a b
	ok (p, q) =  Sets.elementOf p (finals a)	
		  && Sets.elementOf q (finals b)	
    in	trim $ c { nfa_info = funni "intersection" [ info a, info b ]
		 , finals = Sets.sfilter ok $ states c 
		 }


-- | addiere neuen Startzustand,
-- in den keine Pfeile hineinführen
punkt :: (NFAC c s )
      => s -> NFA c s -> NFA c s
punkt t a = 
    a { nfa_info = funni "punkt" [ toDoc t, info a ]
      , alphabet = letters a
      , states = Sets.union (states a) (Sets.unitSet t)
      , starts = Sets.unitSet t
      , finals = if not $ Sets.isEmptySet $ Sets.intersect (starts a) (finals a) 
		 then Sets.union (finals a) (Sets.unitSet t)
		 else            finals a
      , trans = addListToFM_C Sets.union (trans a)
	      $ do ((p, c), qs) <- ltrans a
		   guard $ Sets.elementOf p (starts a)
		   return ((t, c), qs)
      }

-- | get new state (inefficient, looks at all states first)
neu :: NFAC c Int
    => NFA c Int -> Int
neu a = 1 + maximum (0 : Sets.setToList (states a))

-- | concatenation
dot :: NFAC c Int 
    => NFA c Int -> NFA c Int -> NFA c Int
dot a b =
    let n = neu a
	b' = statemap (+ n) b
    in	trim $
	NFA { nfa_info = funni "dot" [ info a, info b ]
	    , alphabet = Sets.union ( letters a ) ( letters b )
	    , states = Sets.union (states a) (states b')
	    , starts = if ceps a
		       then Sets.union (starts a) (starts b')
		       else starts a
	    , finals = if ceps b' 
		       then Sets.union (finals a) (finals b')
		       else finals b'
	    , trans = addListToFM_C Sets.union
	          (plusFM_C (error "dot") (trans a) (trans b'))
		  $ do ((p, c), qs) <- ltrans b'
		       guard $ Sets.elementOf p $ starts b'
		       f <- lfinals a
		       return ((f, c), qs)
	    }

power :: (ToDoc [c], NFAC c Int) 
      => Integer -> NFA c Int -> NFA c Int
power e a = ( foldr dot epsilon $ take (fromInteger e) $ repeat a )
	    { nfa_info = funni "power" [ toDoc e, info a ] }

plus :: NFAC c Int => NFA c Int -> NFA c Int
plus a = 
    let n = neu a
	b = punkt n a
    in	trim $ b { nfa_info = funni "plus" [ info a ]

		 , trans = addListToFM_C Sets.union (trans b)
		  $ do ((p, c), qs) <- ltrans b
		       f <- lfinals b
		       guard $ Sets.elementOf p $ starts b
		       return ((f, c), qs)
		 }

star :: NFAC c Int => NFA c Int -> NFA c Int

-- BUG: star(empty) = {epsilon} aber star(empty)={} wurde hier berechnet 
-- FIX: Eine Anfrage hier reinbauen
-- Problem: Die Rückverfolgung über plus führt auf trim und immer
-- tiefer, deswegen als einfache und schnelle Abhilfe nur hier diesen
-- Fall betrachten. Voraussetzung: Der Rest haut hin...

star a 
    | isEmptySet (finals b) = epsilon
    | otherwise             = b { nfa_info = funni "star" [ info a ]
				, finals = Sets.union (finals b) (starts b) 
				}
    where b = plus a
