-- $Id$

module NFA.Shortest where

import NFA.Type
import NFA.Trim

import Sets
import FiniteMap

import Letters

import ToDoc
import Monad (guard)
import List (nub)

type State c s  = ([c], s)

succs :: (NFAC c s, Ord (Set (State c s))) 
      => NFA c s -> State c s -> Set ( State c s )
succs a (w, p) = mkSet $ do
    c <- setToList $ letters a -- sehr dubios, kostet zeit
    q <- setToList $ lookupWithDefaultFM (trans a) emptySet (p, c)
    return ( w ++ [c] -- na und das erst
	   , q
	   )

computations :: (NFAC c s, Ord (Set (State c s)) )
	     => NFA c s -> [ Set ( State c s ) ]
computations a = comps ( smap ( \ p -> ( [], p ) ) $ starts a ) where
    comps todo = todo :
	  let next = unionManySets $ setToList $ smap (succs a) todo
	  in  if isEmptySet next then [] else comps next

accepted :: (NFAC c s, Ord (Set (State c s))) 
	 => NFA c s -> [ [c] ]
accepted a = 
    let ws = do
	   wqs <- computations a
	   return $ nub $ do
	       ( w, q ) <- setToList wqs
	       guard $ q `elementOf` finals a
	       return w
        n = cardinality $ states a
	(short, long) = splitAt n ws
	(mid, rest)   = splitAt n long
	
	here  = concat short
	there = concat mid
	after = concat rest

    in	here ++ if null here then [] 
		else there ++ if null there then []
			      else after

shortest :: (NFAC c s, Ord (Set (State c s)) )
	     => NFA c s -> [ [c] ]
shortest a = 
    let b = trim a
    in	case accepted b of
	     [] -> []
	     rest @ ( w : _ ) -> takeWhile (\ u -> length u == length w) rest

------------------------------------------------------------------------------

reachables :: (NFAC c s, Ord (Set (State c s)) )
	     => NFA c s -> [c] -> Set s
reachables a w = reachables_from a w ( starts a )


reachables_from :: (NFAC c s,  Ord (Set (State c s)) )
	     => NFA c s -> [c] -> Set s -> Set s
reachables_from a [] ps = ps
reachables_from a ( c : cs ) ps = 
    reachables_from a cs $ mkSet $ do
	     p <- setToList ps
	     setToList $ lookupWithDefaultFM (trans a) emptySet (p, c)

is_accepted :: (NFAC c s, Ord (Set (State c s)) )
	     => NFA c s -> [c] -> Bool
is_accepted a w = 
    not $ isEmptySet $ intersect ( reachables a w ) ( finals a )

-------------------------------------------------------------------

present :: (NFAC c s, Ord (Set (State c s)) 
	 , ToDoc [c])
	     => NFA c s -> Doc
present a =
    ( text "Zur Sprache" <+> info a )
    $+$ nest 4 (  vcat [ text "gehören unter anderem diese Wörter:"
		       , toDoc $ take 20 $ accepted a
		       ] )


