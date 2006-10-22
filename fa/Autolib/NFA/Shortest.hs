--  $Id$

module Autolib.NFA.Shortest where

import Autolib.NFA.Type
import Autolib.NFA.Trim

import Autolib.Set
import Autolib.FiniteMap

import Autolib.Letters
import Autolib.Util.Hide
import Autolib.Schichten

import Autolib.ToDoc
import Control.Monad (guard)
import Data.List (nub)

-- | watch out: word is in reverse order
type State c s  = (s, [c])

succs :: (NFAC c s, Ord (Set (State c s))) 
      => NFA c s -> State c s -> Set ( State c s )
succs a (p, w) = mkSet $ do
    c <- setToList $ alphabet a
    q <- setToList $ lookupset (trans a) (p, c)
    return ( q
	   , c : w 
	   )

computations :: (NFAC c s, Ord (Set (State c s)) )
	     => NFA c s -> [ Set ( State c s ) ]
computations a = comps ( smap ( \ p -> ( p, [] ) ) $ starts a ) where
    comps todo = todo :
	  let next = unionManySets $ setToList $ smap (succs a) todo
	  in  if isEmptySet next then [] else comps next

-- | (infinite) list of accepted words
-- does correctly output empty list
-- iff language is empty
accepted :: (NFAC c s, Ord (Set (State c s))) 
	 => NFA c s -> [ [c] ]
accepted a = 
    let ws = do
	   wqs <- computations a
	   return $ nub $ do
	       ( q, rw ) <- setToList wqs
	       guard $ q `elementOf` finals a
	       return $ reverse rw
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

-- | compute some of the shortest accepted words
some_shortest :: NFAC c s
	     => NFA c s -> [ [c] ]
some_shortest a = do
    let succ (p, hw) = mkSet $ do
	     c <- setToList $ alphabet a
	     q <- setToList $ lookupset (trans a) (p, c)
	     return (q, Hide $ c : unHide hw)
    level <- schichten' succ 
	     $ smap ( \ p -> (p, Hide []))
	     $ starts a 
    let ws = do 
	     (q, hw) <- setToList level
	     guard $ elementOf q (finals a)
	     return $ reverse $ unHide hw
    guard $ not $ null ws
    ws

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


