module Autolib.NFTA.Trim

( prods, precs
, trim
) 

where

import Autolib.Set
import Autolib.FiniteMap

import Autolib.NFTA.Type
import Autolib.NFTA.Ops
import Autolib.Schichten
import qualified Autolib.Relation as Relation

-- | producers: those that ->> leaves

prods :: NFTAC c a 
      => Relation.Type a (c, [a])
      -> Set a
prods tr =
    let -- from list of states to set of states
        rel = addListToFM_C union emptyFM $ do
             (p, (c, qs)) <- Relation.pairs tr
	     return ( qs, unitSet p )
	-- find all states that are mapped from []
        -- then remove these from all lhss
	handle rel =
	    let here = lookupset rel []
                fresh p =  not $ elementOf p here
		next = addListToFM_C union emptyFM $ do
		       ( ps, qs ) <- fmToList rel
		       return ( filter fresh ps, sfilter fresh qs )
	    in  if isEmptySet here then [] else here : handle next
    in
        unionManySets $ handle rel
	
------------------------------------------------------------------------

-- | produceds: those that start ->> .

precs :: NFTAC c a 
      => Relation.Type a (c, [a]) 
      -> Set a 
      -> Set a
precs tr starts =
    let	h p = mkSet $ do
            (c, qs) <- setToList $ Relation.images tr p
            qs
    in	mkSet $ bfs' h starts

----------------------------------------------------------

-- | keep only those states that produce leaves
-- and that are reachable from the start
trim :: NFTAC c a
       => NFTA c a -> NFTA c a
trim a1 =
    let	
	qs = prods $ trans a1
	a3 = statefilter (`elementOf` qs) a1
	ps = precs (trans a3) (finals a3)
	e5 = statefilter (`elementOf` ps) a3
    in
	e5

