-- | powerset construction

module Autolib.NFTA.Det where

import Autolib.NFTA.Type

import Autolib.Set
import Autolib.Informed
import Autolib.ToDoc
import qualified Autolib.Relation as Relation

import Autolib.FiniteMap
import Control.Monad


-- | look up the (set of) predec. of a term (whose comps. are sets)
pick :: ( NFTAC c a , NFTAC c (Set a) )
     => FiniteMap (c, [a]) (Set a) -> (c, [Set a]) -> Set a
pick m (c, qss) = mkSet $ do
    args <- mapM setToList qss
    setToList $ lookupset m (c, args)


phull :: ( NFTAC c a )
      => Set c
      -> FiniteMap (c, [a]) (Set a)	-- original map
	-> Set (Set a) 
	-> Set (Set a) 		-- known/unknown
	-> [(Set a, (c, [Set a]))] -- input
	-> (Set (Set a), [(Set a, (c, [Set a]))] ) -- output

phull tcons m known unknown rels | isEmptySet unknown = (known, rels)
phull tcons m known unknown rels =
    let	ps = do
            tc <- setToList tcons
            let n = arity tc
	    a <- packs n (n-1) (setToList known) (setToList unknown)
	    let t = ( tc, a )
            let p = pick m t -- new relations
	    -- guard $ not $ isEmptySet p -- sink state?
	    return ( p, t )		
	gs = mkSet $ map fst ps 		-- new sets
	ks = known `union` unknown		-- they are no longer unknown
	ns = gs `minusSet` ks			-- these are brand new
	rs = ps ++ rels
    in	phull tcons m ks ns rs

-- | built lists with n elements from xs ++ ys
-- where at most k elements are from xs
packs :: Int -> Int ->  [a] -> [a] -> [[a]]
packs n k xs ys
    | 0 == n = return []
    | otherwise =
        do  guard $ k > 0 && n >= k
            x <- xs
            rest <- packs (pred n) (pred k) xs ys
	    return $ x : rest
     ++ do  guard $ n > 0
	    x <- ys
	    rest <- packs (pred n)        k xs ys
	    return $ x : rest

xform :: NFTAC c s 
      => Relation.Type s (c,[s])
      -> FiniteMap (c, [s]) (Set s)
xform tr = addListToFM_C union emptyFM $ do
            (p, (c, qs)) <- Relation.pairs tr
            return ((c, qs), unitSet p)

det :: ( NFTAC c s , NFTAC c (Set s) )
    => NFTA c s -> NFTA c (Set s)
det a = 
    let 
	(ks, rs) = phull (alphabet a) 
		         (xform $ trans a)
			 emptySet
			 ( mkSet [ emptySet ] )
			 []		
	fs = sfilter (\ s -> not (isEmptySet (s `intersect` finals a))) ks
    in	
        NFTA { nfta_info = text "det" <+> info a
	     , alphabet = alphabet a
	     , states = ks
	     , finals = fs
	     , trans = Relation.make rs
             , eps = Relation.empty ks
	     }


