-- | instead of listing all paths (exponential),
-- listen only reachable states

module Autolib.ENFA.Reach where

--  $Id$

import Autolib.ENFA.Data
import Autolib.Relation.Ops hiding ( trans )
import Control.Monad ( guard )

-- | paths (first move maybe eps, last move is non-eps)
reachables_from :: ( Eq b, NFAC c s )
      => ( b -> c -> Bool ) -- ^ compatibility
      -> ENFA c s
      -> Set s 
      -> [ b ] 
      -> Set s
reachables_from compat a ps [] = ps
reachables_from compat a ps (x : xs) = 
    reachables_from compat a 
        ( mkSet $ do
	      p <- setToList ps
	      p' <- leclosure a p
	      Just fm <- return $ lookupFM ( trans a ) p'
	      ( y, qs ) <- fmToList fm
	      guard $ compat x y
	      setToList qs
	) xs

eps_reachables_from compat a ps w = 
    seclosure a $ reachables_from compat a ps w

seclosure a ps = unionManySets $ map ( eclosure a ) $ setToList ps

    
