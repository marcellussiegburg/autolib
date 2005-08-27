module Autolib.ENFA.Path where

--  $Id$

import Autolib.ENFA.Data
import Autolib.Relation.Ops hiding ( trans )
import Control.Monad ( guard )

data Step c a = Step { from :: a
		     , mark :: Maybe c
		     , to :: a 
		     }
     deriving ( Eq, Ord, Show )

type Path c a = [ Step c a ]


-- | find paths whose image corresponds to given string
-- TODO: find only such paths that begin and end with a real (non-eps) step

paths :: ( Eq b, NFAC c a )
      => ( b -> c -> Bool ) -- ^ compatibility
      -> ENFA c a 
      -> [ b ] 
      -> [ Path c a ]
paths compat a w = do
    p <- lstates a
    paths_from compat a p w

-- | all paths starting from here
paths_from :: ( Eq b, NFAC c a )
      => ( b -> c -> Bool ) -- ^ compatibility
      -> ENFA c a 
      -> a 
      -> [ b ] 
      -> [ Path c a ]
paths_from compat a p w = do
        q <- leclosure a p
        f <- real_paths_from compat a q w
        return $ if p == q 
		 then f
		 else Step { from = p, mark = Nothing, to = q } : f

-- | all paths starting from here
-- and starting with a real ( non eps ) transition
real_paths_from :: ( Eq b, NFAC c a )
      => ( b -> c -> Bool ) -- ^ compatibility
      -> ENFA c a 
      -> a 
      -> [ b ] 
      -> [ Path c a ]
real_paths_from compat a p [] = return []
real_paths_from compat a p (x : xs) = do
    Just fm <- return $ lookupFM ( trans a ) p
    ( y, qs ) <- fmToList fm
    guard $ compat x y
    q <- setToList qs
    f <- paths_from compat a q xs    
    return $ Step { from = p, mark = Just y, to = q } : f





