module Autolib.NFTA.Basic where

--  $Id$

import Autolib.NFTA.Type
import Autolib.NFTA.Insert
import Autolib.TES.Term

import Autolib.Util.Wort ( alle )
import qualified Autolib.Relation as Relation
import Autolib.Informed
import Autolib.ToDoc

-- | automaton the recognizes all trees
complete :: NFTAC c Int
    => Set c -- ^ signature
    -> NFTA c Int
complete cs = 
    let stats = mkSet [0] 
	pcqs = do
	       c <- setToList $ cs
	       return ( 0, c, replicate (arity c) 0 )
    in NFTA 
         { nfta_info = funni "complete" [ info cs ]
	 , alphabet = cs
         , states = stats
	 , finals = stats
	 , trans  = mach pcqs
	 , eps     = Relation.empty $ stats
	 }

-- | recognize all terms
-- but use different state for each symbol
split ::  NFTAC c Int
    => Set c -- ^ signature
    -> NFTA c Int
split s = 
    let ics = zip [ 1 .. ] $ setToList s
	its = map fst ics
    in NFTA { nfta_info = funni "split" [ info s ]
	    , alphabet = s
	    , states = mkSet $ its
	    , finals = mkSet $ its
	    , eps = Relation.empty $ mkSet $ its
	    , trans  = Relation.make $ do
	          (i, c) <- ics
	          argv <- alle its (arity c)
	          return (i, (c, argv))
            }

-- | recognize all terms
-- but use different state for each symbol
split_eps ::  NFTAC c Int
    => Set c -- ^ signature
    -> NFTA c Int
split_eps s = 
    let top = 0
        ics = zip [ 1 .. ] $ setToList s
	its = map fst ics
    in NFTA { nfta_info = funni "split_eps" [ info s ]
	    , alphabet = s
	    , states = mkSet $ top : its
	    , finals = mkSet [ top ]
	    , eps = Relation.make $ do 
	          it <- its
	          return ( top, it )
	    , trans  = Relation.make $ do
	          (i, c) <- ics
	          return (i, (c, replicate (arity c) top))
            }

mach pcqs =  Relation.make
		  $ do (p,c,qs) <- pcqs; return (p,(c,qs))

