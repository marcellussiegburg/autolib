module NFTA.Basic where

--  $Id$

import NFTA.Type
import NFTA.Insert
import TES.Term

import Util.Wort ( alle )
import qualified Relation

-- | automaton the recognizes all trees
complete :: NFTAC c Int
    => Set c -- ^ signature
    -> NFTA c Int
complete cs = 
    let stats = mkSet [0] 
	pcqs = do
	       c <- setToList $ cs
	       return ( 0, c, replicate (arity c) 0 )
    in NFTA { states = stats
	 , finals = stats
	 , trans  = mach pcqs
	 , inv_trans = cham  pcqs
	 , eps     = Relation.flat $ stats
	 , inv_eps = Relation.flat $ stats
	 }

-- | recognize all terms
-- but use different state for each symbol
split ::  NFTAC c Int
    => Set c -- ^ signature
    -> NFTA c Int
split s = 
    let ics = zip [0 .. ] $ setToList s
	its = map fst ics
	pcqs =  do
	          (i, c) <- ics
	          args <- alle its (arity c) 
	          return (i, c, args)
    in NFTA { states = mkSet its
	    , finals = mkSet its -- all are accepting
	    , eps = Relation.flat $ mkSet its
	    , inv_eps = Relation.flat $ mkSet its
	    , trans  = mach pcqs
	    , inv_trans = cham pcqs
            }

mach pcqs =  Relation.make
		  $ do (p,c,qs) <- pcqs; return (p,(c,qs))
cham pcqs =  Relation.make
		     $ do (p,c,qs) <- pcqs; return ((qs,c),p)
