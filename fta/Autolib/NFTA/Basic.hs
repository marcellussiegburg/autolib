module NFTA.Basic where

--  $Id$

import NFTA.Type
import NFTA.Insert
import TES.Term

import Util.Wort ( alle )

-- | automaton the recognizes all trees
complete :: NFTAC c Int
    => Set c -- ^ signature
    -> NFTA c Int
complete cs = 
    NFTA { states = mkSet [0]
	 , finals = mkSet [0]
	 , trans  = mkSet $ do
	       c <- setToList $ cs
	       return ( 0, c, replicate (arity c) 0 )
	 , eps    = mkSet []
	 }

-- | recognize all terms
-- but use different state for each symbol
split ::  NFTAC c Int
    => Set c -- ^ signature
    -> NFTA c Int
split s = 
    let ics = zip [0 .. ] $ setToList s
	its = map fst ics
    in NFTA { states = mkSet its
	    , finals = mkSet its -- all are accepting
	    , eps = mkSet []
	    , trans  = mkSet $ do
	          (i, c) <- ics
	          args <- alle its (arity c) 
	          return (i, c, args)
            }
