module NFTA.Basic where

--  $Id$

import NFTA.Type
import NFTA.Insert
import TES.Term

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

