module Autolib.TES.Auto where

--  $Id$

import Sets



data NFTA c s = 
     NFTA { states :: Set s
	  , finals :: Sets a
	  , trans  :: ( s, c, [s] )
	  }
