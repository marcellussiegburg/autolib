module Autolib.TES.Over where

--  $Id$

import Autolib.TES.Term
import Autolib.TES.Position
import Autolib.TES.Over.Lap
import Autolib.TES.Unify
import Autolib.TES.Data

import Autolib.Reporter
import Autolib.ToDoc

import Data.List ( tails )
import Data.Maybe ( maybeToList )
import Control.Monad ( guard )

laps :: TRSC v c
     => [ Term v c ]
     -> [ Lap v c ]
laps ts = do
    x : ys <- tails ts
    y <- ys
    (q, z) <- positions y
    guard $ not $ isvar z
    u <- maybeToList $ mgu x z
    return $ Lap { l = x , r = y,  p = q, s = u }

overlaps :: TRSC v c
	 => TRS v c
	 -> [ Lap v c ]
overlaps trs = laps $ lhss trs

is_nonoverlapping :: TRSC v c
		  => TRS v c
		  -> Reporter ()
is_nonoverlapping trs = do
    inform $ text "system should be nonoverlapping:"
    let ols = overlaps trs
    if null ols 
       then return ()
       else reject $ vcat
	    [ text "but has these overlaps:"
	    , nest 4 $ toDoc ols
	    ]

