module Autolib.Reporter.Parallel where

--  $Id$

import Autolib.Reporter.Type
import Autolib.Reporter.Iterator
import Autolib.ToDoc


-- | execute several reporters in parallel
-- the first with a result stops all others
parallel :: [ Iterator a ]
	 -> Reporter (Maybe a)
parallel [] = do
    inform $ text "empty iterator list"
    return Nothing

parallel (it @ ( Iterator name prod fun )  : its) = do
    state <- prod
    inform $ text "execute one step of iterator" $$ nest 4 name
    res  <- nested 8 $ wrap $ fun state
    case res of
         Nothing -> do
	     inform $ text "stopping iterator" $$ nest 4 name
             parallel its
         Just ( Left next ) -> do
	     inform $ text "keeping iterator" $$ nest 4 name
	     parallel $ its ++ [ Iterator name ( return next ) fun ]
         Just ( Right result) -> do
	     inform $ text "stopping all iterators"
	     return $ Just result

singular :: Iterator a -> Reporter ( Maybe a )
singular it = parallel [ it ]

