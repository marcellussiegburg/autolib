module Reporter.Parallel where

-- execute several reporters in parallel
-- the first with a result stops all others

import Reporter.Type
import Reporter.Iterator
import ToDoc


parallel :: [ Iterator a ]
	 -> Reporter (Maybe a)
parallel [] = do
    inform $ text "empty iterator list"
    return Nothing

parallel (it @ ( Iterator name fun state )  : its) = do
    inform $ text "execute one step of iterator" $$ nest 4 name
    res  <- nested 8 $ wrap $ fun state
    case res of
         Nothing -> do
	     inform $ text "stopping iterator" $$ nest 4 name
             parallel its
         Just ( Left next ) -> do
	     inform $ text "keeping iterator" $$ nest 4 name
	     parallel $ its ++ [ Iterator name fun next ]
         Just ( Right result) -> do
	     inform $ text "stopping all iterators"
	     return $ Just result

singular :: Iterator a -> Reporter ( Maybe a )
singular it = parallel [ it ]

