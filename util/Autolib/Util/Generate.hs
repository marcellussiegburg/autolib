module Autolib.Util.Generate where

--  $Id$

import Autolib.Util.Zufall
import Random

-- | erzeugt "zufälligen" dinge fixierter größe
type Generator a = Int -> IO ( a )


-- | annahme: größen verhalten sich additiv
type Operator a = ( Int, [ a ] -> a )

data Config a = 
     Config { base :: [ Generator a ]
	    , ops  :: [ Operator  a ]
	    , depth :: Int
	    , size :: Int
	    }

generate :: Config a -> IO ( a )

generate conf | depth conf <= 0 = do
    gen <- eins $ base conf
    gen $ size conf

generate conf = do
    ( arity, op ) <- eins $ ops conf
    if arity > size conf
       then generate conf { depth = 0 }
       else do
	    xs <- summe arity $ size conf
	    gs <- sequence $ do 
	       x <- xs
	       return $ do
	           generate $ conf { depth = pred $ depth conf , size  = x }
	    return $ op gs

