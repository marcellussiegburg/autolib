module NFA.Some where

-- $Id$

import NFA

import Util.Zufall

import ToDoc
import Random
import Set

some :: Set Char -> Int -> IO (NFA Int)
-- erzeugt irgendeinen NFA mit s zust�nden
some alpha s = do
    let ss = [ 1 .. s ]
    st <- eins ss -- start
    fi <- eins ss -- final
    pfeile <- sequence $ replicate ( s * cardinality alpha ) $ do
                  p <- eins ss
		  q <- eins ss
		  c <- eins $ setToList alpha
		  return (p, c, q)
    return $ NFA { info = funni "some" [ toDoc alpha , toDoc s ]
		 , states = mkSet ss
		 , starts = unitSet st
		 , finals = unitSet fi
		 , trans  = collect pfeile
		 }

		 
nontrivial ::  Set Char -> Int -> IO (NFA Int)
nontrivial alpha s = 
    repeat_until ( some alpha s )
	( \ a -> states (trim a) == states a )