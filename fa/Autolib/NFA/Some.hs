module NFA.Some where

-- $Id$

import NFA

import Util.Zufall

import ToDoc
import Random
import Set

some :: Set Char -> Int -> IO (NFA Char Int)
-- erzeugt irgendeinen NFA mit s zuständen
some alpha s = do
    let ss = [ 1 .. s ]
    st <- eins ss -- start
    fi <- eins ss -- final
    pfeile <- sequence $ replicate ( s * cardinality alpha ) $ do
                  p <- eins ss
		  q <- eins ss
		  c <- eins $ setToList alpha
		  return (p, c, q)
    let a = NFA { info = funni "some" [ toDoc alpha , toDoc s ]
		 , states = mkSet ss
		 , starts = unitSet st
		 , finals = unitSet fi
		 , trans  = collect pfeile
		 }
    return a

		 
nontrivial ::  Set Char -> Int -> IO (NFA Char Int)
nontrivial alpha s = 
    repeat_until ( some alpha s )
	( \ a -> letters a == alpha
	      && states (trim a) == states a )
