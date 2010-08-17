module Autolib.NFA.Some where

-- -- $Id$

import Autolib.NFA.Type
import Autolib.NFA.Trim

import Autolib.Util.Zufall

import Autolib.ToDoc
import Autolib.Set

-- | erzeugt irgendeinen NFA mit s zuständen
some :: NFAC c Int
     => Set c -> Int 
     -> IO (NFA c Int)
some alpha s = do
    let ss = [ 1 .. s ]
    st <- eins ss -- start
    fi <- eins ss -- final
    pfeile <- sequence $ replicate ( s * cardinality alpha ) $ do
                  p <- eins ss
		  q <- eins ss
		  c <- eins $ setToList alpha
		  return (p, c, q)
    let cs = mkSet $ do (p,c,q) <- pfeile; return c
    let a = NFA { nfa_info = funni "some" [ toDoc alpha , toDoc s ]
		, alphabet = cs
		 , states = mkSet ss
		 , starts = unitSet st
		 , finals = unitSet fi
		 , trans  = collect pfeile
		 }
    return a

		 
nontrivial :: NFAC c Int
	   => Set c -> Int -> IO (NFA c Int)
nontrivial alpha s = 
    repeat_until ( some alpha s )
	( \ a -> alphabet a == alpha
	      && states (trim a) == states a )
