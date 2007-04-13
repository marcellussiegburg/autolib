{-# OPTIONS -fglasgow-exts #-}

-- -- $Id$

module Autolib.NFA.Minus where



import Autolib.NFA.Type hiding ( cross, union )
import Autolib.NFA.Ops ( cross, neu, union )
import Autolib.NFA.Det

import Autolib.NFA.Trim
import Autolib.NFA.Det
import Autolib.NFA.Normalize

import Autolib.Letters

import Autolib.FiniteMap
import qualified Autolib.Set as Sets
import Autolib.Set (setToList)

import Autolib.ToDoc ( ToDoc, toDoc )
import Control.Monad ( guard )



complete :: ( ToDoc [c], NFAC c Int ) => [c] -> NFA c Int -> NFA c Int
complete alpha a = 
    let n = neu a
	ns = Sets.union (states a) (Sets.unitSet n)
    in	NFA { nfa_info = funni "complete" [ info alpha , info a ]
	    -- , alphabet = alphabet a
	    , alphabet = mkSet alpha
	    , states = ns
	    , starts = if Sets.isEmptySet (starts a) 
		       then Sets.unitSet n else starts a
	    , finals = Sets.minusSet ns (finals a)
	    , trans  = listToFM $ do
	         p <- Sets.setToList ns; c <- alpha
		 return $ ( (p, c) 
			  , case lookupFM (trans a) (p, c) of
				 Just qs -> qs
				 Nothing -> Sets.unitSet n
			  )
	    }

-- | geht immer (argument wird erst deterministisch gemacht)
complement :: ( NFAC c s, ToDoc [c], NFAC c (Set Int) ) 
	   => [c] -> NFA c s -> NFA c Int
complement alpha a 
    = informed ( funni "complement" [ toDoc alpha, info a ] )   
    $ complete alpha $ normalize $ det a

-- | vorsicht: geht nur, wenn argument bereits deterministisch ist
complement_det :: ( NFAC c Int, ToDoc [c], NFAC c (Set Int) ) 
	       => [c] -> NFA c Int -> NFA c Int
complement_det alpha a 
    = informed ( funni "complement_det" [ toDoc alpha, info a ] )   
    $ complete alpha $ normalize $ a

-- | difference
minus :: ( NFAC c Int, NFAC c (Int, Int), NFAC c (Set Int), ToDoc [c] )
      => NFA c Int -> NFA c Int -> NFA c Int
minus a b = 
    let alpha = setToList $ letters a
	c = complement alpha b
	d = cross a c
	ok (p, q) =  Sets.elementOf p (finals a)	
		  && Sets.elementOf q (finals c)
    in	trim $ normalize $ d { nfa_info = funni "minus" [ info a, info b ]
			     , finals = Sets.sfilter ok $ states d 
			     }

-- | symmetric difference (stupid implementation)
symdiff :: (NFAC c Int, NFAC c (Either Int Int), NFAC c (Int, Int) 
	   , ToDoc [c], NFAC c (Set Int)) 
	=> NFA c Int -> NFA c Int -> NFA c Int
-- TODO: effizienter implementieren
symdiff a b 
    = informed ( funni "symdiff" [ info a, info b ] )
    $ normalize $ union (minus a b) (minus b a)

-- | difference, if second argument is already deterministic 
-- note: precondition is NOT checked
minus_det :: ( NFAC c (Int, Int), NFAC c Int, ToDoc [c], NFAC c (Set Int) ) 
	  => NFA c Int -> NFA c Int -> NFA c Int
minus_det a b = 
    let alpha = setToList $ letters a
	c = complement_det alpha b
	d = cross a c
	ok (p, q) =  Sets.elementOf p (finals a)	
		  && Sets.elementOf q (finals c)
    in	trim $ normalize $ d { nfa_info = funni "minus_det" [ info a, info b ]
			     , finals = Sets.sfilter ok $ states d 
			     }



