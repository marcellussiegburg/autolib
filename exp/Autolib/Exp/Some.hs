module Autolib.Exp.Some where

-- -- $Id$

import Autolib.Exp.Type
import Autolib.Exp.Print
import Autolib.Exp.Syntax

import Autolib.Util.Zufall

import Autolib.Exp.Inter
import Autolib.NFA hiding ( Dot )

import Autolib.Size
import qualified Autolib.NFA.Ops as Ops
import qualified Autolib.NFA.Basic as Basic
import Autolib.ToDoc hiding ( empty )

import Autolib.Set
-- import System.Random

-- | erzeugt irgendeinen ausdruck  x  über gegebenem alphabet
-- mit  size x  <= 2 * s
-- mit wenigstens zwei sternen
-- mit  DFA-größe >= s
-- tatsächlich rechnen wir gleich den DFA mit aus
nontrivial :: ( RandomC m,  NFAC c Int )
	   => Set c 
	   -> Int 
	   -> m (RX c, NFA c Int)
nontrivial alpha s = 
    repeat_until ( some alpha (2 * s) )
	( \ (x,a) -> 2 <= stars x
	             && size a >= s )


some :: ( NFAC c Int, RandomC m )
     => Set c -> Int 
     -> m ( RX c, NFA c Int)
some alpha s | s <= 1 = do 
	   x <- eins (setToList alpha)
	   return ( Letter x, Basic.word [x] )
some alpha s = 
    entweder ( do (x, a) <- binary alpha (s - 1)
	          return ( PowerStar x, minimize $ normalize $ Ops.star a )
	     )
             ( binary alpha s )

both x = ( x, inter std x )

binary alpha s = do 
       let mn op l r = minimize $ normalize $ op l r
       (dist, opx, opa) <- eins [ (True, Dot, mn Ops.dot)
				, (False, Union, mn Ops.union) 
				]
       sl <- randomRIO (1, s - 2)
       let sr = s - 1 - sl
       l @ (xl, al) <- some alpha sl
       r @ (xr, ar) <- repeat_until (some alpha sr)
	    ( \ (xr, ar) -> dist || xr /= xl )
       let x = opx xl xr ; a = opa al ar
       -- wenn das resultat zu groß ist,
       -- dann reicht eines der argumente.
       if size a >  s + 2 
	  then do
		  -- putStr "CUT "
		  if size al > s `div` 2 
		     then return l else return r
	  else return (x, a)

stars :: RX c -> Int
stars x = sum $ do
    s <- subtrees x
    return $ case s of PowerStar _ -> 1 ; _ -> 0




