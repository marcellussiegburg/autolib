module Autolib.Exp.Some where

-- -- $Id$

import Autolib.Exp.Type
import Autolib.Exp.Print
import Autolib.Exp.Syntax

import Autolib.Util.Zufall

import Autolib.Exp.Inter
import Autolib.NFA

import Autolib.Size
import qualified Autolib.NFA.Ops
import qualified Autolib.NFA.Basic
import Autolib.ToDoc hiding ( empty )

import Data.Set
import Random

nontrivial :: Set Char -> Int -> IO (Exp, NFA Char Int)
-- erzeugt irgendeinen ausdruck  x  über gegebenem alphabet
-- mit  size x  <= 2 * s
-- mit wenigstens zwei sternen
-- mit  DFA-größe >= s
-- tatsächlich rechnen wir gleich den DFA mit aus
nontrivial alpha s = 
    repeat_until ( some alpha (2 * s) )
	( \ (x,a) -> 2 <= stars x
	             && size a >= s )


some :: Set Char -> Int -> IO (Exp, NFA Char Int)
some alpha s | s <= 1 = do 
	   x <- eins (setToList alpha)
	   return ( Letter x, NFA.Basic.word [x] )
some alpha s = 
    entweder ( do (x, a) <- binary alpha (s - 1)
	          return ( Star x, minimize $ normalize $ NFA.Ops.star a )
	     )
             ( binary alpha s )

both x = ( x, inter std x )

binary alpha s = do 
       let mn op l r = minimize $ normalize $ op l r
       (dist, opx, opa) <- eins [ (True, Dot, mn NFA.Ops.dot)
				, (False, Union, mn NFA.Ops.union) 
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

stars :: Exp -> Int
stars x = sum $ do
    s <- subtrees x
    return $ case s of Star _ -> 1 ; _ -> 0




