module Exp.Some where

-- $Id$

import Exp.Type
import Exp.Print
import Exp.Syntax

import Util.Zufall

import Exp.Inter
import NFA

import Size
import qualified Ops
import qualified Basic
import ToDoc hiding ( empty )

import Set
import Random

some :: Set Char -> Int -> IO (Exp, NFA Int)
-- erzeugt irgendeinen ausdruck
-- über dem alphabet
-- von gegebener DFA-größe
-- tatsächlich rechnen wir gleich den DFA mit aus

some alpha s | s <= 1 = do 
	   x <- eins (setToList alpha)
	   return ( Letter x, Basic.word [x] )
some alpha s = 
    entweder ( do (x, a) <- binary alpha (s - 1)
	          return ( Star x, Ops.star a )
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
       if size a > 3 * s 
	  then do putStr "CUT "
	          eins [ l, r ]
	  else return (x, a)

stars :: Exp -> Int
stars x = sum $ do
    s <- subtrees x
    return $ case s of Star _ -> 1 ; _ -> 0

nontrivial :: Set Char -> Int -> IO (Exp, NFA Int)
nontrivial alpha s = 
    repeat_until ( some alpha (2 * s) )
	( \ (x,a) -> 2 <= stars x
	             && cardinality (states a) >= s )




