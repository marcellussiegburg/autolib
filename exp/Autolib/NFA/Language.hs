module NFA.Language where

-- -- $Id$

import Exp -- for debugging
import Exp.Inter

import NFA.Type
import NFA.Shortest ( accepted, is_accepted )
import qualified NFA.Minus
import Letters

import Language.Type

import ToDoc
import Set
import List (nub)
import Util.Zufall

language :: String  -- nametag
	 -> NFA Char Int 
	 -> Language
language tag a = 
   let cs = letters a
   in  Language
       { nametag      = tag
       , abbreviation = render $ info a
       , alphabet     = cs
       , contains     = is_accepted a

       , sample       = \ c n -> do
            ws <- einige c $ take (n^2 + 1) 
			 -- -- $ takeWhile ( \ w -> length w <= n^2 ) 
			 $ accepted a
	    return $ nub ws

       , anti_sample  = 
	     sample $ language ( "Com" ++ tag )
		    $ NFA.Minus.complement (setToList cs) a 
       }

