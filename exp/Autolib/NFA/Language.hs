module Autolib.NFA.Language where

-- -- $Id$

import Autolib.Exp -- for debugging
import Autolib.Exp.Inter

import Autolib.NFA.Type
import Autolib.NFA.Shortest ( accepted, is_accepted )
import qualified Autolib.NFA.Minus
import Autolib.Letters

import Autolib.Language.Type

import Autolib.ToDoc
import Data.Set
import Data.List (nub)
import Autolib.Util.Zufall

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

