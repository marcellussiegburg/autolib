module NFA.Language where

-- $Id$

import Exp -- for debugging
import Exp.Inter

import NFA_Type
import Shortest ( accepted, is_accepted )
import qualified Minus
import Letters

import Language.Type

import ToDoc
import Set
import List (nub)
import Util.Zufall

language :: NFA Int -> Language
language a = 
   let cs = letters a
   in  Language
       { abbreviation = render $ info a
       , alphabet     = cs
       , contains     = is_accepted a

       , sample       = \ c n -> do
            ws <- einige c $ take (n^2 + 1) $ accepted a
	    return $ nub ws

       , anti_sample  = 
	     sample ( language $ Minus.complement ( setToList cs ) a )

       }

