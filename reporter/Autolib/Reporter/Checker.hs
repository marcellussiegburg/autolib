module Reporter.Checker where

-- $Id$

import Grammatik.Type
import Reporter.Type
import ToDoc


data Type a =
     Make { nametag :: String
	     , condition :: Doc
	     , investigate :: a -> Reporter ()
	     }

make :: String
	-> Doc 
	-> ( a -> Reporter () ) 
	-> Type a
make tag doc inv = Make { nametag = tag, condition = doc, investigate = inv }

run :: Type a -> a -> Reporter ()
run c g = do
    inform $ condition c
    investigate c g





