module Reporter.Checker where

-- -- $Id$

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
    nested 4 $ investigate c g

and_then :: Type a -> Type a
     -> Type a
-- beides ausführen, 
-- aber kurznamen nur vom rechten anzeigen
and_then l r = Make
	 { nametag = nametag r
	 , condition = condition r
	 , investigate = \ a -> do
	       Reporter.Checker.run l a
	       Reporter.Checker.run r a
         }




