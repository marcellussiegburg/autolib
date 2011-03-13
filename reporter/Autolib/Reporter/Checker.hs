{-# OPTIONS -fglasgow-exts #-}

module Autolib.Reporter.Checker where

import Autolib.Reporter.Type
import Autolib.ToDoc
import Data.Maybe ( isJust )

data Type a =
     Make { nametag :: String
	     , condition :: Doc
	     , investigate :: a -> Reporter ()
	     }

instance Show ( Type a ) where 
    show = nametag

make :: String
	-> Doc 
	-> ( a -> Reporter () ) 
	-> Type a
make tag doc inv = Make { nametag = tag, condition = doc, investigate = inv }

wahr :: Type a
wahr = make "wahr" empty ( const $ return () )

-- | condition drucken, dann auswerten
run :: Type a -> a -> Reporter ()
run c g = do
    inform $ condition c
    nested 4 $ investigate c g

eval :: Type a -> a -> Reporter Bool
eval c g = do
    mr <- wrap $ investigate c g
    return $ isJust mr

-- | beides ausführen, 
-- aber kurznamen nur vom rechten anzeigen
and_then :: Type a -> Type a
     -> Type a
and_then l r = Make
	 { nametag = nametag r
	 , condition = condition r
	 , investigate = \ a -> do
	       Autolib.Reporter.Checker.run l a
	       Autolib.Reporter.Checker.run r a
         }




