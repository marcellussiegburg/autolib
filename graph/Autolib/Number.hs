module Autolib.Number where

-- | Klasse fuer den Nummerieung von Typen
class Number  a b | a -> b where
--class Number  a b where
  number :: a -> b 
