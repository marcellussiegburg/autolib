{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances #-}

module Autolib.Informed where

--   $Id$

import Autolib.ToDoc

class Informed i where
      -- zeigt Information
      info     :: i -> Doc

      -- ändert Information
      informed :: Doc -> i -> i
      informed d x = error "Informed.informed not implemented"

      texinfo  :: i -> String
      texinfo = show . info

      texinformed :: String -> i -> i
      texinformed cs = informed (text cs)

instance ToDoc i => Informed i where
    -- default
    info = toDoc

      
funni :: String -> [ Doc ] -> Doc
-- konstruiere info-doc für Funktions-Aufruf
funni f args = parens $ fsep ( text f : args )

