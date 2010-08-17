{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances #-}

module Autolib.Informed where

import Autolib.ToDoc

class Informed i where
      -- | zeigt Information
      info     :: i -> Doc

      -- | ändert Information
      informed :: Doc -> i -> i
      informed d x = error "Informed.informed not implemented"

      texinfo  :: i -> String
      texinfo = show . info

      texinformed :: String -> i -> i
      texinformed cs = informed (text cs)

instance ToDoc i => Informed i where
    info = toDoc

-- | konstruiere info-doc für Funktions-Aufruf
funni :: String -> [ Doc ] -> Doc
funni f args = parens $ fsep ( text f : args )

