module Informed where

-- $Id$

import ToDoc

class Informed i where
      info     :: i -> Doc
      informed :: Doc -> i -> i
      
funni :: String -> [ Doc ] -> Doc
-- konstruiere info-doc für Funktions-Aufruf
funni f args = parens $ fsep ( text f : args )
