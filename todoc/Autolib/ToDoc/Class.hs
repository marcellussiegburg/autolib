module Autolib.ToDoc.Class 

( module Autolib.ToDoc.Class
, module Text.PrettyPrint.HughesPJ
)

where

--  $Id$

import Text.PrettyPrint.HughesPJ 

class ToDoc a where 
    toDoc :: a -> Doc
    -- default:
    toDoc = toDocPrec 0 -- useful?

    toDocPrec :: Int -> a -> Doc
    -- default:
    toDocPrec p = toDoc -- dangerous?


docParen :: Bool -> Doc -> Doc
docParen f = if f then parens else id

-- | zur ausgabe ohne zeilenschaltungen
-- TODO: sollte besser gehen (anderen renderer wählen?)
showDoc :: Doc -> String
showDoc = unwords . words . render


-- | funcall precedence
fcp = 10 :: Int
