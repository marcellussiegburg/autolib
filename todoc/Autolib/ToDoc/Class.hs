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

-- | mutual default instances
-- so that you only have to define one of them
instance Show a => ToDoc a where toDoc = text . show
instance ToDoc a => Show a where show = render . toDoc

docParen :: Bool -> Doc -> Doc
docParen f = if f then parens else id

-- | zur ausgabe ohne zeilenschaltungen
-- TODO: sollte besser gehen (anderen renderer wählen?)
showDoc :: Doc -> String
showDoc = unwords . words . render


-- | funcall precedence
fcp = 10 :: Int
