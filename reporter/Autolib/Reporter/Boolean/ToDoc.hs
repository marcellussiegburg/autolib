module Reporter.Boolean.ToDoc where

--  $Id$

import Reporter.Boolean.Data
import ToDoc

instance ToDoc i => ToDoc (Boolean i) where
    toDocPrec p ( Not x ) 
	= text "not" <+> toDocPrec 5 x
    toDocPrec p ( And xs ) 
	= docParen ( p > 3 ) 
	$ sepBy ( text "and" ) $ map ( toDocPrec 3 ) xs
    toDocPrec p ( Or xs ) 
	= docParen ( p > 1 ) 
	$ sepBy ( text "or" ) $ map ( toDocPrec 1 ) xs
    toDocPrec p ( Atomic a ) 
	= toDocPrec p a

instance ToDoc i => Show (Boolean i) where show = render . toDoc

