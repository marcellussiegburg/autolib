module Reporter.Boolean.ToDoc where

--  $Id$

import Reporter.Boolean.Data
import ToDoc

instance ToDoc i => ToDoc (Boolean i) where
    toDocPrec p ( Not x ) 
	= text "not" <+> toDocPrec 5 x
    toDocPrec p ( Fun op xs ) 
	= let pop = 1 + fromEnum op
	  in   docParen ( p > pop ) 
	     $ sepBy ( text $ name op ) $ map ( toDocPrec pop ) xs
    toDocPrec p ( Atomic a ) 
	= toDocPrec p a

instance ToDoc i => Show (Boolean i) where show = render . toDoc

