module Reporter.Boolean.ToDoc where

--  $Id$

import Reporter.Boolean.Data
import ToDoc

instance ToDoc i => ToDoc (Boolean i) where
    toDocPrec p ( Uf Star x ) 
	=  toDocPrec 10 x <> text "^*"
    toDocPrec p ( Uf up x ) 
	= text ( uname up ) <+> toDocPrec 10 x
    toDocPrec p ( Bof op xs ) 
	= let pop = 1 + fromEnum op
	  in   docParen ( p > pop ) 
	     $ sepBy ( text $ bname op ) $ map ( toDocPrec pop ) xs
    toDocPrec p ( Atomic a ) 
	= toDocPrec p a

instance ToDoc i => Show (Boolean i) where show = render . toDoc

