module ToDoc.Typeable where

--  $Id$

import Data.Typeable
import ToDoc.Class
import ToDoc.Dutch

-- this should rather be next to the Show instance
-- in Data.Typeable (the ghc library)?

instance ToDoc TypeRep where
    toDocPrec p t =
	let con  = typerepTyCon t
	    args = typerepArgs t
	    str  = tyconString con
	in case args of
	    [] -> text str
	    [x] | str == "[]" -> brackets $ toDocPrec 0 x
	    [a, r] | str == "->" -> docParen (p >= fcp)
			   $ toDocPrec fcp a
			      $$ nest 4 ( text str <+>
				   toDocPrec (pred fcp) r
				  )
	    xs | head str == ',' -> dutch_tuple 
		     $ map (toDocPrec 0) args
	       | otherwise -> docParen (p >= fcp)
                     $ text str <+> sep (map (toDocPrec fcp) args)
