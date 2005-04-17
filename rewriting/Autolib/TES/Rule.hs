{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances #-}

module Autolib.TES.Rule where

--   $Id$

import Autolib.Symbol
import Autolib.Size
import Autolib.Letters
import Autolib.TES.Term

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Set
import Autolib.Hash

import Autolib.TES.Parsec


data Rule t = Rule { lhs :: t
		   , rhs :: t
		   , strict :: Bool
		   }
    deriving ( Eq, Ord )

instance Hash t => Hash ( Rule t ) where
    hash r = hash ( lhs r, rhs r, strict r )

instance Size t => Size ( Rule t ) where
    size r = size (lhs r) + size (rhs r)

instance ToDoc (t ) => ToDoc ( Rule t ) where
    toDoc r = fsep [ toDoc $ lhs r
		   , if strict r then text "->" else text "->="
		   , toDoc $ rhs r
		   ]

instance ToDoc c => ToDoc ( Rule [c] ) where
    toDoc r = fsep [ hsep $ map toDoc $ lhs r
		   , if strict r then text "->" else text "->="
		   , hsep $ map toDoc $ rhs r
		   ]

instance Reader (t) => Reader ( Rule t ) where
    reader = do
        l <- reader
	s <-     do reservedOp trs "->" ; return True
	     <|> do reservedOp trs "->=" ; return False
	r <- reader
	return $ Rule { lhs = l, strict = s, rhs = r }

instance Reader c => Reader ( Rule [c] ) where
    reader = do
        l <- many reader
	s <-     do reservedOp trs "->=" ; return False
	     <|> do reservedOp trs "->" ; return True
	r <- many reader
	return $ Rule { lhs = l, strict = s, rhs = r }


instance Letters t c => Letters (Rule t) c where
    letters rule = union (letters $ lhs rule) (letters $ rhs rule)
    

