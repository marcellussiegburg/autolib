-- -*- mode: haskell -*-
{-# LANGUAGE TypeSynonymInstances, TemplateHaskell #-}

module Autolib.Reporter.Proof where

--  $Id$

import Autolib.Reporter.Boolean.Type
import Autolib.Output
import Autolib.ToDoc

data Proof = 
     Proof { value  :: Bool
	   , formula :: Doc
	   , reason :: Output
	   , history :: [ Output ]
	   }



explain :: Proof -> Output
explain p = foldr1 Above
	  [ Doc $ text "the value of" 
		<+> quoted ( formula p )
		-- <+> text "(the formula)"
	  , Doc $ text "is" <+> toDoc ( value p ) <> text ", because"
	  , Itemize $ reverse $ history p
	  , Nest $ reason p
	  ]
       
instance Eq Proof where
    p == q = value p == value q


instance ToDoc Output where toDoc = Autolib.Output.render

$(derives [makeToDoc] [''Proof])

instance Show Proof where show = Autolib.ToDoc.render . toDoc

