module SRS.Rule where

--  $Id$


import Autolib.Symbol
import TES.Parsec

import Autolib.ToDoc
import Autolib.Reader

type Rule c = ( [c], [c] )

instance ( Symbol c ) => Reader ( Rule c ) where
    readerPrec p = do
        lhs <- many $ readerPrec 0
	reservedOp trs "->"
	rhs <- many $ readerPrec 0
	option "" $ my_comma -- use maximum munch
	return ( lhs, rhs )

instance Symbol c => ToDoc ( Rule c ) where
    toDoc (lhs, rhs) = hsep 
        [ toDoc_list lhs
	, text "->"
	, toDoc_list rhs
       	-- TODO: , ToDoc.comma 
	]

instance Symbol c => Show ( Rule c ) where
    show = render . toDoc
