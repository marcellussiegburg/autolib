module Reporter.Iterator where

-- $Id$

import Reporter.Type
import ToDoc

data Iterator a = forall s . 
     Iterator Doc
	      ( s -> Reporter (Either s a) )
              s 

no_iterator = Iterator ( text "no_iterator" )
	              ( \ _ -> reject $ text "no_iterator" )
		      undefined

