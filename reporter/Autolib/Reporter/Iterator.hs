module Reporter.Iterator where

-- $Id$

import Reporter.Type
import ToDoc

data Iterator a = forall s . 
     Iterator Doc
	      ( s -> Reporter (Either s a) )
              s 

