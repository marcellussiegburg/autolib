module Reporter.Iterator where

-- $Id$

import Reporter.Type
import ToDoc
import Informed

-- zustandstyp s
-- jeweils ein rechenschritt

data Iterator a = forall s . 
     Iterator Doc
	      ( s -> Reporter (Either s a) ) -- stepper
            s

instance Informed ( Iterator a ) where
    info ( Iterator doc step start ) = doc


instance Functor Iterator where
    fmap f ( Iterator doc step start ) 
       = Iterator ( funni "fmap <function>" [ doc ] )
               ( \ s -> do
		     n <- step s
		     case n of
		          Left s' -> return $ Left s'
		          Right x -> return $ Right $ f x
               )
	       ( start )






