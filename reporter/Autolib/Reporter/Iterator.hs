module Reporter.Iterator where

--  $Id$

import Reporter.Type
import ToDoc
import Informed

-- | for stepwise computations
-- s is state type
data Iterator a = forall s . 
     Iterator Doc -- ^ description
              ( Reporter s ) -- ^ produce initial state
	      ( s -> Reporter (Either s a) ) -- ^ stepper

instance Informed ( Iterator a ) where
    info ( Iterator doc start step ) = doc


instance Functor Iterator where
    fmap f ( Iterator doc start step  ) 
       = Iterator ( funni "fmap <function>" [ doc ] )
	       ( start )
               ( \ s -> do
		     n <- step s
		     case n of
		          Left s' -> return $ Left s'
		          Right x -> return $ Right $ f x
               )






