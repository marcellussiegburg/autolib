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

-- execute iterator until result occurs
execute :: ToDoc a => Iterator a -> Reporter a
execute ( Iterator doc step start ) = helper start where
    helper state = do
        inform $ text "execute one step for iterator" <+> doc
        next <- nested 4 $ step state
        inform $ text "... one step for iterator" <+> doc
	case next of 
	     Left  state' -> do
	         inform $ text "continue"
                 helper state'
	     Right result -> do
		 inform $ text "got result" <+> toDoc result     
                 return result

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






