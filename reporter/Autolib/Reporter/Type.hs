module Reporter.Type 

where

-- $Id$

import ToDoc

data Reporter a = OK { result :: a , kommentar :: Doc }
		| NO {               kommentar :: Doc }
     
instance Functor Reporter where
    fmap f ( ok @ OK {} ) = OK { result = f ( result ok )
			       , kommentar = kommentar ok
			       }
    fmap f ( no @ NO {} ) = NO { kommentar = kommentar no
			       }
kommentiere :: Doc -> Reporter a -> Reporter a
-- fügt neuen kommentar (am anfang) hinzu
kommentiere doc r = r { kommentar = doc $$ kommentar r }

instance Monad Reporter where
    return x = OK { result = x , kommentar = empty }
    m >>= f  = kommentiere ( kommentar m ) $ case m of
	         ok @ OK {} -> f ( result ok )
		 no @ NO {} -> NO {}
    
inform :: Doc -> Reporter ()
inform doc = OK { result = () , kommentar = doc }

reject :: Doc -> Reporter ()
reject doc = NO { kommentar = doc }

accept :: Doc -> a -> Reporter a
accept doc x = OK { kommentar = doc, result = x }

       
    
