module Reporter.Type 

where

-- $Id$

import ToDoc

import Right
import Wrong

data Reporter a = Reporter { result :: Maybe a , kommentar :: Doc }
     
instance Functor Reporter where
    fmap f r = Reporter { result = fmap f $ result r
			, kommentar = kommentar r 
			}

kommentiere :: Doc -> Reporter a -> Reporter a
-- fügt neuen kommentar (am anfang) hinzu
kommentiere doc r = r { kommentar = doc $$ kommentar r }

instance Monad Reporter where
    return x = Reporter { result = return x , kommentar = empty }
    m >>= f  = kommentiere ( kommentar m ) $ case result m of
	         Just x  -> f x
		 Nothing -> Reporter { result = Nothing, kommentar = empty }
    
inform :: Doc -> Reporter ()
inform doc = Reporter { result = Just () , kommentar = doc }

reject :: Doc -> Reporter a
reject doc = Reporter { result = Nothing,   kommentar = doc }


reporter :: Reporter Int -> IO String
reporter r = do
    print $ kommentar r
    case result r of
        Just i -> right_with $ "OK # Size: " ++ show i
	Nothing -> wrong

       
    
