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

    -- ein bißchen um die ecke programmiert,
    -- damit die zusammensetzung der dokumente lazy ist
    -- d. h. wir wollen die texte so früh wie möglich sehen,
    -- unabhängig vom fortgang der rechnung
    m >>= f  = 
        let k = kommentar m
	    x = do r <- result m ; return $ f r
	    l = ( case x of Nothing -> empty ; Just n -> kommentar n ) :: Doc
	in  Reporter { kommentar = k $$ l
		     , result = do r <- x ; result r
		     }

    
inform :: Doc -> Reporter ()
inform doc = Reporter { result = Just () , kommentar = doc }

newline :: Reporter ()
newline = inform ( text " " )


reject :: Doc -> Reporter a
reject doc = Reporter { result = Nothing,   kommentar = doc }


reporter :: Reporter Int -> IO String
reporter r = do
    print $ kommentar r
    case result r of
        Just i -> right_with $ "OK # Size: " ++ show i
	Nothing -> wrong

       
    
