module Reporter.Type 

where

-- $Id$

import ToDoc
import Maybe (isJust, fromMaybe)

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

nested :: Int -> Reporter a -> Reporter a
nested d r = r { kommentar = nest d $ kommentar r }

reject :: Doc -> Reporter a
reject doc = Reporter { result = Nothing,   kommentar = doc }


silent :: Reporter a -> Reporter a
silent r = r { kommentar = if isJust ( result r ) 
			   then empty else kommentar r
	     }

wrap :: Reporter a -> Reporter ( Maybe a )
-- a reporter who always returns
wrap r = Reporter { result = Just $ result r 
		  , kommentar = kommentar r 
		  }

-- for use in classical autotool problems
reporter :: Reporter Int -> IO String
reporter r = do
    print $ kommentar r
    case result r of
        Just i -> right_with $ "OK # Size: " ++ show i
	Nothing -> wrong

-- for use in challenger problems
cheporter :: Reporter Bool -> ( Bool, Doc )
cheporter r = 
    ( fromMaybe False $ result r
    , kommentar r
    )    

