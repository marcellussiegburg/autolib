module Reporter.Type 

where

-- $Id$

import Output

import qualified Pretty
import qualified Html

import Maybe (isJust, fromMaybe)

import Right
import Wrong

data Reporter a = 
     Reporter { result :: Maybe a 
	      , kommentar :: Output
	      , action :: IO ()
	      }
     
-- die action wird (evtl.) ausgeführt,
-- aber separat, d. h. innerhalb des reporters
-- kann man ihre ergebnisse nicht verwenden
-- ist also nur sinnvoll für reine ausgaben (tracing usw.)

instance Functor Reporter where
    fmap f r = Reporter { result = fmap f $ result r
			, kommentar = kommentar r 
			, action = action r
			}

-- kommentiere :: Output -> Reporter a -> Reporter a
-- fügt neuen kommentar (am anfang) hinzu
-- kommentiere doc r = r { kommentar = doc `Above` kommentar r }

execute :: IO () -> Reporter ()
execute act = Reporter { result = return ()
		       , kommentar = Empty
		       , action = act
		       }

instance Monad Reporter where
    return x = Reporter { result = return x 
			, kommentar = Empty 
			, action = return ()
			}

    -- ein bißchen um die ecke programmiert,
    -- damit die zusammensetzung der dokumente lazy ist
    -- d. h. wir wollen die texte so früh wie möglich sehen,
    -- unabhängig vom fortgang der rechnung
    m >>= f  = 
        let k = kommentar m
	    x = do r <- result m ; return $ f r
	    l = ( case x of Nothing -> Empty ; Just n -> kommentar n ) 
	    a = ( case x of Nothing -> return () ; Just n -> action n ) 
	in  Reporter { kommentar = k `Above` l
		     , result = do r <- x ; result r
		     , action = action m >> a
		     }

    fail msg = reject $ Pretty.text $ "*** fail: " ++ msg

output :: Output -> Reporter ()
output o = Reporter { result = Just () , action = return () , kommentar = o }
    
inform :: Pretty.Doc -> Reporter ()
inform = output . Doc

html :: Html.Html -> Reporter ()
html =  output . Html

link :: FilePath -> Reporter ()
link ref = output $ Html $ Html.anchor ( Html.tt Html.<< ref ) 
	                   Html.! [ Html.href ref ]

image :: String -> Reporter ()
image ref = html $ Html.image Html.! [ Html.src ref, Html.alt ref ]

newline :: Reporter ()
newline = inform ( Pretty.text " " )

nested :: Int -> Reporter a -> Reporter a
nested d r = r { kommentar = Nest $ kommentar r }

reject :: Pretty.Doc -> Reporter a
reject d = Reporter { result = Nothing, action = return (),  kommentar = Doc d }


silent :: Reporter a -> Reporter a
-- wenn ok, dann nichts sagen, sonst fehler melden
silent r = r { kommentar = if isJust ( result r ) 
			   then Empty else kommentar r
	     }

wrap :: Reporter a -> Reporter ( Maybe a )
-- a reporter who always returns
wrap r = r { result = Just $ result r }

export :: Render r => Reporter a -> ( Maybe a, r )
export r = ( result r, render $ kommentar r )

run :: Render r => Reporter a -> IO ( Maybe a, r )
run r = do
    action r
    return $ export r

-- for use in classical autotool problems
reporter :: Reporter Int -> IO String
reporter r = do
    print ( render ( kommentar r ) :: Pretty.Doc )
    case result r of
        Just i -> right_with $ "OK # Size: " ++ show i
	Nothing -> wrong

-- for use in challenger problems
-- flag is true  iff  reporter returns at all (with any value)
lazy_cheporter :: Reporter a -> ( Bool, Pretty.Doc )
lazy_cheporter r = 
    ( isJust $ result r
    , render $ kommentar r
    )    

cheporter :: Reporter Bool -> ( Bool, Pretty.Doc )
cheporter r = lazy_cheporter $ do
    f <- r
    assert f $ Pretty.text "Bedingung erfüllt?"

porterche :: ( Bool, Pretty.Doc ) -> Reporter ()
porterche ( p, d ) = if p then inform d else reject d 

assert :: Bool -> Pretty.Doc -> Reporter ()
assert p doc = do
    inform doc
    nested 4 $
	 if p then inform $ Pretty.text "Ja."
	      else reject $ Pretty.text "Nein."
