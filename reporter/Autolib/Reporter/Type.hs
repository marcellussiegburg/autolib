module Autolib.Reporter.Type 

where

-- -- $Id$

import Autolib.Output

import qualified Text.PrettyPrint.HughesPJ as Pretty

import Data.Maybe (isJust, fromMaybe)

import System.Random

import Control.Monad.State

data Dontuse
type Reporter_State = Reporter Dontuse

data Reporter a = 
     Reporter { result :: Maybe a 
	      , kommentar :: Output
	      , action :: IO ()
	      , transformer :: StdGen -> StdGen
	      }
     
-- die action wird (evtl.) ausgeführt,
-- aber separat, d. h. innerhalb des reporters
-- kann man ihre ergebnisse nicht verwenden
-- ist also nur sinnvoll für reine ausgaben (tracing usw.)

instance Functor Reporter where
    fmap f r = r { result = fmap f $ result r }

execute :: IO () -> Reporter ()
execute act = Reporter { result = return ()
		       , kommentar = Empty
		       , action = act
		       , transformer = id
		       }

instance Monad Reporter where
    return x = Reporter { result = return x 
			, kommentar = Empty 
			, action = return ()
			, transformer = id
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
		     , transformer = id
		     }

    fail msg = reject $ Pretty.text $ "*** fail: " ++ msg


output :: Output -> Reporter ()
output o = Reporter 
	 { result = Just () 
	 , action = return () 
	 , kommentar = o 
	 , transformer = id
	 }
    
reject :: Pretty.Doc -> Reporter a
reject d = Reporter 
	 { result = Nothing
	 , action = return ()
	 , kommentar = Doc d 
	 , transformer = id
	 }

--------------------------------------------------------------------------

inform :: Pretty.Doc -> Reporter ()
inform = output . Doc

newline :: Reporter ()
newline = inform ( Pretty.text " " )

nested :: Int -> Reporter a -> Reporter a
nested d r = r { kommentar = Nest $ kommentar r }

repo :: Reporter a -> Pretty.Doc
repo = Autolib.Output.render .  kommentar

-- | wenn ok, dann nichts sagen, sonst fehler melden
silent :: Reporter a -> Reporter a
silent r = r { kommentar = if isJust ( result r ) 
			   then Empty else kommentar r
	     }

-- | a reporter who always returns
wrap :: Reporter a -> Reporter ( Maybe a )
wrap r = r { result = Just $ result r }

export :: Render r => Reporter a -> ( Maybe a, r )
export r = ( result r, render $ kommentar r )

run :: Render r => Reporter a -> IO ( Maybe a, r )
run r = do
    action r
    return $ export r

runs :: Reporter a -> ( Maybe a, Reporter_State )
runs r =
    ( result r, fmap undefined r )

initial :: Reporter_State
initial = fmap undefined $ return ()

runsIO r = return $ runs r

-----------------------------------------------------------

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

------------------------------------------------------------------

assert :: Bool -> Pretty.Doc -> Reporter ()
assert p doc = do
    inform doc
    nested 4 $
	 if p then inform $ Pretty.text "Ja."
	      else reject $ Pretty.text "Nein."


