{-# OPTIONS -fglasgow-exts #-}

module Autolib.Reporter.Classic.Type 

where

import Autolib.Output
import Autolib.Multilingual

-- import qualified Autolib.Multilingual.Doc as Pretty
import Autolib.ToDoc hiding ( render )

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

    fail msg = reject $ text $ "*** fail: " ++ msg


output :: Output -> Reporter ()
output o = Reporter 
	 { result = Just () 
	 , action = return () 
	 , kommentar = o 
	 , transformer = id
	 }
    
reject :: Doc -> Reporter a
reject d = Reporter 
	 { result = Nothing
	 , action = return ()
	 , kommentar = Doc d 
	 , transformer = id
	 }

--------------------------------------------------------------------------

inform :: Doc -> Reporter ()
inform = output . Doc

newline :: Reporter ()
newline = inform ( text " " )

nested :: Int -> Reporter a -> Reporter a
nested d r = r { kommentar = Nest $ kommentar r }

repo :: Reporter a -> Doc
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

run :: Reporter a -> IO ( Maybe a, Output )
run r = do
    action r
    return ( result r, kommentar r )

runs :: Reporter a -> ( Maybe a, Reporter_State )
runs r =
    ( result r, fmap undefined r )

initial :: Reporter_State
initial = fmap undefined $ return ()

runsIO r = return $ runs r

-----------------------------------------------------------

-- for use in challenger problems
-- flag is true  iff  reporter returns at all (with any value)
lazy_cheporter :: Reporter a -> ( Bool, Doc )
lazy_cheporter r = 
    ( isJust $ result r
    , render $ kommentar r
    )    

cheporter :: Reporter Bool -> ( Bool, Doc )
cheporter r = lazy_cheporter $ do
    f <- r
    assert f $ multitext [ (DE, "Bedingung erfüllt?")
			 , (UK, "Condition satisfied?")
			 ]

porterche :: ( Bool, Doc ) -> Reporter ()
porterche ( p, d ) = if p then inform d else reject d 

------------------------------------------------------------------

assert :: Bool -> Doc -> Reporter ()
assert p doc = do
    inform doc
    nested 4 $
	 if p then inform $ multitext [ (DE, "Ja."), (UK, "Yes.") ]
	      else reject $ multitext [ (DE, "Nein."), (UK, "No.") ]



