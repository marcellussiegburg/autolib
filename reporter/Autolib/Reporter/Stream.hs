module Reporter.Stream 

( Type
, make
, exec
, nicht, und, oder, erster
)

where

--  $Id$

import Reporter.Type
import Output
import ToDoc
import Reporter.Iterator


-- | lazy three-valued (?) logic. 
-- Streams are produced by steps of computations.
-- A step either produces a Result (True/False),
-- or it produces a Fail,
-- or it produces some output (an Output)
-- and wants to continue with another step.
-- The idea is to have a lazy implementation
-- of the boolean connectives.

data Type
     = Cons { message :: Output
	    , activity :: IO () -- could be any monad
	    , continue :: Continue
	    }
data Continue
     = Next Type
     | Result ( Bool, String )
     | Fail String


exec :: Type -> Reporter ( Maybe Bool, String )
exec x = do
     output  $ message  x
     execute $ activity x
     case continue x of
	  Fail msg -> return ( Nothing, msg )
	  Result ( f, msg ) -> return ( Just f, msg )
 	  Next n   -> exec n

-- | the stream of outputs of the iterator (lazily)
make :: Iterator ( Bool, String ) -> Type
make ( Iterator doc prod step ) =
    let rep = do
	    start <- prod
            inform $ text "execute one step for iterator" $$ nest 4 doc
            res <- nested 4 $ step start
            inform $ text "... one step for iterator" $$ nest 4 doc
	    return res
    in  Cons { message = kommentar rep
	  , activity = action rep
	  , continue = case result rep of
	           Nothing -> Fail $ "failed: " ++ ToDoc.render doc 
		   Just x -> case x of
		       Left state -> Next $ make
	                                  $ Iterator doc (return state) step 
		       Right a -> Result a
	     }

---------------------------------------------------------------------------

nicht :: Type -> Type
nicht x = x { continue = case continue x of
    Fail msg -> Fail msg
    Result ( f, msg ) -> Result ( not f, msg )
    Next n -> Next $ nicht $ n
   }

und, oder :: [ Type ] -> Type
und  = helper "" ( Just True ) True
oder = helper "" ( Just False ) True

-- | as soon as any of the argument streams produces a result
-- this is taken as the overall result
-- and the others are stopped.
-- for this to be useful, the streams should be compatible
erster :: [ Type ] -> Type
erster = helper "" Nothing False -- impure

-- | direction is used this: if subcomputation gives result 
-- with Just x /= direction,
-- then x is immediate result. So we need
-- Just True for und, Just False for oder, Nothing for erster

-- | pure is True as long as there is no Fail in the arguments
-- this is used in case the argument list gets empty
-- if it is pure, then we can use the default result

helper :: String -> Maybe Bool -> Bool -> [ Type ] -> Type
helper inf direction pure [] = 
    Cons { message = Doc $ text inf
         , activity = return ()
	 , continue = case ( direction, pure ) of
		    ( Just d, True ) -> Result ( d, inf )
		    _                -> Fail inf
	 }
helper inf direction pure (x : xs) = 
    Cons { message = message x
	 , activity = activity x
	 , continue = case continue x of
	       Fail msg -> Next 
	             $ helper (msg ++ ", " ++ inf)
	                      direction False xs -- make impure
	       Result ( x, msg ) -> 
	           let inf' = msg ++ ", " ++ inf
	 	   in  if Just x /= direction 
		       then   Result ( x, inf' )
		       else   Next $ helper inf' direction pure   xs
	       Next n ->      Next $ helper inf  direction pure $ xs ++ [ n ]
	 }



