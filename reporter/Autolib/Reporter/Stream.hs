module Reporter.Stream 

( Type
, make
, exec
, nicht, und, oder, erster
, module Output
, module Reporter.Proof
)

where

--  $Id$

import Reporter.Type
import Output
import ToDoc
import Reporter.Iterator
import Reporter.Proof

-- | lazy constructive (?) logic. 
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
     | Result Proof
     | Fail Output


exec :: Type -> Reporter ( Either Output Proof )
exec x = do
     output  $ message  x
     execute $ activity x
     case continue x of
	  Fail msg -> return $ Left msg
	  Result p -> return $ Right p
 	  Next n   -> exec n

-- | the stream of outputs of the iterator (lazily)
make :: Iterator Proof -> Type
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
	           Nothing -> Fail $ Above (Text "failed:") (Nest $ Doc doc)
		   Just x -> case x of
		       Left state -> Next $ make
	                                  $ Iterator doc (return state) step 
		       Right a -> Result a
	     }

---------------------------------------------------------------------------

nicht :: Type -> Type
nicht x = x { continue = case continue x of
    Fail msg -> Fail msg
    Result p -> Result $ p { value = not $ value p
			   , reason = Above (Text "not") (Nest $ reason p)
			   }
    Next n -> Next $ nicht $ n
   }

und, oder :: [ Type ] -> Type
und  = helper Empty ( Just $ Proof { value = True
				, reason = Text "empty und"
				}
		 ) True
oder = helper Empty ( Just $ Proof { value = False 
				, reason = Text "empty oder"
				}
		 ) True

-- | as soon as any of the argument streams produces a result
-- this is taken as the overall result
-- and the others are stopped.
-- for this to be useful, the streams should be compatible
erster :: [ Type ] -> Type
erster = helper Empty Nothing False -- impure

-- | direction is used this: if subcomputation gives result 
-- with Just x /= direction,
-- then x is immediate result. So we need
-- Just True for und, Just False for oder, Nothing for erster

-- | pure is True as long as there is no Fail in the arguments
-- this is used in case the argument list gets empty
-- if it is pure, then we can use the default result

helper :: Output -> Maybe Proof -> Bool -> [ Type ] -> Type
helper inf direction pure [] = 
    Cons { message = inf
         , activity = return ()
	 , continue = case ( direction, pure ) of
		    ( Just d, True ) -> Result $ d { reason = Above (reason d) inf }
		    _                -> Fail inf
	 }
helper inf direction pure (x : xs) = 
    Cons { message = message x
	 , activity = activity x
	 , continue = case continue x of
	       Fail msg -> Next 
	             $ helper (Above msg  inf)
	                      direction False xs -- make impure
	       Result p -> 
	           let inf' = Above (reason p) inf
	 	   in  if Just p /= direction -- use Eq instance
		       then   Result $ p { reason = inf' }
		       else   Next $ helper inf' direction pure   xs
	       Next n ->      Next $ helper inf  direction pure $ xs ++ [ n ]
	 }



