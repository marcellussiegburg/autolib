module Reporter.Stream 

( Type
, make
, exec
, nicht, und, oder
)

where

-- $Id$

import Reporter.Type
import Output
import ToDoc
import Reporter.Iterator


{- 

lazy three-valued (?) logic. 

Streams are produced by steps of computations.
A step either produces a Result (True/False),
or it produces a Fail,
or it produces some output (an Output)
and wants to continue with another step.

The idea is to have a lazy implementation
of the boolean connectives.

-}

data Type
     = Cons { message :: Output
	    , continue :: Continue
	    }
data Continue
     = Next Type
     | Result Bool
     | Fail


exec :: Type -> Reporter Bool
exec x = do
     output $ message x
     case continue x of
	  Fail     -> reject $ text "no result"
	  Result r -> return r
 	  Next n   -> exec n

make :: Iterator Bool -> Type
-- the stream of outputs of the iterator (lazily)
make ( Iterator doc step start ) =
    let rep = do
            inform $ text "execute one step for iterator" <+> doc
            res <- nested 4 $ step start
            inform $ text "... one step for iterator" <+> doc
	    return res
    in  Cons { message = kommentar rep
	  , continue = case result rep of
	           Nothing -> Fail
		   Just x -> case x of
		       Left state -> Next $ make ( Iterator doc step state )
		       Right a -> Result a
	     }

nicht :: Type -> Type
nicht x = x { continue = case continue x of
    Fail -> Fail
    Result f -> Result $ not f
    Next n -> Next $ nicht $ n
   }

und, oder :: [ Type ] -> Type
und  = helper True True
oder = helper False True

helper :: Bool -> Bool -> [ Type ] -> Type
-- direction: True for und, False for oder
helper direction pure [] = 
    Cons { message = Doc $ text "helper []"
	 , continue = case pure of
		    True -> Result direction
		    False -> Fail
	 }
helper direction pure (x : xs) = 
    Cons { message = message x
	 , continue = case continue x of
	       Fail -> Next $ helper direction False xs -- make impure
	       Result x -> if x /= direction 
		  then Result x
		  else   Next $ helper direction pure   xs
	       Next n -> Next $ helper direction pure $ xs ++ [ n ]
	 }



