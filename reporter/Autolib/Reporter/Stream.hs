module Autolib.Reporter.Stream 

( Type
, make
, exec
, nicht, erfolg, und, oder, erster, before
, module Autolib.Output
, module Autolib.Reporter.Proof
)

where

--  $Id$

import Autolib.Reporter.Type
import Autolib.Output
import Autolib.ToDoc
import Autolib.Reporter.Boolean.Type
import Autolib.Reporter.Iterator
import Autolib.Reporter.Proof

-- | lazy constructive logic. 
-- Streams are produced by steps of computations.
-- A step either produces a Result (Boolean)
-- or it produces a Fail,
-- or it produces some output (an Output)
-- and wants to continue with another step.
-- The idea is to have a lazy implementation
-- of the boolean connectives.
data Type
     = Cons { cons_info :: Doc
	    , continue :: Reporter Continue
	    }

data Continue
     = Next Type
     | Result Proof
     | Fail Output

exec :: Type -> Reporter ( Either Output Proof )
exec x = do
     c <- continue x
     case c of
	  Fail msg -> return $ Left msg
	  Result p -> return $ Right p
 	  Next n   -> exec n

-- | the stream of outputs of the iterator (lazily)
make :: Doc -> Iterator Proof -> Type
make d ( Iterator doc prod step ) = Cons 
          { cons_info = d
	  , continue = do
	    start <- prod
            inform $ text "execute one step for iterator" $$ nest 4 doc
            res <- nested 4 $ wrap $ step start
            inform $ text "... one step for iterator" $$ nest 4 doc
            return $ case res of
	           Nothing -> Fail $ Above (Doc $ text "failed:") 
					   (Nest $ Doc doc)
		   Just x -> case x of
		       Left state -> Next $ make d
	                                  $ Iterator doc (return state) step 
		       Right a -> Result a
	     }

-----------------------------------------------------------------------


nicht :: Doc -> Type -> Type
nicht doc x = x { continue = do
    c <- continue x
    return $ case c of 
        Fail msg -> Fail msg
        Result p -> Result $ Proof 
            	{ value = not $ value p
            	, formula = doc
            	, reason = explain p
            	, history = [] -- history p
            	}
        Next n -> Next $ nicht doc $ n
   }

-- | report True in case of success
-- and False (not Fail) in case of failure
erfolg :: Doc -> Type -> Type
erfolg doc x = x { continue = do
    c <- continue x
    return $ case c of
        Fail msg -> Result $ Proof 
            	{ value = False
            	, formula = doc
            	, reason = msg
            	, history = [] 
            	}
        Result p -> Result $ Proof 
            	{ value = True
            	, formula = doc
            	, reason = explain p
            	, history = [] -- history p
            	}
        Next n -> Next $ erfolg doc $ n
   }

und, oder :: Doc -> [ Type ] -> Type
und  doc = helper doc ( Just $ Proof { value = True
			     , formula = doc
			     , reason = Doc $ text "empty conjunction"
			     , history = []
			     }
		 ) True
oder doc = helper doc ( Just $ Proof { value = False 
			      , formula = doc
				, reason = Doc $ text "empty alternative"
				, history = []
				}
		 ) True


-- | evaluate first stream. if it returns result, then this is it.
-- if it fails, evaluate second stream etc.
before :: Doc -> [ Type ] -> Type
before doc [] = 
    Cons { cons_info = doc
	 , continue = return $ Fail Empty -- FIXME
         }
before doc (x : xs) = 
    Cons { cons_info = doc
	 , continue = do
            c <- continue x
            return $ case c of
	       Fail msg -> Next -- FIXME: collect msg
	             $ before doc xs
	       Result p -> 
		   Result $ Proof
				{ value = value p
				, formula = doc
				, reason = explain p
				, history = [] -- history p
				}
	       Next x' -> Next $ before doc ( x' : xs )
	 }


-- | as soon as any of the argument streams produces a result
-- this is taken as the overall result
-- and the others are stopped.
-- for this to be useful, the streams should be compatible
erster :: Doc ->  [ Type ] -> Type
erster doc = helper doc Nothing False -- impure

-- | direction is used this: if subcomputation gives result 
-- with Just x neq direction,
-- then x is immediate result. So we need
-- Just True for und, Just False for oder, Nothing for erster
-- pure is True as long as there is no Fail in the arguments
-- this is used in case the argument list gets empty
-- if it is pure, then we can use the default result
helper :: Doc -> Maybe Proof -> Bool -> [ Type ] -> Type
helper doc  direction pure [] = 
    Cons { cons_info = doc
	 , continue = case ( direction, pure ) of
	      ( Just d, True ) -> 
	          return $ Result $ Proof 
			 { value = value d
			 , formula = doc
			 , reason = explain d
			 , history = [] -- history d
			 }
	      _  -> return $ Fail Empty -- ??
	 }

helper doc direction pure xxs @ (x : xs) = Cons 
       { cons_info = doc 
       , continue = do 
         c <- continue x 
         return $ case c of
               Fail msg -> Next -- FIXME: collect msg
                     $ helper doc direction  False xs -- make impure
               Result p -> 
                   if Just p /= direction -- use Eq instance
                   then  Result $ Proof
                                { value = value p
                                , formula = doc
                                , reason = explain p
                                , history = [] -- history p
                                }
                   else  Next $ helper doc direction --  collect ( reason p ) )
                                 pure   xs
               Next n -> Next $ helper doc direction pure $ xs ++ [ n ]

       }


