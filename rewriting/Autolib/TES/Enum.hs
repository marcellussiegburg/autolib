module TES.Enum where

-- | enumerate terms over given signature
-- ideally, complete enumeration
-- and random generation should share almost all code

import TES.Term
import TES.Data
import TES.Identifier
import TES.Position
import TES.Sexp

import System.Random
import Control.Monad.State

-- | class that describes choice:
class Monad g => Pick g where
    pick :: [a] -> g a

instance Pick [] where
    -- take all
    pick xs = xs 

instance RandomGen g => Pick (State g) where
    -- take one
    pick xs = do
        g <- get
	let ( i, g' ) = randomR (0, pred $ length xs) g
	put g'
	return $ xs !! i

-- | class that describes choosables
class Choose a b where
      choose :: Pick p => a -> Int -> p b

instance ( Choose a b, Choose a c ) => Choose a (b, c) where
    choose a s = do
        sl <- pick [ 1 .. s - 1 ]
	l  <- choose a sl
	let sr = s - sl
	r  <- choose a sr
	return ( l, r )

-- | restricted case: binary symbol and nullary symbols
data Binu c = Binu
	  { binary  :: [c]
	  , unary   :: [c]
	  , nullary :: [c]
	  }

b = Binu { binary  = [ mkbinary "f" ]
	 , unary   = []
	 , nullary = [ mknullary "a" ]
	 }

instance Symbol c => Choose (Binu c) (Term a c) where
    choose conf s = 
        if s < 2 
	then do
	     c <- pick $ nullary conf
	     return $ Node c []
	else do
	     flag <- pick [ False, True ]
	     if flag 
		then do
		     (l, r) <- choose conf s
		     c <- pick $ binary conf
		     return $ Node c [ l, r ]
		else do
		     l <- choose conf (pred s)
		     c <- pick $ unary conf
		     return $ Node c [l]

-------------------------------------------------------------------------

-- | a single linear size-increasing rule
instance ( Symbol c , Choose a (Term Identifier c) )
    => Choose a (RS (Term Identifier c)) where
    choose conf s = do
        sl <- pick  [ 1 .. s `div` 2 ]
        l  <- choose conf sl
	let sr = s - sl
	r <- choose conf sr
	p <- pick $ leafpos l
	q <- pick $ leafpos r
	let x = mknullary "x" 
        let trs = from_rules False 
	        $ [ ( poke l (p, Var x), poke r (q, Var x) ) ]
	    anno = [ List [ Leaf "VAR", Leaf $ show x ]
		   ]
	return $ trs { annotations = anno }


