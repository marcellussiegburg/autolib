-- | enumerate terms over given signature
-- ideally, complete enumeration
-- and random generation should share almost all code

module TES.Enum where

import TES.Term
import TES.Data
import TES.Identifier
import TES.Position
import TES.Sexp

import Autolib.Util.Splits
import Autolib.Util.Zufall

import Data.FiniteMap
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

instance Pick IO where
    pick = eins


-- | class that describes choosables
class Choose a b where
      choose :: Pick p => a -> Int -> p b

-- | lazy infinite list
cache  :: ( Choose a b, Pick p ) => a -> [ p b ]
cache a = do s <- [ 0 .. ] ; return $ choose a s

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
	     flag <- pick $ [ False | not $ null $ unary conf ] ++ [ True ]
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
        l <- choose conf sl 
	let sr = s - sl
	r <- choose conf sr
        insert_variable ( mknullary "x" ) ( l, r )

insert_variable :: Pick p 
		=> Identifier 
	        -> ( Term Identifier c, Term Identifier c )
		-> p ( RS (Term Identifier c))
insert_variable v ( l, r ) = do
	p <- pick $ leafpos l
	q <- pick $ leafpos r
	let x = mknullary "x" 
        let trs = from_rules False 
	        $ [ ( poke (vmap undefined l) (p, Var x)
		    , poke (vmap undefined r) (q, Var x) 
		    ) 
		  ]
	    anno = [ List [ Leaf "VAR", Leaf $ show x ]
		   ]
	return $ trs { annotations = anno }

-------------------------------------------------------------------------

data Plain = Plain

mach k = mk k $ show k

instance Choose Plain a => Choose Plain [a] where
    choose Plain s | s == 0 = do
        return []
    choose Plain s | s  > 0 = do
	t  <- pick [ 1 .. s ] 
	x  <- choose Plain t 
	xs <- choose Plain (s - t) 
	return $ x : xs 

instance Choose Plain ( Term a Identifier ) where
    choose Plain s | s  >= 1  = do
        xs <- choose Plain ( s - 1 )
	return $ Node ( mach $ length xs ) xs

-----------------------------------------------------------------------------

-- | combine 4 = [[4], [3,1], [2,2], [2,1,1], [1,1,1,1]]
combine i = combi i i
combi b 0 = return []
combi b k | k > 0 = do
    x <- pick [ 1 .. min k b ]
    xs <- combi x (k - x)
    return $ x : xs

-- | each permutation
permute :: Pick p => [(a, Int)] -> p [a]
permute [] = return []
permute things = do
    ( pre, (x,i) : post ) <- pick $ splits things
    xs <- permute $ pre ++ [ (x, pred i) | i > 1 ] ++ post
    return $ x : xs

-- | lexicographically rising
ordered_permute :: Pick p => [(a, Int)] -> p [a]
ordered_permute things = 
    helper $ do (x,i) <- things ; return (True,x,i)
    
helper [] = return []
helper fxis = do
    ( pre, (f,x,i) : post ) <- pick 
        $ filter ( \ (pre, _) -> all (\ (f,x,i) -> not f) pre)
	$ splits fxis
    xs <- helper $ pre ++ [ (False, x, pred i) | i > 1 ] ++ post
    return $ x : xs


pairs :: Int
      -> [ ( Term Identifier Identifier, Term Identifier Identifier ) ]
pairs s = do
    r <- terms False s
    let sig = addListToFM_C (++) emptyFM $ do
          c <- lsyms r
	  return ( arity c, [c] )
    t <- pick [ 1 .. s ]  
    l0 <- choose Plain t
    l <- painted sig $ l0
    return ( vmap undefined l, vmap undefined r )

rules :: Identifier
      -> [ RS ( Term Identifier Identifier ) ]
rules v = do
    s <- [ 1 .. ]
    (l0, r0) <- pairs s
    rs <- insert_variable v (l0, r0)
    guard $ or $ do 
       -- some variables at least at depth 2
       (l, r) <- TES.Data.rules rs
       p <- varpos r 
       return $ 1 < length p
    guard $ or $ do
       -- some symbols have arity > 1
       (l, r) <- TES.Data.rules rs
       c <- symsl r
       return $ 1 < arity c

    return rs


painted :: FiniteMap Int [ Identifier ]
        -> Term d Identifier
        -> [ ( Term c Identifier ) ]
painted fm ( Node _ args ) = do
    let ds = lookupWithDefaultFM fm [] ( length args )
    d <- pick ds
    xs <- mapM (painted fm) args
    return $ Node d xs

roll :: Pick p
     => (Int, FiniteMap Identifier Int)
     -> p (Int, [Identifier])
roll (a, fm) = do
    cs <- permute $ fmToList fm
    return (a, cs)

terms :: Pick p 
      => Bool
      -> Int 
      -> p ( Term () Identifier )
terms full s = do
    -- find skeleton
    t <- choose Plain s
    decorate full t

decorate :: Pick p 
         => Bool
	 -> Term a Identifier
         -> p ( Term b Identifier )
decorate full t = do
    sub <- distribute full $ count t
    return $ markup t sub

-- | count arities of symbols
count t = addListToFM_C (+) emptyFM $ do 
	   c <- symsl t
	   return ( arity c, 1 )

distribute :: Pick p 
	   => Bool
           -> FiniteMap Int Int 
           -> p ( FiniteMap Int [ Identifier ] )
distribute full count = do
    xs <- mapM ( distro full ) $ fmToList count
    return $ listToFM xs
    
-- | we need n pieces of a-ary symbols
distro :: Pick p
       => Bool
       -> ( Int, Int ) 
       -> p ( Int, [ Identifier ] )
distro full (a, n) = do
    let ss = do i <- [ 0 .. ] ; return $ einmach a i
    cs <- combine n
    ps <- ( if full then permute else ordered_permute )
	  $ zip ss cs
    return ( a, ps )        


einmach a i = mk a $ [ 'a' .. ] !! a : show i

markup :: Term a b
       -> FiniteMap Int [ c ]
       -> Term d c 
markup t sub = evalState ( mu t ) $ sub
mu ( Node _ args ) = do
    fm <- get
    let Just (d : ds) = lookupFM fm (length args)
    put $ addToFM fm (length args) ds
    xs <- mapM mu args
    return $ Node d xs
