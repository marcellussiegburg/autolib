module TES.Hull where

--  $Id$

import Sets
import Control.Monad (mzero)
import Control.Monad.State
import Data.List ( insert, nub )
import Data.Maybe (isJust)

data Reactor w a t = 
     Reactor { weight :: a -> w
	     , tag :: a -> t -- for uniqueness
	     , op :: a -> a -> [a]
	     , start :: [a]
	     , done :: Set ( w, a )
	     , todo :: Set ( w, a )
	     , tags :: Set t
	     }
     deriving Show

class (Ord w, Ord a, Ord t ) => ReactorC w a t
instance (Ord w, Ord a, Ord t ) => ReactorC w a t

-- | get x from todo with smallest weight
-- and put it into done
pop :: ( ReactorC w a t )
    => State ( Reactor w a t ) ( Maybe a )
pop = do
    r <- get
    case setToList $ todo r of
	 [] -> return Nothing
	 (w, x) : _ -> do
	     put $ r { todo = delFromSet ( todo r ) (w,x)
		     , done = addToSet   ( done r ) (w,x)
		     -- , tags = addToSet   ( tags r ) (tag r x)
		     }	 
	     return $ Just x

-- | insert into todo set (if not already in done set)
push :: ( ReactorC w a t )
     => a -> State ( Reactor (Maybe w) a t ) [a]
push x = do
    r <- get
    let w = weight r x
	wx = ( w, x )
    if (  ( not $ elementOf wx $ done r ) 
       && ( not $ elementOf (tag r x) $ tags r )
       && ( isJust w )
       )
       then do put $ r { todo = addToSet ( todo r ) wx 
		       , tags = addToSet ( tags r ) ( tag r x )
		       }
	       return [x]
       else do return [ ]

pushes xs = do
    xss <- mapM push xs
    return $ concat xss
    
contents :: ( ReactorC w a t )
	 => State ( Reactor w a t ) [ a ]
contents = do
    r <- get
    return $ map snd $ setToList $ done r

work :: ( ReactorC w a t )
     => State ( Reactor (Maybe w) a t ) [ a ]
work = do
    mx <- pop
    case mx of
        Nothing -> return []
	Just x  -> do
	    r <- get
	    ys <- contents
	    xs <- pushes $ do y <- ys ; op r x y ++ op r y x
			   ++ op r x x 
	    zs <- work
	    return $ xs ++ zs

burn :: ( ReactorC w a t )
	=> ( a -> Maybe w )   -- ^ weight (prefer smaller weights, remove Noth)
	-> ( a ->  t ) -- ^ tagging
	-> ( a -> a -> [a] ) -- ^ next generation
	-> [ a ] -- ^ initial elements
	-> [ a ] -- ^ generated
burn w t o s = s ++ evalState work 
	( Reactor { weight = w, op = o, start = s, tag = t
		  , done = emptySet , tags = emptySet
		  , todo = mkSet $ do x <- s ; return ( w x, x )
		  }
	)

-------------------------------------------------------------------------

reactor :: ( Ord a, Ord b )
	=> ( a -> Maybe b )   -- ^ weight (prefer smaller weights, remove Noth)
	-> ( a -> a -> [a] ) -- ^ next generation
	-> [ a ] -- ^ initial elements
	-> [ a ] -- ^ generated
reactor weight op start = start ++
        handle ( mkSet $ annotate start ) ( annotate start )
    where annotate xs = do x <- xs ; return ( weight x, x )
	  handle done [] = []
	  handle done ((Nothing, x) : wxs) = 
	      handle done wxs
	  handle done ((Just w, x) : wxs) =  
	      let new = filter ( not . ( `elementOf` done ) ) 
		      $ filter ( \ (w, x) -> isJust w )
		      $ annotate 
		      $ ( do (w, y) <- setToList done ; op x y ++ op y x) 
			++ ( op x x )
	      in  map snd new 
		     ++  handle ( done `union` mkSet new )
			 ( setToList $ union ( mkSet new ) ( mkSet wxs ) )

	  
-- | extend by one step 
single_hull :: Ord a 
     => (a -> a -> [a]) -- ^ associative operation
     -> [a] -- ^ initial elements
     -> [a] 
single_hull op base = help (mkSet base) base where
    help done [] = []
    help done xs = xs ++
        let ys = do x <- xs ; b <- base ; op x b 
	    new = nub $ filter ( not . ( `elementOf` done ) ) ys
	in  help ( done `union` mkSet new ) new 

-- | combine totally (many steps)
full_hull :: Ord a 
     => (a -> a -> [a]) -- ^ associative operation
     -> [a] -- ^ initial elements
     -> [a] 
full_hull op base = help (mkSet base) base where
    help done [] = []
    help done xs = xs ++
        let ys = do x <- xs ; b <- setToList done ; op x b 
	    new = nub $ filter ( not . ( `elementOf` done ) ) ys
	in  help ( done `union` mkSet new ) new 

