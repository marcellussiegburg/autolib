module Autolib.TES.Linear where

--  $Id$

import Autolib.TES.Term
import Autolib.TES.Position
import Autolib.TES.Data
import Autolib.Reporter
import Autolib.ToDoc
import Autolib.FiniteMap


-- |  check if term is linear
-- if not: fail, if yes: return ()
is_linear_term :: TRSC v c
	  => Term v c 
	  -> Reporter ()
is_linear_term t = do
    inform $ text "term should be linear:" <+> toDoc t
    let more = nonlinearities t
    when ( not $ null more )  
	 $ reject 
	 $ text "multiple occurences of:" <+> toDoc more

-- | check if system is left_linear
-- if not: fail, if yes: return ()
is_left_linear_trs :: TRSC v c
	  => TRS v c
	  -> Reporter ()
is_left_linear_trs trs = do
    inform $ text "system should be left linear:"
    mapM_ is_linear_term $ lhss trs

is_right_linear_trs :: TRSC v c
	     => TRS v c
	     -> Reporter ()
is_right_linear_trs trs = do
    inform $ text "system should be right linear"
    mapM_ is_linear_term $ rhss trs

-- | check if system is linear
-- if not: fail, if yes: return ()
is_linear_trs :: TRSC v c
	  => TRS v c
	  -> Reporter ()
is_linear_trs trs = do
    inform $ text "system should be linear:"
    mapM_ is_linear_term $ lhss trs
    mapM_ is_linear_term $ rhss trs

nonlinearities :: TRSC v c
	  => Term v c 
	  -> [ (v, Int) ]
nonlinearities t = do 
    ( v, n ) <- fmToList $ multiplicities t
    guard $ n > 1
    return ( v, n )

-- | FM of variables with multiplicities
multiplicities :: Ord v 
      => Term v c -> FiniteMap v Int
multiplicities t = addListToFM_C (+) emptyFM 
	$ do Var v <- subterms t
	     return ( v, 1 )

