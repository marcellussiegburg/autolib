module Autolib.Graph.Labeling where

import Autolib.Graph.Graph
import Autolib.FiniteMap
import Autolib.ToDoc
import Autolib.Set
import Control.Monad (guard)

type Labeling a b = FiniteMap a b

the :: Maybe a -> a
the (Just x) = x
the Nothing = error "Labeling.the"

valid :: (Ord a, ToDoc a, ToDoc [a])
      => Graph a -> Labeling a b -> ( Bool, Doc )
valid g f = 
    let xs = mkSet $ keysFM f
	( fehlt, zuviel ) = symdiff ( knoten g ) xs
    in	if not $ isEmptySet fehlt
	then ( False, text "Diesen Knoten wird kein Wert zugeordnet:"
		      <+> toDoc fehlt )
	else if not $ isEmptySet zuviel
	then ( False, text "Diese Urbilder sind überhaupt keine Knoten:"
		      <+> toDoc zuviel )
	else ( True, text "Jedem Knoten wird genau ein Wert zugeordnet." )

injektiv :: ( Ord a, Ord b , ToDoc [a], ToDoc b ) 
	 => Labeling a b -> ( Bool, Doc )
injektiv f =
    let fc = addListToFM_C union emptyFM $ do
	     ( x, y ) <- fmToList f
	     return ( y, unitSet x )
	multi = do
	      ( y, xs ) <- fmToList fc
	      guard $ 1 < cardinality xs
	      return ( y, xs )
    in	if null multi
	then ( True, text "Das ist eine injektive Abbildung." )
	else ( False, fsep [ text "Die Abbildung ist nicht injektiv,"
			   , text "denn diese Bilder haben mehrere Urbiilder:"
			   , toDoc multi
			   ] )

symdiff :: Ord a => Set a -> Set a -> (Set a , Set a)
symdiff xs ys = ( minusSet xs ys, minusSet ys xs )
