--  $Id$

module Autolib.Graph.Valid (

valid

) where

import Autolib.Graph.Type

import Data.Set
import Control.Monad ( guard )
import Autolib.ToDoc

valid :: ( ToDoc a, ToDoc [a], Ord a ) 
      => Graph a -> ( Bool, Doc )
valid g =
    let loops = do k <- setToList $ kanten g
		   guard $ von k == nach k
		   return k
	ecken = mkSet $ do k <- setToList $ kanten g
			   [ von k, nach k ]
	aussen = minusSet ecken ( knoten g )
    in	if not $ null loops
	then ( False, text "Schlingen sind nicht erlaubt:"
		      <+> toDoc loops )
	else if not $ isEmptySet aussen
	then ( False, text "Diese Kanten-Endpunkte gehören nicht zur Knotenmenge:"
		      <+> toDoc aussen )
	else ( True,  text "Das ist ein einfacher schlingenfreier Graph.")
