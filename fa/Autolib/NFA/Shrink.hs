module NFA.Shrink where

-- $Id$

import NFA.Type

import Util.Size
import Sets
import OrdFM
import Data.FiniteMap
import ToDoc
import Reporter

shrinkR :: NFAC c a
	=> NFA c a
	-> Reporter ( NFA c a )
shrinkR ( a :: NFA c a ) = do
    inform $ text "NFA.Shrink.input:" <+> toDoc (size a)
    let fm :: FiniteMap a ( FiniteMap c ( Set (Maybe a) ) )
        fm = addListToFM_C (plusFM_C (error "NFA.Shrink.plus")) emptyFM $ do
	       ((p, c), qs) <- fmToList $ trans a
	       let qs' = smap ( \ q -> if q == p then Nothing else Just q ) qs
	       return (p, listToFM [(c, qs')])
    let tab :: FiniteMap a a 
        tab = addListToFM_C (error "NFA.Shrink.add") emptyFM $ do
	       ( _ , p : qs ) <- fmToList $ invertFM fm
	       q <- qs
	       return ( q, p )
    inform $ text "tab:" <+> toDoc tab
    if 0 == sizeFM tab
       then return a
       else do
	    let fun q = lookupWithDefaultFM tab q q
	    shrinkR $ statemap fun a

invertFM :: ( Ord a, Ord b ) 
	 => FiniteMap a b
	 -> FiniteMap b [ a ]
invertFM fm = addListToFM_C (++) emptyFM $ do
    ( x, y ) <- fmToList fm
    return ( y, [x] )
