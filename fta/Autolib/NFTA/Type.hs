{-# LANGUAGE TemplateHaskell #-}

module Autolib.NFTA.Type 

( module Autolib.NFTA.Type
, module Autolib.Set
, module Autolib.Symbol
)

where

import Autolib.Set
import Autolib.Letters
import Autolib.Util.Size
import Autolib.Symbol
import Autolib.Reader
import Autolib.ToDoc
import qualified Autolib.Relation as Relation
import Autolib.Hash
import Autolib.Informed

class ( Symbol c, ToDoc [c], Reader [c]
      , Ord s , ToDoc s
      , ToDoc [s], Reader [s]
      , Reader s
      , Hash c, Hash s
      ) => NFTAC c s

instance ( Symbol c, ToDoc [c], Reader [c]
      , Ord s , ToDoc s
      , ToDoc [s], Reader [s]
      , Reader s
      , Hash c, Hash s
      ) => NFTAC c s

data ( NFTAC c s ) => NFTA c s = 
     NFTA { nfta_info :: Doc

	  , alphabet :: Set c
	  , states :: Set s
	  -- | if we think Top-Down, the name should be 'starts'
	  , finals :: Set s

	  -- | @( p, f, [q1, .. ] )@ means @transition p <- f [q1, .. ]@
	  , trans :: Relation.Type s (c,  [s]) 

	  -- | the following can now be obtained from Relation.Type directly:
	  -- @, inv_trans :: Relation.Type ([s] , c) s@

	  --  @( p, q )@ means transition  @p <- q@ 
	  --  (in accepting bottom-up computation)
	  -- this relation is always kept transitive
	  -- self-loops are not normally included
	  , eps     :: Relation.Type s s 
	  -- ^ the following can now be obtained from Relation.Type directly:
	  -- and this is always the inverse of above
	  -- @, inv_eps :: Relation.Type s s@
	  }



instance NFTAC c s
    => Informed (NFTA c s) where
    info = nfta_info
    informed i a = a { nfta_info = i }

essence a = ( finals a, trans a , eps a )

instance NFTAC c s => Eq ( NFTA c s ) where
    a == b = essence a == essence b

instance ( NFTAC c s ) => Hash ( NFTA c s ) where
    hash = hash . essence

self_eps :: NFTAC c s
	 => NFTA c s -> Relation.Type s s
self_eps a = Relation.flat ( states a ) `Relation.plus` eps a

inv_self_eps :: NFTAC c s
	 => NFTA c s -> Relation.Type s s
inv_self_eps a = Relation.inverse $ self_eps a

lstates :: NFTAC c s => NFTA c s -> [s]
lstates = setToList . states

lfinals :: NFTAC c s => NFTA c s -> [s]
lfinals = setToList . finals

ltrans :: NFTAC c s => NFTA c s -> [(s, c, [s])]
ltrans  a = do ( p, (c, qs )) <- Relation.pairs ( trans a )
	       return ( p, c, qs )

leps :: NFTAC c s => NFTA c s -> [ (s, s) ]
leps = Relation.pairs . eps

instance  NFTAC c s => Letters (NFTA c s) c where
    letters a = mkSet $ do
        ( p, c, qs ) <- ltrans a
	return c

instance NFTAC c s  => Size ( NFTA c s ) where
    size = cardinality . states

$(derives [makeToDoc, makeReader] [''NFTA])

instance NFTAC c s => Show ( NFTA c s) where
     show = render . toDoc