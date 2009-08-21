{-# language IncoherentInstances, OverlappingInstances, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, DeriveDataTypeable #-}

-- | Graph Datenstruktur mit elementaren Klasseninstanzen

-- autor Georg Martius
-- mai99dgf@studserv.uni-leipzig.de

--  $Id$


module Autolib.Graph.Graph ( 
      module  Autolib.Set
      , module Autolib.Informed
      , GraphC 
     ,  Graph (..), mkGraph
    , Kante (..), kante
    , Layout_Program (..)
) where

import Autolib.Graph.Kante
import qualified Autolib.Graph.Reading.Type as R

import Autolib.Set
import Autolib.Dot
import Autolib.Informed
import Autolib.ToDoc
import Autolib.Reader

import Autolib.Boxing.Position
import Autolib.FiniteMap

import Autolib.Xml
import Autolib.Size
import Autolib.Hash

import Autolib.XmlRpc

import Network.XmlRpc.Internals 

import Data.Typeable



-----------------------------------------------------------------

data GraphC a => Graph a  = Graph
	      { knoten    :: Set a
	      , kanten    :: Set (Kante a)
	      -- neue komponenten (nicht show/read-fähig)
	      , graph_info :: Doc
	      , graph_texinfo :: String
	      -- partiell (entspricht pinning in neato)
	      , graph_layout  :: FiniteMap a Position
	      , bounding :: Position
	      , layout_hints :: [ String ]
	      , layout_program :: Layout_Program
	      , show_labels :: Bool
	      } 
    deriving Typeable

instance GraphC a => XmlRpcType ( Graph a ) where
    toValue g = toValue [ ("knoten"  , toValue $ knoten g )
                        , ("kanten" , toValue $ kanten g )
                        ]
    fromValue ( ValueStruct v ) = do
                  f <- getField "knoten" v
                  ff <- fromValue f
                  s <- getField "kanten" v
                  ss <- fromValue s
                  return $ mkGraph ff ss
    getType _ = TStruct


instance GraphC a =>  Informed ( Graph a) where
    info = graph_info
    informed i g = g { graph_info = i }
    texinfo = graph_texinfo
    texinformed i g = g { graph_texinfo = i }

instance GraphC a 
	 => Size ( Graph a ) where
    size g = cardinality $ knoten g

mkGraph :: GraphC a 
	=> Set a -> Set (Kante a) -> Graph a
mkGraph v e = Graph
	    { knoten = v
	    , kanten = e
	    , graph_info = text "mkGraph"
	    , graph_texinfo = "G"
	    , graph_layout = emptyFM
	    , bounding = 0
	    , layout_hints = [ "-s" ]
	    , layout_program = Neato
	    , show_labels = True
	    }

class ( Ord a, ToDoc a, ToDoc [a], Reader a, Reader [a]
      , Show a
      , Hash a
      , R.GraphC a
      , Typeable a
      , XmlRpcType ( Set a )
      , XmlRpcType ( Set ( Kante a ))
      ) => GraphC a 


instance ( Ord a, ToDoc a, ToDoc [a], Reader a, Reader [a]
      , Show a
      , Hash a
      , R.GraphC a
      , Typeable a
      , XmlRpcType ( Set a )
      , XmlRpcType ( Set ( Kante a ))
      ) => GraphC a 


instance GraphC a 
    => Container ( Graph a ) ( R.Graph a ) where
    label _ = "Graph"
    pack g =  R.Graph { R.knoten = knoten g 
		      , R.kanten = kanten g
   		      }
    unpack h = mkGraph ( R.knoten h ) (R.kanten h )

instance GraphC a => Reader ( Graph a ) where
    readerPrec d = do
        h <- readerPrec d 
        return $ unpack h

instance GraphC a => ToDoc ( Graph a ) where
        toDocPrec p g = toDocPrec p 
		      $ pack g

instance GraphC a => Eq ( Graph a ) where
    g == h = pack g == pack h

instance GraphC a => Ord ( Graph a ) where
    compare g h = compare (pack g) (pack h)

-- | todo: cache this (add new component graph_hash to Graph type)
instance GraphC a => Hash ( Graph a ) where
    hash g = hash $ pack g





