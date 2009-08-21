-- -*- mode: haskell -*-
{-# LANGUAGE FlexibleContexts, UndecidableInstances, FlexibleInstances,
             TemplateHaskell #-}

--  $Id$

module Autolib.Graph.Reading.Type where

import Autolib.Graph.Kante

import Autolib.ToDoc
import Autolib.Symbol
import Autolib.Reader
import Autolib.Set
import Autolib.Hash

class ( Hash a, Ord a
     , ToDoc ( Set (Kante a)), ToDoc ( Set a )
     , Reader ( Set (Kante a)), Reader ( Set a )
     ) => GraphC a

instance ( Hash a, Ord a
     , ToDoc ( Set (Kante a)), ToDoc ( Set a )
     , Reader ( Set (Kante a)), Reader ( Set a )
     ) => GraphC a


data GraphC a => 
     Graph a = 
     Graph { knoten :: Set a
	   , kanten :: Set ( Kante a )
	   }
    deriving ( Eq, Ord )

instance  ( Hash a, ToDoc [a], Reader [a]    
          , ToDoc a, Reader a, Ord a
	  ) 
      =>   Hash ( Graph a ) where
    hash g = hash ( kanten g , knoten g )

$(derives [makeToDoc, makeReader] [''Graph])


				
