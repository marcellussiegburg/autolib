module Autolib.NFTA.Ops where

--  $Id$

import Autolib.NFTA.Type
import Autolib.NFTA.Cross
import qualified Autolib.Relation as Relation

import Autolib.ToDoc
import Autolib.Sets
import Autolib.Informed

alphamap :: ( NFTAC c s, NFTAC d s )
	 => ( c -> d )	
	 -> NFTA c s
	 -> NFTA d s
alphamap f a = 
    NFTA { nfta_info = funni "alphamap" [ info f, info a ]
	 , alphabet = smap f $ alphabet a
	 , states = states a
	 , finals = finals a
	 , trans = Relation.rightmap ( \ (c,qs) -> (f c, qs) )
	 	$ trans a
	 , eps   = eps a
	 }

statemap :: ( NFTAC c s, NFTAC c t )
	 => ( s -> t )	
	 -> NFTA c s
	 -> NFTA c t
statemap f a = 
    NFTA { nfta_info = funni "statemap" [ info f, info a ]
	 , alphabet = alphabet a
	 , states = smap f $ states a
	 , finals = smap f $ finals a
	 , trans = Relation.bothmap f ( \ (c,qs) -> (c, map f qs))
	 	$ trans a
	 , eps   = Relation.bothmap f f $ eps a
	 }


statefilter :: ( NFTAC c s )
	 => ( s -> Bool )	
	 -> NFTA c s
	 -> NFTA c s
statefilter f a = 
    NFTA { nfta_info = funni "statefilter" [ info a ]
	 , alphabet = alphabet a
	 , states = sfilter f $ states a
	 , finals = sfilter f $ finals a
	 , trans = Relation.make 
	         $ filter ( \ (p, (c, qs)) -> and $ map f $ p : qs )
                 $ Relation.pairs $ trans a
	 , eps   = Relation.make 
	         $ filter ( \ (p, q) -> and $ map f [p, q] )
                 $ Relation.pairs $ eps a
	 }

alphafilter :: ( NFTAC c s )
	 => ( c -> Bool )	
	 -> NFTA c s
	 -> NFTA c s
alphafilter f a = 
    NFTA { nfta_info = funni "alphafilter" [ info a ]
	 , alphabet = sfilter f $ alphabet a
	 , states = states a
	 , finals = finals a
	 , trans = Relation.make 
	         $ filter ( \ (p, (c, qs)) -> f c )
                 $ Relation.pairs $ trans a
	 , eps   = eps a
	 }

intersection :: ( NFTAC c s, NFTAC c t )
             => NFTA c s -> NFTA c t
             -> NFTA c (s, t)
intersection a b = 
    ( Autolib.NFTA.Cross.cross a b )
    { finals = Autolib.Sets.cross (finals a) (finals b) }

