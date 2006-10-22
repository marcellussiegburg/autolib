module Autolib.Reporter.Set where

--  $Id$

import Autolib.Reporter.Type
import Autolib.Set
import Autolib.ToDoc
import Control.Monad ( when , guard )

-------------------------------------------------------------------------------

subeq :: ( Ord a , ToDoc [a] ) => (Doc,Set a) -> (Doc,Set a) -> Reporter ()
subeq ( d1, s1 ) ( d2, s2 ) = do
    inform $ vcat
	   [ text "Ist die Menge"
	   , nest 4 $ fsep [ d1, equals, toDoc s1 ]
	   , text "Teilmenge der Menge"
	   , nest 4 $ fsep [ d2, equals, toDoc s2 ]
	   , text "?"
	   ]
    let no = minusSet s1 s2
    when ( not $ isEmptySet no ) $ reject $ vcat
	     [ text "Nein, diese Elemente sind"
	     , text "in" <+> d1 
	     , text ", aber nicht in" <+> d2
	     , toDoc no
	     ]
    inform $ text "Ja."

-------------------------------------------------------------------------------

proper_subset :: ( Ord a , ToDoc [a] ) 
		 => (Doc,Set a) -> (Doc,Set a) -> Reporter ()
proper_subset ( d1 , s1 ) ( d2 , s2 ) = do
    inform $ vcat [ text "Ist die Menge"
		  , nest 4 $ fsep [ d1, equals, toDoc s1 ]
		  , text "echte Teilmenge der Menge"
		  , nest 4 $ fsep [ d2, equals, toDoc s2 ]
		  , text "?"
		  ]
    let no = minusSet s1 s2
    when ( not $ isEmptySet no ) $ reject $ vcat
	     [ text "Nein, diese Elemente sind"
	     , text "in" <+> d1 
	     , text ", aber nicht in" <+> d2
	     , toDoc no
	     ]
    let no_eq = minusSet s2 s1
    when ( isEmptySet no_eq ) $ reject $ vcat [ text "Nein, die Mengen stimmen überein." ]
    inform $ text "Ja."

-------------------------------------------------------------------------------

non_empty :: ( Ord a , ToDoc [a] ) => ( Doc , Set a ) -> Reporter ()
non_empty ( d , s ) = do inform $ vcat [ text "Ist die Menge"
				       , nest 4 $ fsep [ d , equals , toDoc s ]
				       , text "nicht leer?"
				       ]
                         when ( isEmptySet s ) $ reject $ vcat [ text "Nein." ]
			 inform $ text "Ja."

-------------------------------------------------------------------------------

eq ::  ( Ord a , ToDoc [a] ) => (Doc,Set a) -> (Doc,Set a) -> Reporter ()
eq m1 @ ( d1, s1 ) m2 @ ( d2, s2 ) = do
    inform $ vcat
	   [ text "Stimmen die Menge"
	   , nest 4 $ fsep [ d1, equals, toDoc s1 ]
	   , text "und die Menge"
	   , nest 4 $ fsep [ d2, equals, toDoc s2 ]
	   , text "überein?"
	   ]
    nested 4 $ do
        subeq m1 m2
        subeq m2 m1

-------------------------------------------------------------------------------

pairwise_disjunct :: ( Ord a , ToDoc [a] ) => [(Doc,Set a)] -> Reporter ()
pairwise_disjunct dss = do

    let nice (d,s) = nest 4 $ fsep [ d , equals , toDoc s ]

    inform $ vcat [ text "Sind die Mengen" ]
    inform $ vcat $ map nice dss
    inform $ vcat [ text $ if length dss < 3 then "disjunkt?" 
		                             else "paarweise disjunkt?" 
		  ]

    let nos = do (x@(_,d1),y@(_,d2)) <- cross1H dss
		 let no = intersect d1 d2
		 guard $ not $ isEmptySet no
		 return ((x,y),no)

    when ( not $ null nos ) $ reject $ vcat
	     [ text "Nein, die Mengen"
	     , nice $ fst $ fst $ head nos
	     , nice $ snd $ fst $ head nos
	     , text "enthalten diese gemeinsamen Elemente:" 
	     , nest 4 $ toDoc $ snd $ head nos
	     ]
    inform $ text "Ja."

-- | jedes mit jedem anderen

cross1H :: [a] -> [(a,a)]
cross1H []     =  []
cross1H (x:xs) =  [ (x,y) | y <- xs ] ++ cross1H xs

-------------------------------------------------------------------------------

partition :: ( Ord a , ToDoc [a] ) 
	     => [(Doc,Set a)] -> (Doc,Set a) -> Reporter ()
partition dss dm@(_,m) = do

    let nice (d,s) = nest 4 $ fsep [ d , equals , toDoc s ]

    inform $ vcat [ text "Bilden die Mengen" ]
    inform $ vcat $ map nice dss
    inform $ vcat [ text "eine Partition der Menge"
		  , nice dm
		  , text "?"
		  ]

    pairwise_disjunct dss

    inform $ vcat [ text "Ist die Vereinigung der Mengen" ]
    inform $ vcat $ map nice dss
    inform $ vcat [ text "gleich" , nice dm , text "?" ]

    let no = m `minusSet` ( unionManySets $ map snd dss )

    when ( not $ isEmptySet no ) $ reject $ vcat
	 [ text "Nein, diese Elemente sind in keiner der Mengen enthalten:"
	 , nest 4 $ toDoc no
	 ]

    inform $ text "Ja."