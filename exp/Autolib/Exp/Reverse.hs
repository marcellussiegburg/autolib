module Exp.Reverse where

--  $Id$

import Prelude hiding ( reverse )
import Exp.Type

-- | construct expression for reverse image of language
reverse :: RX c -> RX c
reverse x = case x of
	
     -- only those are direction dependent:
     Dot l r -> Dot (reverse r) (reverse l)
     Left_Quotient l r -> Right_Quotient (reverse r) (reverse l)
     Right_Quotient l r -> Left_Quotient (reverse r) (reverse l)

     -- others are simply mapped
     Ref _ -> x  
     Letter _ -> x

     Union l r -> Union (reverse l) (reverse r)
     Intersection l r -> Intersection (reverse l) (reverse r)
     Difference l r -> Difference (reverse l) (reverse r)
     SymDiff  l r -> SymDiff (reverse l) (reverse r)
     Shuffle  l r -> Shuffle (reverse l) (reverse r)

     PowerStar x -> PowerStar (reverse x)
     PowerPlus x -> PowerPlus (reverse x)
     Power i x -> Power i (reverse x)


