module Relation.Type

--   $Id$

( module Relation.Type

-- damit unsere freunde nichts extra schreiben müssen
, module Sets
, module Data.FiniteMap
)

where


import Sets
import Size
import Data.FiniteMap

import ToDoc
import Reader

-- pforsicht: source und target später hinzugefügt,
-- ist noch nicht konsequent berücksichtigt

data Type a b = Make { source :: Set a
		     , target :: Set b
		     , unRelation :: FiniteMap a (Set b) 
		     }

instance Size (Type a b) where
    size = cardinality . source

instance ( Ord a, Ord b, ToDoc a, ToDoc b ) => ToDoc ( Type a b ) where
    toDoc r = text "Relation.make" <+> toDoc ( pairs r )

instance ( Ord a, Ord b, ToDoc a, ToDoc b ) => Show ( Type a b ) where
    show = render . toDoc

instance ( Ord a, Ord b, Reader a, Reader b ) => Reader ( Type a b ) where
    readerPrec d = readerParen (d > 9) $ do
        my_reserved "Relation.make"
	pairs <- reader
	return $ make pairs

instance ( Ord a, Ord b, Reader a, Reader b ) => Read ( Type a b ) where
    readsPrec = parsec_readsPrec

instance ( Ord a, Ord b ) => Eq ( Type a b ) where
    r == s = unRelation r == unRelation s

make :: (Ord a, Ord b) => [(a,b)] -> Type a b
make xys = 
    let xs = mkSet $ map fst xys
	ys = mkSet $ map snd xys
    in   ( make0 xys )
	 { source = xs , target = ys }

make0 :: (Ord a, Ord b) => [(a,b)] -> Type a b
make0 xys = Make
	 { source = error "Relation.Type.make_unsafe"
	 , target = error "Relation.Type.make_unsafe"
	 , unRelation = addListToFM_C union emptyFM $ do
	       (x, y) <- xys 
	       return (x, unitSet y)
	 }

pairs :: (Ord a, Ord b) => Type a b -> [(a,b)] 
pairs rel = do
    (x, ys) <- fmToList $ unRelation rel
    y <- setToList ys
    return (x, y)


