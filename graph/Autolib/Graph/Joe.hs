module Graph_Joe where

import Data.Set
import Data.FiniteMap

import Control.Monad ( guard )

-------------------------------------------------------------------------------
-- Typdefinitionen

data Graph a  = Graph
	      { tafel     :: FiniteMap a (Knoten a)
--	      , knoten    :: Set a
	      , kanten    :: Set (Kante a)
	      }

knoten :: Ord a => Graph a -> Set a
knoten g = mkSet (keysFM (tafel g))

data Knoten a = Knoten
              { idy      :: a
	      , info      :: String
              } -- deriving (Eq, Ord)
instance Eq  a => Eq  (Knoten a) where x == y = idy x == idy y
instance Ord a => Ord (Knoten a) where x `compare` y = idy x `compare` idy y

data Kante a  = Kante
	      { von       :: a
	      , nach      :: a
              } deriving (Eq, Ord)

-------------------------------------------------------------------------------
-- bissl hübsche ausgabe

instance (Show a, Ord a) => Show (Graph a) where
    showsPrec p g = showString $ unlines 
		  $ [ "{ knoten = " ++ show (setToList (knoten g))
		    , ", kanten = " ++ show (setToList (kanten g))
		    , ", tafel  = " ++ show (fmToList  (tafel g))
		    , "}"
		    ]

instance (Show a, Ord a) => Show (Knoten a) where
    showsPrec p k = showString $ show (info k)
     
instance (Show a, Ord a) => Show (Kante a) where
    showsPrec p k = showString $ "(" ++ show (von k) ++ "," ++ show (nach k) ++ ")"

-------------------------------------------------------------------------------
-- Funktionen über Knoten von Graphen

vorgänger :: Eq a => (Graph a) -> a -> [ a ]
vorgänger g x = do
	  k @ Kante { } <- setToList $ kanten g
	  guard $ nach k == x
	  return $ von k

nachfolger :: Eq a => (Graph a) -> a -> [ a ]
nachfolger g x = do
	  k @ Kante { } <- setToList $ kanten g
	  guard $ von k == x
	  return $ nach k

ein_grad, aus_grad, grad :: Eq a => (Graph a) -> a -> Int
ein_grad g k = length $ vorgänger g k
aus_grad g k = length $ nachfolger g k
grad g k = ein_grad g k + aus_grad g k

-------------------------------------------------------------------------------

mapG :: (Ord a, Ord b) 
     => (String -> String) -> (a -> b) -> (Graph a -> Graph b)
mapG fi f g = Graph 
     { tafel = listToFM [ ( f i, v { idy = f (idy v), info = fi (info v) } )
			| (i, v) <- fmToList (tafel g)
			]
     , kanten = mapSet ( \ k -> Kante { von = f (von k), nach = f (nach k) } ) ( kanten g )
     }

abzähl :: Ord a => Graph a -> Graph Int
abzähl g = 
    let fm  = listToFM $ zip ( setToList $ knoten g ) [ 1 .. ]
        f   = lookupWithDefaultFM fm (error "abzähl") 
    in  mapG id f g

gefährliche_summe :: Ord a => Graph a -> Graph a -> Graph a
gefährliche_summe g1 g2 = 
    Graph { tafel = plusFM (tafel g1) (tafel g2)
	  , kanten = kanten g1 `union` kanten g2
	  }

summe :: (Ord a, Ord b) => Graph a -> Graph b -> Graph (Either a b)
summe g1 g2 =
    mapG ('L' :) Left g1 `gefährliche_summe` mapG ('R' :) Right g2


-------------------------------------------------------------------------------
-- teststuff

kn :: a -> String -> (Knoten a)
kn a i = Knoten { idy = a, info = i }

ka :: a -> a -> (Kante a)
ka m n = Kante { von = m, nach = n }


independent :: Integer -> Graph Integer
independent n = 
        Graph { kanten = emptySet
	      , tafel  = listToFM [ (i, kn i ('I' : show i)) 
				  | i <- [ 1 .. n ] ]
	      }

clique  :: Integer -> Graph Integer
clique n =
    Graph { tafel = listToFM [ (i, kn i ('K' : show i))
			     | i <- [ 1 .. n ] ]
	  , kanten = mkSet [ ka i j 
			   | i <- [ 1 .. n ], j <- [1 .. n], i /= j ] 
	  }

kreis :: Integer -> Graph Integer
kreis n =
    Graph { tafel = listToFM [ (i, kn i ('C' : show i))
			     | i <- [ 1 .. n ] ]
      , kanten = mkSet [ ka i j
		       | x <- [ 1 .. n ], let y = (x `mod` n) + 1
		       , (i, j) <- [(x,y), (y,x) ]
		       ]
      }

