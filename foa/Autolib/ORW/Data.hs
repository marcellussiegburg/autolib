-- | omega regular (= eventually periodic) words

{-# language DeriveDataTypeable #-}

module Autolib.ORW.Data where

import Autolib.Size
import Prelude hiding ( head, tail )
import qualified Data.List as L
import Data.Typeable

data ORW sigma = 
     ORW { prefix :: [sigma] 
         , period :: [sigma]
         }
    deriving (Eq, Ord, Typeable) 

-- FIXME: deriving is not necessarily a good idea:
-- * could use a hash function 
-- * could normalized words:
-- ** the period must be a shortest period
-- ** the prefix must be shortest (otherwise,
--    rotate the period)

instance Size (ORW sigma) where
   size o = length (prefix o) + length (period o)

head :: ORW sigma -> sigma
head o = case prefix o of 
    [] -> case period o of 
        [] -> error $ "ORW.head: empty period"
        y : ys -> y
    x : xs -> x

tail :: ORW sigma -> ORW sigma
tail o = case prefix o of 
    [] -> case period o of 
        [] -> error $ "ORW.head: empty period"
        y : ys -> ORW { prefix = ys, period = period o}
    x : xs -> ORW { prefix = xs, period = period o }

tails :: Eq sigma => ORW sigma -> [ORW sigma]
tails o = L.nub $ do 
    p <- L.tails (prefix o) ++ L.tails (period o)
    return $ ORW { prefix = p, period = period o }
