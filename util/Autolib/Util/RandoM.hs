module Autolib.Util.RandoM 

( module Autolib.Util.RandoM
, Random
)

where

import System.Random ( StdGen, randomR, Random, getStdGen, mkStdGen )
import qualified System.Random

import Control.Monad.State

type RandoM = State StdGen

class Monad m => RandomC m where 
   randomRIO :: Random a => (a,a) -> m a

instance RandomC IO where
   randomRIO = System.Random.randomRIO

instance RandomC RandoM where
    randomRIO bnd = do
        g0 <- get
        let ( x, g1 ) = randomR bnd g0
        put g1
        return x

randomly :: Int -> RandoM a -> a
randomly seed action = evalState action ( mkStdGen seed ) 

lift :: RandoM a -> IO a
lift action = do
    g <- getStdGen
    return $ evalState action g

    

-----------------------------------------------------------

