-- | anamorphism

module Ana where

import Control.Monad ( guard )

-- | compute anamorphism (construct list)
ana :: ( a -> Maybe (b, a) ) -> a -> [b]
ana f x = case f x of
    Just (y, z) -> y : ana f z
    Nothing     -> []

-- | find list of digits w.r.t. base
based :: Integral b => b -> b -> [b]
based b = reverse . ana ( \ x -> do
		       guard $ x > 0
		       let (q,r) = divMod x b
		       return (r,q)
             )

-- | convert from list of digits to number
unbased :: Integral b => b -> [b] -> b
unbased b = foldl ( \ x y -> b * x + y ) 0
