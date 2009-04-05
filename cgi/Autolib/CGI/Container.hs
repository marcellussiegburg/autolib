module Web.Container 

( Container, fresh, add, dump )

where

-- | Container contains items and knows how to combine them
data Container a = Container { items :: [a]
                     , combine :: [a] -> a
                     }

fresh :: ( [a] -> a ) -> Container a
fresh fun = Container { items = [], combine = fun }

add :: a -> Container a -> Container a
add x c = let xs = items c in c { items = x : xs }

dump :: Container a -> a
dump c = combine c $ reverse $ items c
