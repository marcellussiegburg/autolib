module Autolib.Multilingual where

import Data.List ( nub )
import qualified Data.Map
import Data.Map ( Map )

data Language = DE | UK | NL
    deriving ( Read, Show, Eq, Ord, Bounded, Enum )

data Type a = Make 
         { contents :: Map Language a
         }

-- | use several languages
make :: [(Language, a)] -> Type a
make arg = Make { contents = Data.Map.fromList arg }

specialize :: Language -> Type a -> a
specialize lang doc = 
    case Data.Map.lookup lang ( contents doc )  of
        Just this -> this
        Nothing   -> error $ "no version for language " ++ show lang


----------------------------------------------------------------
-- Combinators

fold_unary :: ( a -> b )
           -> Type a
           -> Type b
fold_unary op x = Make { contents = Data.Map.map op $ contents x }

fold_binary :: ( a -> a -> a ) 
      -> Type a -> Type a
      -> Type a
fold_binary op x y =
     Make { contents = Data.Map.unionWith op ( contents x ) ( contents y ) }

fold_list :: ( [ a ] -> b ) 
      -> [ Type a ]
      -> Type b
fold_list op xs =
    let get l = do x <- xs ; return $ specialize l x
        ls = nub $ concat $ map ( Data.Map.keys . contents ) xs
    in  make $ do l <- ls ; return ( l, op $ get l )

uniform d = make
      $ do l <- [ minBound .. maxBound ] ; return ( l, d )

