{-# language DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}

module Web.Collection where

import Happstack.State hiding ( Collection )

import Web.Widget
import Web.Edit

import Data.Typeable
import Control.Monad ( forM )

data Collection a = Collection
                  { counter :: Int
                  , contents :: [ (Int, a) ]
                  }
    deriving ( Show, Typeable )

instance Version (Collection a)

$(deriveSerialize ''Collection)

instance Component a => Component ( Collection a ) where
    type Dependencies ( Collection a ) = End
    initialValue = empty 

empty :: Collection a
empty = Collection
      { counter = 0
      , contents = []
      }

extend :: Component a => Collection a -> Collection a
extend c = 
    let this = counter c
    in  c { counter = succ this
          , contents = contents c ++ [(this, initialValue)]
          }

instance ( Component a, Edit a ) => Edit ( Collection a ) where
    edit c = btable $ do
        click <- tr $ td $ submit "add"
        let d = if click then extend c else c
        pairs <- forM (contents d) $ \ (tag,val) -> tr $ do
             td $ text $ show tag
             val' <- td $ edit val
             return ( tag, val' )
        return $ d { contents = pairs }

