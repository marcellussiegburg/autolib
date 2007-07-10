module Autolib.Multilingual.Doc where

import qualified Text.PrettyPrint.HughesPJ as PP
import qualified Data.Map
import Data.Map ( Map )

----------------------------------------------------------------
-- Types

data Language = DE | UK | NL
    deriving ( Read, Show, Eq, Ord )

data Doc = Doc 
         { contents :: Map Language PP.Doc 
         , fallback :: PP.Doc
         }

----------------------------------------------------------------
-- Accessors, Constructors

specialize :: Language -> Doc -> PP.Doc
specialize lang doc = 
    case Data.Map.lookup ( contents doc ) lang of
        Just this -> this
        Nothing -> fallback doc

-- | use default language 
text :: String -> Doc
text cs = Doc { contents = Data.Map.make []
              , fallback = PP.text cs
              }

-- | use several languages
multitext :: [(Lang, String)] -> Doc
multitext arg = Doc { contents = Data.Map.fromList arg 
                    , fallback = PP.empty
                    }

----------------------------------------------------------------
-- Combinators

fold_binary :: ( PP.Doc -> PP.Doc -> PP.Doc ) 
      -> Doc -> Doc 
      -> Doc
fold_binary op x y =
     undefined

fold_list :: ( [ PP.Doc ] -> PP.Doc ) 
      -> [ Doc ]
      -> Doc
fold_list op x y =
     undefined

vcat :: [ Doc ] -> Doc
vcat fold_list PP.vcat

