module ToDoc 

--   $Id$

( module ToDoc
, module Pretty 
)

where

import Pretty 

import Data.Set
import Data.FiniteMap

showDoc :: Doc -> String
-- zur ausgabe ohne zeilenschaltungen
-- TODO: sollte besser gehen (anderen renderer wählen?)
showDoc = unwords . words . render


max_list_length = 50 :: Int
max_string_length = 50 :: Int

-- funcall precedence
fcp = 10 :: Int

class ToDoc a where 
    toDoc :: a -> Doc
    -- default:
    toDoc = toDocPrec 0 -- useful?

    toDocPrec :: Int -> a -> Doc
    -- default:
    toDocPrec p = toDoc -- dangerous?


instance ToDoc Int where toDocPrec p = int
instance ToDoc Integer where toDocPrec p = integer
instance ToDoc Float where toDocPrec p = float
instance ToDoc Double where toDocPrec p = double

instance ToDoc Bool where toDocPrec p = text . show
instance ToDoc Char where toDocPrec p = text . show

instance ToDoc () where toDocPrec p = text . show

instance (ToDoc a, ToDoc b) => ToDoc (a, b) where
    toDocPrec p (x,y) = dutch_tuple
	      $ [ toDocPrec 0 x, toDocPrec 0 y ]

instance (ToDoc a, ToDoc b, ToDoc c) => ToDoc (a, b, c) where
    toDocPrec p (x,y,z) = dutch_tuple
	      $ [ toDocPrec 0 x, toDocPrec 0 y, toDocPrec 0 z]

instance (ToDoc a, ToDoc b, ToDoc c, ToDoc d) => ToDoc (a, b, c, d) where
    toDocPrec p (x,y,z,q) = dutch_tuple
	      $ [ toDocPrec 0 x, toDocPrec 0 y, toDocPrec 0 z, toDocPrec 0 q]

-- brauchen wir tatsächlich, für SQLqueries
instance (ToDoc a, ToDoc b, ToDoc c, ToDoc d, ToDoc e) 
    => ToDoc (a, b, c, d, e) where
    toDocPrec p (x,y,z,q,r) = dutch_tuple
	      $ [ toDocPrec 0 x, toDocPrec 0 y, toDocPrec 0 z
		, toDocPrec 0 q, toDocPrec 0 r
		]




instance ToDoc a => ToDoc [a] where
    toDocPrec p xs = dutch_list $ map toDoc xs

{-
        let (kurz, lang) = splitAt max_list_length xs
	    kdocs = map (toDocPrec 0) kurz
	    alles = kdocs ++  [ text "..." |  not $ null lang ]
	in  brackets $ fsep $ punctuate comma $ alles
-}

instance (ToDoc a, ToDoc b) => ToDoc (Either a b) where
    toDocPrec p (Left  x) = docParen (p >= fcp) $ text "Left " <+> toDocPrec fcp x
    toDocPrec p (Right x) = docParen (p >= fcp) $ text "Right" <+> toDocPrec fcp x

docParen :: Bool -> Doc -> Doc
docParen f = if f then parens else id


instance ToDoc (a -> b) where
    toDocPrec p f = text "<<function>>"

-- overlapping
instance ToDoc String where
    toDocPrec p cs = 
	  let (kurz, lang) = splitAt max_string_length cs
	      alles = kurz ++ if null lang then "" else "..."
	  in  char '"' <> text alles <> char '"'

putz :: ToDoc [a] => [a] -> IO ()
-- benutzt implizit  take max_list_length
putz = putStrLn . render . toDoc 

instance (ToDoc a, ToDoc b) => ToDoc (FiniteMap a b)
    where toDocPrec p fm = 
	      docParen (p >= fcp) $ text "listToFM" <+> toDocPrec fcp (fmToList fm)

--instance (ToDoc a, ToDoc b) => Show (FiniteMap a b)
--    where show = render . toDoc

instance (Show a, Show b) => Show (FiniteMap a b)
    where show fm = "listToFM " ++ show (fmToList fm)


instance ToDoc a => ToDoc (Maybe a) where
    toDocPrec p Nothing = text "Nothing"
    -- vorsicht, da fehlen eventuell klammern (now fixed)
    toDocPrec p ( Just x ) = docParen (p >= fcp) $ text "Just" <+> toDocPrec fcp x

--------------------------------------------------------------------------

-- dutch style

dutch :: Int -> (Doc, Doc, Doc) -> [ Doc ] -> Doc
dutch clip (op, sep, cl) [] = op <+> cl
dutch clip (op, sep, cl) ( x : xs ) = 
    let ( kurz, lang ) = splitAt clip xs
	over = if null lang then empty else sep <+> text "..."
    in  cat [ op  <+> x
	    , fsep -- cat 
	    $ do y <- kurz ; return $ sep <+> y
	    , over
	    , cl
	    ]

dutch_record :: [ Doc ] -> Doc
dutch_record = dutch max_list_length ( text "{", comma, text "}" )    

dutch_tuple :: [ Doc ] -> Doc
dutch_tuple = dutch max_list_length ( text "(", comma, text ")" )    

clipped_dutch_list :: Int -> [ Doc ] -> Doc
clipped_dutch_list c  = dutch c ( text "[", comma, text "]" )    

dutch_list :: [ Doc ] -> Doc
dutch_list = clipped_dutch_list max_list_length


