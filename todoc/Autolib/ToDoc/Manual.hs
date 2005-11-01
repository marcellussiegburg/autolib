{-# OPTIONS -fallow-overlapping-instances -fallow-undecidable-instances -fglasgow-exts -fallow-incoherent-instances #-}

module Autolib.ToDoc.Manual where

--  $Id$


import Autolib.ToDoc.Class
import Autolib.ToDoc.Dutch
import Autolib.ToDoc.Frame
import Autolib.ToDoc.Beside

import Data.Int
import Data.Typeable ( Typeable )
import Data.Ratio

instance ToDoc Int   where toDocPrec p = int
instance ToDoc Int32 where toDocPrec p = int . fromIntegral
instance ToDoc Int16 where toDocPrec p = int . fromIntegral
instance ToDoc Integer where toDocPrec p = integer
instance ToDoc Float where toDocPrec p = float
instance ToDoc Double where toDocPrec p = double

instance ToDoc Char where toDocPrec p = text . show
instance ToDoc Ordering where toDocPrec p = text . show

instance ( Integral a, ToDoc a ) => ToDoc ( Ratio a ) where 
    toDocPrec p r = docParen ( p > 0 )
        $ hsep [ toDoc ( numerator r ) , text "%", toDoc ( denominator r ) ]

instance ToDoc () where
    toDocPrec p () = dutch_tuple $ [ ]

instance (ToDoc a, ToDoc b) => ToDoc (a, b) where
    toDocPrec p (x,y) = dutch_tuple
	      $ [ toDocPrec 0 x, toDocPrec 0 y ]

instance (ToDoc a, ToDoc b, ToDoc c) => ToDoc (a, b, c) where
    toDocPrec p (x,y,z) = dutch_tuple
	      $ [ toDocPrec 0 x, toDocPrec 0 y, toDocPrec 0 z]

instance (ToDoc a, ToDoc b, ToDoc c, ToDoc d) => ToDoc (a, b, c, d) where
    toDocPrec p (x,y,z,q) = dutch_tuple
	      $ [ toDocPrec 0 x, toDocPrec 0 y, toDocPrec 0 z, toDocPrec 0 q]


instance (Nice a, Nice b) => Nice (a, b) where
    nicePrec p (x,y) = nice_tuple
	      $ [ nicePrec 0 x, nicePrec 0 y ]

instance (Nice a, Nice b, Nice c) => Nice (a, b, c) where
    nicePrec p (x,y,z) = nice_tuple
	      $ [ nicePrec 0 x, nicePrec 0 y, nicePrec 0 z]

instance (Nice a, Nice b, Nice c, Nice d) => Nice (a, b, c, d) where
    nicePrec p (x,y,z,q) = nice_tuple
	      $ [ nicePrec 0 x, nicePrec 0 y, nicePrec 0 z, nicePrec 0 q]

-- | TODO: parentheses, commas
nice_tuple = frame . besides 
-- | TODO: parentheses, commas
nice_list xs = 
    if null $ drop 3 xs
    then frame $ besides xs
    else frame $ vcat xs

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

instance Nice String where 
    nicePrec p = toDoc

instance Nice a => Nice [a] where
    nicePrec p xs = nice_list $  map nice xs

instance ToDoc (a -> b) where
    toDocPrec p f = text "<<function>>"

-- overlapping
instance ToDoc String where
    toDocPrec p cs = 
	  let (kurz, lang) = splitAt max_string_length cs
	      alles = kurz ++ if null lang then "" else "..."
	  in  text $ show alles

-- (un)clipped lists/strings

data Clip a = Full [a] | Clip Int [a]
	      deriving ( Eq , Ord , Show , Read , Typeable )

instance ToDoc a => ToDoc (Clip a) where 
    toDocPrec p (Full   xs) = unclipped_dutch_list $ map toDoc xs
    toDocPrec p (Clip n xs) = clipped_dutch_list n $ map toDoc xs

-- overlapping
instance ToDoc (Clip Char) where
    toDocPrec p (Full   cs) = text cs
    toDocPrec p (Clip n cs) = 
	  let (kurz, lang) = splitAt n cs
	      alles = kurz ++ if null lang then "" else "..."
	  in  text $ show alles

putz :: ToDoc [a] => [a] -> IO ()
-- benutzt implizit  take max_list_length
putz = putStrLn . render . toDoc 
