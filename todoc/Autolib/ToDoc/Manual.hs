{-# OPTIONS -fallow-overlapping-instances #-}

module Autolib.ToDoc.Manual where

--  $Id$


import Autolib.ToDoc.Class
import Autolib.ToDoc.Dutch

import Data.Set
import Data.Int

instance ToDoc Int   where toDocPrec p = int
instance ToDoc Int32 where toDocPrec p = int . fromIntegral
instance ToDoc Int16 where toDocPrec p = int . fromIntegral
instance ToDoc Integer where toDocPrec p = integer
instance ToDoc Float where toDocPrec p = float
instance ToDoc Double where toDocPrec p = double

instance ToDoc Char where toDocPrec p = text . show


-- drift kann das folgende nicht, schade

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


