module ToDoc.Dutch where

--  $Id$

import ToDoc.Class
import Data.List (intersperse)

-- output sequences in "dutch style"
-- i. e. wrapped lines start (instead of end) with separators 

dutch :: Int -- ^ clipping
      -> (Doc, Doc, Doc) -- ^ ( opening, separator, closing )
      -> [ Doc ] -- ^ input
      -> Doc
dutch clip (op, sep, cl) [] = op <+> cl
dutch clip (op, sep, cl) ( x : xs ) = 
    let ( kurz, lang ) = splitAt clip xs
	over = if null lang then empty else sep <+> text "..."
    in  cat [ op <+> x
	    , fsep -- cat 
	    $ do y <- kurz ; return $ sep <+> y
	    , over
	    , cl
	    ]

max_list_length = 50 :: Int
max_string_length = 50 :: Int

dutch_record :: [ Doc ] -> Doc
dutch_record = dutch max_list_length ( text "{", comma, text "}" )    

dutch_tuple :: [ Doc ] -> Doc
dutch_tuple = dutch max_list_length ( text "(", comma, text ")" )    

clipped_dutch_list :: Int -> [ Doc ] -> Doc
clipped_dutch_list c  = dutch c ( text "[", comma, text "]" )    

dutch_list :: [ Doc ] -> Doc
dutch_list = clipped_dutch_list max_list_length

-----------------------------------------------------------------------

sepBy :: Doc -> [ Doc ] -> Doc
sepBy s ds = fsep $ intersperse s ds

quoted :: Doc -> Doc
quoted d = char '`' <> d <> char '\''

