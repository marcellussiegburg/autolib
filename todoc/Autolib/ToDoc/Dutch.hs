module Autolib.ToDoc.Dutch where

--  $Id$

import Autolib.ToDoc.Class
import Data.List (intersperse)

-- | output sequences in "dutch style"
-- i. e. wrapped lines start (instead of end) with separators 
dutch :: Maybe Int -- ^ clipping
      -> (Doc, Doc, Doc) -- ^ ( opening, separator, closing )
      -> [ Doc ] -- ^ input
      -> Doc
dutch mclip (op, sep, cl) [] = op <+> cl
dutch mclip (op, sep, cl) ( x : xs ) = 
    let ( kurz, lang ) = case mclip of
            Nothing   -> ( xs, [] )
            Just clip -> splitAt clip xs
	over = if null lang then empty else sep <+> text "..."
	its = ( op <+> x ) 
	    : ( do y <- kurz ; return $ sep <+> y ) ++ [ over ]
    in  Autolib.ToDoc.Class.sep [ fsep its , cl ]

max_list_length = 50 :: Int
max_string_length = 70 :: Int

dutch_record :: [ Doc ] -> Doc
dutch_record = dutch (Just max_list_length) ( text "{", comma, text "}" )    

dutch_tuple :: [ Doc ] -> Doc
dutch_tuple = dutch (Just max_list_length) ( text "(", comma, text ")" )    

clipped_dutch_list :: Int -> [ Doc ] -> Doc
clipped_dutch_list c  = dutch (Just c) ( text "[", comma, text "]" )    

unclipped_dutch_list :: [ Doc ] -> Doc
unclipped_dutch_list = dutch Nothing ( text "[", comma, text "]" )    

dutch_list :: [ Doc ] -> Doc
dutch_list = clipped_dutch_list max_list_length

-----------------------------------------------------------------------

sepBy :: Doc -> [ Doc ] -> Doc
sepBy s ds = fsep $ intersperse s ds

quoted :: Doc -> Doc
quoted d = char '`' <> d <> char '\''

