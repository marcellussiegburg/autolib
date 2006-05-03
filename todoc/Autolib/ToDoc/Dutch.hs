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
{- BUG #132 -> keine Verkürzungen mehr, bitte
    let ( kurz, lang ) = case mclip of
            Nothing   -> ( xs, [] )
            Just clip -> splitAt clip xs
-}
    let ( kurz, lang ) = ( xs , [] )
	over = if null lang then empty else sep <+> text "..."
	its = ( op <+> x ) 
	    : ( do y <- kurz ; return $ sep <+> y ) ++ [ over ]
    in  Autolib.ToDoc.Class.sep [ fsep its , cl ]

{- BUG #132 -> keine Verkürzungen mehr, bitte
max_list_length = 50 :: Int
max_string_length = 70 :: Int
-}

dutch_record :: [ Doc ] -> Doc
{- BUG #132 -> keine Verkürzungen mehr, bitte
dutch_record = dutch (Just max_list_length) ( text "{", comma, text "}" )    
-}
dutch_record = dutch Nothing ( text "{", comma, text "}" )    

named_dutch_record :: String -> [ Doc ] -> Doc
{- BUG #132 -> keine Verkürzungen mehr, bitte
named_dutch_record tag = 
    dutch ( Just max_list_length ) ( text tag <+> text "{", comma, text "}" )
-}
named_dutch_record tag = 
    dutch Nothing ( text tag <+> text "{", comma, text "}" )

dutch_tuple :: [ Doc ] -> Doc
{- BUG #132 -> keine Verkürzungen mehr, bitte
dutch_tuple = dutch (Just max_list_length) ( text "(", comma, text ")" )    
-}
dutch_tuple = dutch Nothing ( text "(", comma, text ")" )    

clipped_dutch_list :: Int -> [ Doc ] -> Doc
clipped_dutch_list c  = dutch (Just c) ( text "[", comma, text "]" )    

unclipped_dutch_list :: [ Doc ] -> Doc
unclipped_dutch_list = dutch Nothing ( text "[", comma, text "]" )    

dutch_list :: [ Doc ] -> Doc
{- BUG #132 -> keine Verkürzungen mehr, bitte
dutch_list = clipped_dutch_list max_list_length
-}
dutch_list = unclipped_dutch_list

-----------------------------------------------------------------------

sepBy :: Doc -> [ Doc ] -> Doc
sepBy s ds = fsep $ intersperse s ds

quoted :: Doc -> Doc
quoted d = char '`' <> d <> char '\''

(</>) :: Doc -> Doc -> Doc
l </> r = l $+$ nest 4 r

