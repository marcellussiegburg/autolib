module Autolib.Output.Html where

--   $Id$

import Autolib.Output.Type
import qualified Text.Html as Html

instance Render Html.Html where
    render (Text t) = Html.stringToHtml t
    render (Doc d)  = Html.pre $ Html.primHtml ( show d )
    render (Image src) = 
	Html.image Html.! [ Html.src src, Html.alt src ]
    render (Link ref) =  
	Html.anchor ( Html.tt Html.<< ref ) Html.! [ Html.href ref ]
    render (Empty)  = Html.noHtml

    render (Above x y) = {- Html.p -} ( render x :: Html.Html)
			 Html.+++ 
			 Html.p ( render y :: Html.Html )
    render (Itemize xs) = Html.ulist Html.<<
		         do x <- xs ; return $ Html.li $ render x
    render (Nest x) = Html.blockquote $ render x


