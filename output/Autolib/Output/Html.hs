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

    render (Above x Empty) = render x
    render (Above Empty y) = render y
    render (Above x y    ) =
        let rx = render x :: Html.Html
	    ry = render y :: Html.Html
        in  if breaking y 
	    then rx                  Html.+++ ry
	    else rx Html.+++ Html.br Html.+++ ry

    render (Beside x Empty) = render x
    render (Beside Empty y) = render y
    render (Beside x y    ) =
        let rx = render x :: Html.Html
	    ry = render y :: Html.Html
        in  rx Html.+++ ry

    render (Itemize xs) = Html.ulist Html.<<
		         do x <- xs ; return $ Html.li $ render x
    render (Nest x) = Html.blockquote $ render x


breaking :: Output -> Bool
breaking ( Itemize _ ) = True
breaking ( Nest _ ) = True
breaking ( Doc _ ) = True
breaking ( Above x y ) = breaking x
breaking ( Beside x y ) = breaking x
breaking _ = False

