module Output 


where

-- $Id$

import qualified Pretty
import qualified Html

data Output = Text String
	    | Doc  Pretty.Doc
	    | Html Html.Html
	    | Empty
	    | Above Output Output
	    | Itemize [ Output ]
	    | Nest Output

class Render r where
      render :: Output -> r

instance Render Pretty.Doc where
    render (Text t) = Pretty.text t
    render (Doc d)  = d
    render (Html h)  = Pretty.text $ Html.prettyHtml h
    render (Empty)  = Pretty.empty

    render (Above x y) = render x Pretty.$$ render y
    render (Itemize xs) = Pretty.vcat 
               $ do x <- xs ; return ( Pretty.text "*" Pretty.<+> render x )
    render (Nest x) = Pretty.nest 4 $ render x

instance Render Html.Html where
    render (Text t) = Html.stringToHtml t
    render (Doc d)  = Html.pre $ Html.primHtml ( show d )
    render (Html h) = h
    render (Empty)  = Html.noHtml

    render (Above x y) = {- Html.p -} ( render x :: Html.Html)
			 Html.+++ 
			 Html.p ( render y :: Html.Html )
    render (Itemize xs) = Html.ulist Html.<<
		         do x <- xs ; return $ Html.li $ render x
    render (Nest x) = Html.blockquote $ render x
