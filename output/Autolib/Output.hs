module Output 


where

-- $Id$

import qualified Pretty
import qualified Html
import qualified HTMLMonad -- wash

data Output = Text String
	    | Doc  Pretty.Doc
--	    | Html Html.Html
	    | Image FilePath -- source
	    | Link FilePath 
	    | Empty
	    | Above Output Output
	    | Itemize [ Output ]
	    | Nest Output

class Render r where
      render :: Output -> r

instance Render Pretty.Doc where
    render (Text t) = Pretty.text t
    render (Doc d)  = d
--    render (Html h)  = Pretty.text $ Html.prettyHtml h
    render (Image src) = Pretty.text $ "<img src=" ++ src ++ "/>"
    render (Link url) = Pretty.text $ "<a href=" ++ url ++ "/>"
    render (Empty)  = Pretty.empty

    render (Above x y) = render x Pretty.$$ render y
    render (Itemize xs) = Pretty.vcat 
               $ do x <- xs ; return ( Pretty.text "*" Pretty.<+> render x )
    render (Nest x) = Pretty.nest 4 $ render x

instance Render Html.Html where
    render (Text t) = Html.stringToHtml t
    render (Doc d)  = Html.pre $ Html.primHtml ( show d )
--    render (Html h) = h
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



instance Monad m => Render ( HTMLMonad.WithHTML m () ) where
    render (Text t) = HTMLMonad.text t
    render (Doc d)  = HTMLMonad.pre $ HTMLMonad.formattedtext ( show d )
--    render (Html h) = error "Output.Render.HTMLMonad.Html"
    render (Empty)  = HTMLMonad.empty

    render (Image src) = 
	HTMLMonad.img $ HTMLMonad.attr "src" src
    render (Link ref) =  
	HTMLMonad.a $ HTMLMonad.attr "href" ref

    render (Above x y) = do 
        () <- render x 
        () <- render y
	return ()
    render (Itemize xs) = HTMLMonad.ul $ sequence_ $ do
	x <- xs
	return $ do
	    () <- HTMLMonad.li ( render x )
	    return ()
    render (Nest x) = HTMLMonad.blockquote $ render x
