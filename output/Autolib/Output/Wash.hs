module Output.Wash where

--   $Id$

import Output.Type
import qualified HTMLMonad

instance Monad m => Render ( HTMLMonad.WithHTML m () ) where
    render (Text t) = HTMLMonad.text t
    render (Doc d)  = HTMLMonad.pre $ HTMLMonad.formattedtext ( show d )
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

