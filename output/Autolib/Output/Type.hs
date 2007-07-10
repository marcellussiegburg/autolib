-- | hier stehen nur der Typ und die Pretty-Instanz

module Autolib.Output.Type where

--   $Id$


-- import qualified Text.PrettyPrint.HughesPJ as Pretty
import qualified Autolib.Multilingual.Doc as Pretty

data Output = Text String
	    | Doc  Pretty.Doc
	    | Pre  Pretty.Doc
	    | Image FilePath -- source
	    | Link FilePath 
	    | Named_Link String FilePath 
	    | Empty
	    | Above  Output Output
	    | Beside Output Output
	    | Itemize [ Output ]
	    | Nest Output

class Render r where
      render :: Output -> r

instance Render Pretty.Doc where
    render (Text t) = Pretty.text t
    render (Doc d)  = d
    render (Pre d)  = d

    -- render (Image src) = Pretty.text $ "<img src=" ++ src ++ "/>"
    -- render (Link url) = Pretty.text $ "<a href=" ++ url ++ "/>"

    render (Image src) = Pretty.text "<img>"
    render (Link url) = Pretty.text "<a href>"
    render (Named_Link name url) = Pretty.text "<a href>"

    render (Empty)  = Pretty.empty

    render (Above  x y) = render x Pretty.$$ render y
    render (Beside x y) = render x Pretty.<+> render y

    render (Itemize xs) = Pretty.vcat 
               $ do x <- xs ; return ( Pretty.text "*" Pretty.<+> render x )
    render (Nest x) = Pretty.nest 4 $ render x

