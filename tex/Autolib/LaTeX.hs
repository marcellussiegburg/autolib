module LaTeX where

--   $Id$

import ToDoc
import Data.List (intersperse)

class LaTeX a where latex :: a -> Doc

lbraces x = text "\\left\\{" <+> nest 4 x <+> text "\\right\\}"
lparens x = text "\\left(" <+> nest 4 x <+> text "\\right)"
lbrackets x = text "\\left[" <+> nest 4 x <+> text "\\right]"

instance LaTeX Char where latex c = text [c]

instance LaTeX Int where latex n = toDoc n

instance LaTeX a => LaTeX [a] where 
    latex [] = text "\\epsilon"
    latex xs = fsep . map latex $ xs


table :: Bool -> [[ Doc ]] -> Doc
-- alles zentriert
-- ein paar striche noch
table strich xss = 
    let width  = maximum $ map length xss
	header = text $ "\\begin{array}{" 
		      ++ ( if strich then intersperse '|' else id )
			 ( replicate width 'c' )
		      ++ "}"
	footer = text $ "\\end{array}"
        body   = do (k, xs) <- zip [1..] xss
		    return $ fsep ( punctuate (text " &") xs )
			     <+> ( if k < length xss 
				   then text "\\\\" else empty )
			     <+> ( if strich && k < length xss 
				   then text "\\hline" else empty )
			     <+> ( if strich && 1 == k
				   then text "\\hline" else empty )
    in  vcat [ header , nest 4  $ vcat body , footer ]
