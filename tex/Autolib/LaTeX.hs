module LaTeX where

-- $Id$

import ToDoc

class LaTeX a where latex :: a -> Doc

lbraces x = text "\\left\\{" <+> nest 4 x <+> text "\\right\\}"
lparens x = text "\\left(" <+> nest 4 x <+> text "\\right)"
lbrackets x = text "\\left[" <+> nest 4 x <+> text "\\right]"

instance LaTeX Char where latex c = text [c]

instance LaTeX Int where latex n = toDoc n

instance LaTeX a => LaTeX [a] where 
    latex [] = text "\\epsilon"
    latex xs = fsep . map latex $ xs

table :: [[ Doc ]] -> Doc
-- alles zentriert
table xss = 
    let width  = maximum $ map length xss
	header = text $ "\\begin{array}{" ++ replicate width 'c' ++ "}"
	footer = text $ "\\end{array}"
        body   = do xs <- xss
		    return $ fsep ( punctuate (text " &") xs )
			     <+> text "\\\\"
    in  vcat [ header , nest 4  $ vcat body , footer ]
