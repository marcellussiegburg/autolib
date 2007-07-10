module Autolib.ToDoc.Frame where

-- import Text.PrettyPrint.HughesPJ
import Autolib.Multilingual.Doc

frame :: Doc -> Doc
frame d = 
    let ls = lines $ render d
	wl = maximum $ 0 : map length ls
        fill w = take wl $ w ++ repeat ' '
        clip l r w = text $ l ++ w ++ r
        pre : midpost = ls
	mid = init midpost
	post = last midpost
    in  if null ls then empty
	else if 1 == length ls then parens d
	else vcat [ clip "/" "\\" $ fill pre 
		  , vcat $ map (clip "|" "|" . fill) mid
		  , clip "\\" "/" $ fill post
		  ]
