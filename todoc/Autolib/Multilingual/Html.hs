{-# OPTIONS -fno-monomorphism-restriction #-}

module Autolib.Multilingual.Html where

import Autolib.Multilingual
import qualified Text.XHtml as Html

type Html = Autolib.Multilingual.Type Html.Html

stringToHtml = uniform . Html.stringToHtml

blockquote = fmap Html.blockquote
li = fmap Html.li
ulist = fmap Html.ulist
tt = fmap Html.tt

itemize xs = fold_list ( Html.ulist Html.<< ) xs 

br = uniform Html.br
noHtml = uniform Html.noHtml

pre = fmap Html.pre
primHtml = fmap Html.primHtml 
image = uniform Html.image

x ! y  = fmap (Html.! y) x

src = Html.src
alt = Html.alt
href = Html.href

anchor = fmap Html.anchor
f << doc = fold_list  (f Html.<<)  doc

(+++) = fold_binary (Html.+++)


