module Autolib.LTL.Print where

import Autolib.LTL.Data

import Autolib.ToDoc

instance ToDoc Formula where
    toDocPrec p f = case f of
        Variable v -> text v
        Nullary nop -> toDoc nop
        Unary uop g -> 
            toDoc uop <+> toDocPrec 10 g
        Binary bop g h -> case bop of
            Implies  -> binary p 2 1 bop g h
            Until    -> binary p 3 4 bop g h
            Or       -> binary p 5 6 bop g h
            And      -> binary p 7 8 bop g h

instance ToDoc Nop where
    toDoc (Constant b) = toDoc b

instance ToDoc Uop where
    toDoc uop = case uop of
         Not -> text "not"
         Always -> text "always"
         Eventually -> text "eventually"
         Next -> text "next"

binary context l r bop g h = 
    let me = min l r in
    ( if context > me then parens else id )
    $ toDocPrec l g <+> toDoc bop <+> toDocPrec r h

instance ToDoc Bop where
    toDoc bop = case bop of
        And -> text "&&"
        Or  -> text "||"
        Implies -> text "=>"
        Until -> text "until"

instance Show Formula where show = render . toDoc
instance Show Nop where show = render . toDoc
instance Show Uop where show = render . toDoc
instance Show Bop where show = render . toDoc
