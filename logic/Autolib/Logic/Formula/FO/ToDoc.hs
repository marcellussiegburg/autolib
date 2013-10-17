{-# language OverloadedStrings #-}
{-# language FlexibleInstances #-}

module Autolib.Logic.Formula.FO.ToDoc where

import Autolib.Logic.Formula.FO.Data
import Autolib.Logic.Formula.Doc () -- instance
import Autolib.Logic.Formula.Name
import Autolib.ToDoc

instance ToDoc Formula where 
    toDocPrec p (Formula f) = formula names p f
instance Show Formula where 
    show = render . toDoc

instance ToDoc (Form Name) where
    toDocPrec p f = formula names p f
instance Show (Form Name) where
    show = render . toDoc

formula :: ToDoc n => [ n ] -> Int -> Form n -> Doc
formula names p f = case f of
    Succ l r -> 
        parens $ hsep [ toDoc l, "<1", toDoc r ]
    Less l r -> 
        parens $ hsep [ toDoc l, "<", toDoc r ]
    Letter s f -> 
        hsep [ toDoc s, parens $ toDoc f ]

    Not f -> 
        hsep [ "not" , parens $ formula names p f ]
    And l r -> parens $ hsep 
        [ formula names p l, "&&", formula names p r ]
    Or  l r -> parens $ hsep 
        [ formula names p l, "||", formula names p r ]
    Implies l r -> parens $ hsep 
        [ formula names p l, "=>", formula names p r ]

    Forall f -> with_name names $ \ n names' -> 
             hsep [ "forall" , toDoc n, parens $ formula names' p $ f n ]
    Exists f -> with_name names $ \ n names' -> 
             hsep [ "exists" , toDoc n, parens $ formula names' p $ f n ]

-- | these are used only for printing (?)

names :: [ Name ]
names = [ "p", "q", "r", "s", "t" ] 
        ++ map ( \ i -> Name ("t" ++ show i) ) [2 .. ]

with_name :: [a] -> ( a -> [a] -> b ) -> b
with_name ns cont = cont ( head ns ) ( tail ns )


