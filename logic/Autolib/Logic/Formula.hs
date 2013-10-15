{-# language OverloadedStrings, GeneralizedNewtypeDeriving #-}
{-# language FlexibleInstances, TypeSynonymInstances #-}

module Autolib.Logic.Formula where

import Autolib.ToDoc
import Autolib.Reader
import Data.String

-- | name that begins with lowercase letter denotes a position.
-- name that begins with uppercase letter denotes set of positions.
-- special case: letter predicates denote set of positions and are upcased as well.

data FOName = FOName String 
            | Unlift SOName

instance IsString FOName where fromString = FOName

newtype SOName = SOName String 

instance IsString SOName where fromString = SOName

data Formula 
    = SuccFO FOName FOName | LessFO FOName FOName
    | Apply SOName FOName
    | Subseteq SOName SOName | Singleton SOName | SuccSO SOName SOName
    | Not Formula | Or Formula Formula | And Formula Formula | Implies Formula Formula
    | ForallFO ( FOName -> Formula )
    | ForallSO ( SOName -> Formula )
    | ExistsFO ( FOName -> Formula )
    | ExistsSO ( SOName -> Formula )

f1 :: Formula
f1 = ForallFO $ \ x -> Implies ( Apply "A" x ) 
   $ ExistsFO $ \ y -> And (SuccFO x y) (Apply "Z" y)

instance ToDoc FOName where 
    toDoc n = case n of
        FOName s -> text s
        Unlift u -> hsep [ "Unlift", toDoc u ]

instance ToDoc SOName where toDoc (SOName s) = text s

-- should be in Autolib.ToDoc
instance IsString Doc where fromString = text

instance ToDoc Formula where toDocPrec p f = formula (fonames, sonames) p f
instance Show Formula where show = render . toDoc

formula fs @ (fonames,sonames) p f = case f of
    SuccFO l r -> parens $ hsep [ toDoc l, "<1", toDoc r ]
    LessFO l r -> parens $ hsep [ toDoc l, "<", toDoc r ]
    Apply s f -> hsep [ toDoc s, parens $ toDoc f ]

    Subseteq l r -> parens $ hsep [ toDoc l, "<=", toDoc r ]
    Singleton n -> hsep [ "Sing", parens $ toDoc n ]
    SuccSO l r -> parens $ hsep [ toDoc l, "<1", toDoc r ]

    Not f -> hsep [ "not" , parens $ formula fs p f ]
    And l r -> parens $ hsep [ formula fs p l, "&&", formula fs p r ]
    Or  l r -> parens $ hsep [ formula fs p l, "||", formula fs p r ]
    Implies l r -> parens $ hsep [ formula fs p l, "=>", formula fs p r ]
    ForallFO f -> with_foname fs $ \ n fs' -> 
             hsep [ "forall" , toDoc n, parens $ formula fs' p $ f n ]
    ForallSO f -> with_soname fs $ \ n fs' -> 
             hsep [ "forall" , toDoc n, parens $ formula fs' p $ f n ]
    ExistsFO f -> with_foname fs $ \ n fs' -> 
             hsep [ "exists" , toDoc n, parens $ formula fs' p $ f n ]
    ExistsSO f -> with_soname fs $ \ n fs' -> 
             hsep [ "exists" , toDoc n, parens $ formula fs' p $ f n ]


-- | these are used only for printing (?)

fonames :: [ FOName ]
fonames = [ "p", "q", "r", "s", "t" ] 
        ++ map ( \ i -> FOName ("t" ++ show i) ) [2 .. ]

sonames :: [ SOName ]
sonames = [ "X", "Y", "Z" ]
        ++ map ( \ i -> SOName ("Z" ++ show i) ) [2 .. ]

with_foname (fos, sos) cont = cont ( head fos ) ( tail fos, sos )
with_soname (fos, sos) cont = cont ( head sos ) ( fos, tail sos )





