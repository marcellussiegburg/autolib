-- $Header$

-- ghc:  -fallow-overlapping-instances -fallow-undecidable-instances
--       -fglasgow-exts
-- hugs: -98 +o

-- $Log$
-- Revision 1.3  2002-06-29 18:44:02  joe
-- max_lengths hochgesetzt
--
-- Revision 1.2  2002/06/17 10:52:19  joe
-- neu: showDoc
--
-- Revision 1.1.1.1  2002/05/24 10:46:48  challenger
-- start
--
-- Revision 1.10  2002/01/13 23:02:34  autotool
-- instance ToDoc (Maybe *)
--
-- Revision 1.9  2001/12/01 14:47:33  autotool
-- prekompilierte grader
--
-- Revision 1.8  2001/11/29 09:43:42  autotool
-- max_strin_length
--
-- Revision 1.7  2001/11/18 23:55:23  autotool
-- neu: relation, fixpunkt
-- todoc: instance Either
--
-- Revision 1.6  2001/11/11 17:10:09  autotool
-- ToDoc: instance ToDoc String
--    deswegen viele Typ-Änderungen überall
--    (benutzt overlapping instances)
-- Revision 1.4  2001/11/08 12:55:26  autotool
-- neu: lange listen in toDoc abschneiden


module ToDoc 

( module ToDoc
, module Pretty
)

where

import Pretty 
import Set
import FiniteMap

showDoc :: Doc -> String
-- zur ausgabe ohne zeilenschaltungen
-- TODO: sollte besser gehen (anderen renderer wählen?)
showDoc = unwords . words . render


max_list_length = 100 :: Int
max_string_length = 100 :: Int

class ToDoc a where toDoc :: a -> Doc

instance ToDoc Int where toDoc = int
instance ToDoc Integer where toDoc = integer
instance ToDoc Float where toDoc = float
instance ToDoc Double where toDoc = double

instance ToDoc Bool where toDoc = text . show
instance ToDoc Char where toDoc = text . show


instance (ToDoc a, ToDoc b) => ToDoc (a, b) where
    toDoc (x,y) = parens 
	      $ fsep 
	      $ punctuate comma
	      $ [ toDoc x, toDoc y ]

instance (ToDoc a, ToDoc b, ToDoc c) => ToDoc (a, b, c) where
    toDoc (x,y,z) = parens 
	      $ fsep 
	      $ punctuate comma
	      $ [ toDoc x, toDoc y, toDoc z]


instance ToDoc a => ToDoc [a] where
    toDoc xs = 
        let (kurz, lang) = splitAt max_list_length xs
	    kdocs = map toDoc kurz
	    alles = kdocs ++  [ text "..." |  not $ null lang ]
	in  brackets $ fsep $ punctuate comma $ alles

instance (ToDoc a, ToDoc b) => ToDoc (Either a b) where
    toDoc (Left  x) = text "Left " <+> toDoc x
    toDoc (Right x) = text "Right" <+> toDoc x

-- overlapping
instance ToDoc String where
    toDoc cs = 
	  let (kurz, lang) = splitAt max_string_length cs
	      alles = kurz ++ if null lang then "" else "..."
	  in  char '"' <> text alles <> char '"'

putz :: ToDoc [a] => [a] -> IO ()
-- benutzt implizit  take max_list_length
putz = putStrLn . render . toDoc 


instance ToDoc [a] => ToDoc (Set a)
    where toDoc s = text "mkSet" <+> toDoc (setToList s)

--instance ToDoc [a] => Show (Set a)
--    where show = render . toDoc

instance (Show a) => Show (Set a)
    where show s = "mkSet " ++ show (setToList s)

instance (ToDoc a, ToDoc b) => ToDoc (FiniteMap a b)
    where toDoc fm = text "listToFM" <+> toDoc (fmToList fm)

--instance (ToDoc a, ToDoc b) => Show (FiniteMap a b)
--    where show = render . toDoc

instance (Show a, Show b) => Show (FiniteMap a b)
    where show fm = "listToFM " ++ show (fmToList fm)


instance ToDoc a => ToDoc (Maybe a) where
    toDoc Nothing = text "Nothing"
    -- vorsicht, da fehlen eventuell klammern:
    toDoc ( Just x ) = text "Just" <+> toDoc x




