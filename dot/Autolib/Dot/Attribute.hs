--  $Id$

--  TODO: instance Reader

module Autolib.Dot.Attribute where

import Autolib.ToDoc hiding ( Style )
import Data.List ( intersperse )
import Data.Char ( toLower )

data Type = Color ColorT
	       | FillColor ColorT
	       | FontColor ColorT
               | Style StyleT

instance ToDoc Type where
    toDoc ( Color     c ) = text "color"     <+> equals <+> toDoc c
    toDoc ( FillColor c ) = text "fillcolor" <+> equals <+> toDoc c
    toDoc ( FontColor c ) = text "fontcolor" <+> equals <+> toDoc c
    toDoc ( Style     s ) = text "style"     <+> equals <+> toDoc s

data StyleT = Filled 
	    | Solid
	    | Dashed
	    | Dotted
	    | Bold
	    | Invis
     deriving ( Eq, Show )

instance ToDoc StyleT where
    toDoc st = text $ map toLower $ show st

data ColorT = HSV { hue :: Double
		  , saturation :: Double
		  , brightness :: Double
		  }
	    | RGB { red :: Int
		  , green :: Int
		  , blue :: Int
		  }
	    | Name NameT

instance ToDoc ColorT where
    toDoc ( c @ HSV {} ) = hcat $ intersperse comma $ map toDoc
	  [ hue c, saturation c, brightness c ]
    toDoc ( c @ RGB {} ) = hcat
	  [ char '#' , byte $ red c, byte $ green c, byte $ blue c ]
              where byte x = 
			let (a, b) = divMod x 16
			    form :: Int -> Char
			    form c = if c < 10 
				     then toEnum ( fromEnum '0' + c )
				     else toEnum ( fromEnum 'a' + c - 10 )
			in  text [ form a, form b ]
    toDoc ( Name n ) = toDoc n

data NameT = White | Black | Red | Green | Blue 
	   | Yellow | Magenta | Cyan | Burlywood
    deriving ( Eq, Show )

instance ToDoc NameT where
    toDoc n = text $ map toLower $ show n

