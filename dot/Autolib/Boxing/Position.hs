module Autolib.Boxing.Position where

--   $Id$

import Autolib.ToDoc

data Position = Position { width :: Double, height :: Double }
	deriving ( Eq, Ord )

instance ToDoc Position where
    toDoc p = text "Position" <+> braces ( fsep $ punctuate comma
	  [ text "width" <+> equals <+> toDoc ( width p )
	  , text "height" <+> equals <+> toDoc ( height p )
	  ] )
instance Show Position where 
    show = render . toDoc

-- in dot-format
zeige :: Position -> String
zeige p = 
    let xform r = show $ ( round ( r * 100 ) :: Int )
    in  xform ( width p ) ++ "," ++ xform ( height p )

lies :: String -> Position
lies cs =
    let ( pre, post ) = span (/= ',') cs
    in  Position { width = 0.01 * fromIntegral (read pre) 
		 , height = 0.01 * fromIntegral ( read $ tail post )
		 }


instance Num Position where
    negate p = Position { width = negate $ width p
			, height = negate $ height p
			}
    d + e = Position { width = width d + width e 
		      , height = height d + height e 
		      }
    --  damit wir  2 * p  schreiben können:
    fromInteger i = Position { width = fromInteger i, height = 0 }
    Position { width = f, height = 0 } * e 
          = Position { width = width e * f, height = height e * f }
    e * Position { width = f, height = 0 } 
          = Position { width = width e * f, height = height e * f }

    signum = error "instance Num Position: signum method missing"
    abs p  = Position { width = sqrt $ width p ^ 2 + height p ^ 2
		      , height = 0
		      }

instance Fractional Position where
    -- damit wir 0.5 * p schreiben können
    fromRational r = Position { width = fromRational r, height = 0 }
    recip ( Position { width = f, height = 0 } ) = 
	    Position { width = recip f, height = 0 }

larger :: Position -> Position -> Position
larger p q = Position { width = max (width p) (width q)
		      , height = max (height p) (height q)
		      }

no_height :: Position -> Position
no_height d = d { height = 0 }

no_width :: Position -> Position
no_width d = d { width = 0 }

minimax :: [ Position ] -> ( Position, Position )
minimax ps = 
    let xs = map width ps
	ys = map height ps
    in	( Position { width = minimum xs , height = minimum ys }
	, Position { width = maximum xs , height = maximum ys }
	)


