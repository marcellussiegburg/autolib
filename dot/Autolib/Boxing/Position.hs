module Boxing.Position where

-- $Id$

import ToDoc

data Position = Position { width :: Double, height :: Double }
	deriving ( Eq, Ord )

instance ToDoc Position where
    toDoc p = text "Position" <+> braces ( fsep $ punctuate comma
	  [ text "width" <+> equals <+> toDoc ( width p )
	  , text "height" <+> equals <+> toDoc ( height p )
	  ] )
instance Show Position where 
    show = render . toDoc


instance Num Position where
    d + e = Position { width = width d + width e 
		      , height = height d + height e 
		      }
    -- damit wir  2 * p  schreiben können:
    fromInteger i = Position { width = fromInteger i, height = 0 }
    d @ Position { width = f, height = 0 } * e 
          = Position { width = width e * f, height = height e * f }

instance Fractional Position where
    -- damit wir 0.5 * p schreiben können
    fromRational r = Position { width = fromRational r, height = 0 }

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


