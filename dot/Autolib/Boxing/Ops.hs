module Autolib.Boxing.Ops where

--   $Id$

import Autolib.Boxing.Position
import Autolib.Boxing.Class

import Data.List ( inits, transpose )



pack_tight :: Boxing c => [ (Position, c) ] -> c
-- koordinaten einfach übernehmen
-- und bbox drumbauen, z. b. für graphen
pack_tight pcs = 
    let ( ul, or ) = minimax $ do
	     ( p, c ) <- pcs
	     [ p, p + bounding_box c ]
	-- assume unten links = (0,0) 
    in  set_bounding_box or $ pack pcs

fbox :: Boxing c => Double -> c -> c
-- rahmen drum
fbox d c = 
    let off = Position { width = d, height = d }
	dim = bounding_box c + 2 * off
    in  set_bounding_box dim $ pack [ (off, c) ]

beside :: Boxing c => c -> c -> c
beside l r = pack_tight 
       [ ( 0                         , l )
       , ( no_height $ bounding_box l, r )
       ]

above ::  Boxing c => c -> c -> c
above l r = pack_tight 
       [ ( 0                         , l )
       , ( no_width  $ bounding_box l, r )
       ]

row, column :: Boxing c => [ c ] -> c
row = foldr1 beside
column = foldr1 above

---------------------------------------------------------

zipmax :: (Ord a, Num a) => [a] -> [a] -> [a]
-- paarweise maxima, ansonsten elemente selbst
zipmax xs ys = zipWith max xs ( ys ++ repeat 0 )

partsum :: (Ord a, Num a) => [a] -> [a]
partsum = map sum . inits

grid :: Boxing c => [[ c ]] -> c
grid css = 
    let widths  = partsum 
		$ foldr1 zipmax 
		$ map (map ( width . bounding_box)) css
        heights = partsum 
		$ foldr1 zipmax 
		$ transpose
                $ map (map ( height . bounding_box)) css
        con = do 
	    (y, cs) <- zip [0..] css -- zeilenweise
	    (x, c)  <- zip [0..] cs  -- elementweise in zeile
	    return 
	      ( Position { width = widths !! x , height = heights !! y } , c )
    in	set_bounding_box 
	   ( Position { width = last widths, height = last heights } )
	$ pack con


