module Graph.Brinkmann where

-- -- $Id$

import Graph.Graph 
import Graph.Basic
import Graph.Ops
import Graph.Display

import Sets
import Util.Teilfolgen
import Control.Monad ( guard )
import ToDoc

brinkmann :: Graph Int
-- Bollobas, Modern Graph Theory, page 175
brinkmann = informed ( text "Brinkmann" )
	  $ texinformed ( "\\mathrm{Brinkmann}" )
	  $ normalize
	  $ brink 7 [ [( 0, 3), (1, 1) ]
		    , [(-1, 1), (1, 0) ]
		    , [(-1, 1), (0, 2) ]
		    ]

brink :: Int -> [[(Int, Int)]] -> Graph (Int, Int)
brink m args = informed ( text "Brink" <+> toDoc (m ,args) )
	   $ let v = cross ( mkSet [ 0 .. (length args - 1) ] )
			   ( mkSet [ 0 .. m - 1 ] )
	         e = mkSet $ do
		       (h, ds) <- zip [0..] args
		       ( dh, dw ) <- ds
		       w <- [ 0 .. m - 1 ]
		       return $ kante (h, w) (h+dh, (w + dw) `mod` m)
	     in ( mkGraph v e )
		{ layout_hints = "-Elen=2" }


