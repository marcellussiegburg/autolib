module Autolib.Graph.Layout where

--  $Id$

import Autolib.Graph.Graph

unlabelled :: Graph a -> Graph a
unlabelled g = g { show_labels = False
		   , layout_hints = "-Nwidth=0.1" : layout_hints g
		   }

neato :: Graph a -> Graph a
neato g = g { layout_program = "neato" }

dot :: Graph a -> Graph a
dot g = g { layout_program = "dot" }

twopi :: Graph a -> Graph a
twopi g = g { layout_program = "twopi" }

