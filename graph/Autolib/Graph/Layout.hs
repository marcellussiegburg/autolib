module Autolib.Graph.Layout where

--  $Id$

import Autolib.Graph.Graph

unlabelled :: GraphC a => Graph a -> Graph a
unlabelled g = g { show_labels = False
		   , layout_hints = "-Nwidth=0.1" : layout_hints g
		   }

neato ::GraphC a => Graph a -> Graph a
neato g = g { layout_program = Neato }

dot :: GraphC a => Graph a -> Graph a
dot g = g { layout_program = Dot }


