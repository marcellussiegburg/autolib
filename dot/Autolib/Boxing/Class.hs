--   $Id$

module Autolib.Boxing.Class where

-- anything that can be laid out in boxes (rectangles)

import Autolib.Boxing.Position


class Boxing b where

    -- each item has a bounding box:
    bounding_box :: b -> Position
    -- bounding_box can be set:
    set_bounding_box :: Position -> b -> b

    -- pack items in the container
    pack         :: [ (Position, b) ] -> b


