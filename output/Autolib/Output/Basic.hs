module Autolib.Output.Basic where

--  $Id$

import Autolib.Output.Type

lead :: Output -> Output -> Output
lead x y = Above x $ Nest y

