module Output.Basic where

--  $Id$

import Output.Type

lead :: Output -> Output -> Output
lead x y = Above x $ Nest y

