module Autolib.ORW.Print where

import Autolib.ORW.Data
import qualified Autolib.Exp as E

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Reporter
import Autolib.Symbol

orw2exp :: ORW s -> E.RX s
orw2exp o = 
    let pre = map E.Letter $ prefix o
        per = map E.Letter $ period o
    in  foldr E.Dot 
          ( E.PowerOmega $ foldr1 E.Dot per ) pre

instance Symbol c => Show ( ORW c ) where 
    show = show . orw2exp

instance Symbol c => ToDoc ( ORW c ) where 
    toDoc = toDoc . orw2exp
