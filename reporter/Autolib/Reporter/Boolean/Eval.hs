module Autolib.Reporter.Boolean.Eval where

--  $Id$

import Autolib.Reporter.Boolean.Data
import Autolib.Reporter.Type
import Autolib.Reporter.Stream
import Autolib.Reporter.Iterator
import Autolib.Reporter.Proof

import Autolib.Output
import Autolib.ToDoc

build :: Boolean ( Iterator Proof )
     -> Autolib.Reporter.Stream.Type
build f @ ( Atomic i ) = make ( toDoc f ) i
build f @ ( Uf up x ) = 
    let fun = case up of 
	    Not -> nicht ; Success -> erfolg
    in  fun ( toDoc f ) $ build x
build f @ ( Bof op xs ) = 
    let ( doc , fun ) = case op of 
	    And -> ( toDoc f           , und    )
	    Or ->  ( text ".. or .."   , oder   )
	    Par -> ( text ".. par .."  , erster )
	    Before -> ( text ".. Before .."  , before )
    in  fun doc $ map build xs

eval :: Boolean ( Iterator Proof )
     -> Reporter ( Either Output Proof )
eval = exec . build




