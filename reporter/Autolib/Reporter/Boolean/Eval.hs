module Reporter.Boolean.Eval where

--  $Id$

import Reporter.Boolean.Data
import Reporter.Type
import Reporter.Stream
import Reporter.Iterator
import Reporter.Proof

import Output
import ToDoc

build :: Boolean ( Iterator Proof )
     -> Reporter.Stream.Type
build f @ ( Atomic i ) = make ( toDoc f ) i
build f @ ( Not x ) = nicht ( toDoc f ) $ build x
build f @ ( Fun op xs ) = 
    let fun = case op of 
	    And -> und ; Or -> oder ; First -> erster
    in  fun ( toDoc f ) $ map build xs

eval :: Boolean ( Iterator Proof )
     -> Reporter ( Either Output Proof )
eval = exec . build




