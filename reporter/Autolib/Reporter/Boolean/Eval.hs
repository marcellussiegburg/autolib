module Reporter.Boolean.Eval where

--  $Id$

import Reporter.Boolean.Data
import Reporter.Type
import Reporter.Stream
import Reporter.Iterator
import Reporter.Proof

import Output

build :: Boolean ( Iterator Proof )
     -> Reporter.Stream.Type
build ( Atomic i ) = make i
build ( Not x ) = nicht $ build x
build ( Fun op xs ) = 
    let fun = case op of 
	    And -> und ; Or -> oder ; First -> erster
    in  fun $ map build xs

eval :: Boolean ( Iterator Proof )
     -> Reporter ( Either Output Proof )
eval = exec . build




