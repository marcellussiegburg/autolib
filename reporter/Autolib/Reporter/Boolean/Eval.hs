module Reporter.Boolean.Eval where

--  $Id$

import Reporter.Boolean.Data
import Reporter.Stream
import Reporter.Iterator

build :: Boolean ( Iterator ( Bool, String ) )
     -> Reporter.Stream.Type
build ( Atomic i ) = make i
build ( Not x ) = nicht $ build x
build ( And xs ) = und $ map build xs
build ( Or xs ) = oder $ map build xs

eval :: Boolean ( Iterator ( Bool, String ) )
     -> Reporter ( Maybe Bool, String )
eval = exec . build




