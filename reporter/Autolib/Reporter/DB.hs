module Reporter.DB obsolete

-- DONT use this module,
-- use Reporter.Result instead,
-- read the explanations there

( module Reporter.Type
, module Reporter.DB
)

where

-- $Id$

import Reporter.Type
import ToDoc

import SQLqueries
import Right
import Wrong

-- for use in classical autotool problems
db_reporter :: ( a -> Reporter Int ) -> ( a -> IO String )
db_reporter fr = \ student -> do
    let (res, com) = export $ fr student
    print ( com :: Doc )
    case res of
        Just i ->  do
	    bepunkteStudentDB snr anr ( Ok i ) hilo
	    right $ i
 	Nothing -> do
	    bepunkteStudentDB snr anr ( No   ) hilo
            wrong

