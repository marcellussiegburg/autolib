module Reporter.DB 

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


type DB_Reporter = String -- SNr
         -> String -- ANr
	 -> ATHighLow 
	 -> IO String

-- for use in classical autotool problems
db_reporter :: ( a -> Reporter Int ) -> ( a -> DB_Reporter )
db_reporter fr = \ student snr anr hilo  -> do
    let (res, com) = export $ fr student
    print ( com :: Doc )
    case res of
        Just i ->  do
	    bepunkteStudentDB snr anr ( Ok i ) hilo
	    right_with $ "OK # Size: " ++ show i
 	Nothing -> do
	    bepunkteStudentDB snr anr ( No   ) hilo
            wrong

