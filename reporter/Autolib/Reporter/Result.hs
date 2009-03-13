{-# language ScopedTypeVariables #-}

module Autolib.Reporter.Result where

{-

$Id$

für email-aufgaben: die einzel-korrektoren rufen das hier auf,
um das resultat an den Manager (bin/Run.hs) mitzuteilen

schreibt (Maybe Int) in File in aktuelles directory (falls erfolgreich)
sonst: gar nichts.

warum schreiben wir nicht gleich in die datenbank?

1) das hier läuft normalerweise im inneren hugs-prozess,
   der von Run.hs gestartet wird.
   der soll aber nicht SQLqueries usw. mit laden müssen
   (kann er im moment auch gar nicht, wg. -package HToolkit)

2) zum DB-schreiben müssten wir noch student- und aufg-Id übertragen
   der Manager (Run) weiß das sowieso schon,
   also ist er die logische stelle, um das auszuführen

-}

import Autolib.Reporter.Type
import Autolib.ToDoc

import Autolib.Right
import Autolib.Wrong

import qualified Control.Exception

wrapper :: Reporter Int -> IO ( Maybe Int )
wrapper r = do
    let (res, com) = export r
    print ( com :: Doc )
    case res of
        Just i ->  do
	    right $ i
 	Nothing -> do
            wrong
    return res

result_string :: Maybe Int -> String
result_string mres = case mres of
    Nothing -> "NO"
    Just i  -> "OK # Size: " ++ show i

put :: FilePath -> Maybe Int -> IO ()
put fpath res = do
    writeFile fpath $ show res
    
get :: FilePath -> IO ( Maybe Int )
get fpath = do
       cs <- readFile $ fpath
       return $ read cs 
    `Control.Exception.catch`
           \ ( e :: Control.Exception.IOException ) -> return Nothing
           -- \ any -> return Nothing

    
