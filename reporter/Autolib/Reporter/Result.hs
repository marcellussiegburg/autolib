module Reporter.Result where

{-

$Id$

f�r email-aufgaben: die einzel-korrektoren rufen das hier auf,
um das resultat an den Manager (bin/Run.hs) mitzuteilen

schreibt (Maybe Int) in File in aktuelles directory (falls erfolgreich)
sonst: gar nichts.

warum schreiben wir nicht gleich in die datenbank?

1) das hier l�uft normalerweise im inneren hugs-prozess,
   der von Run.hs gestartet wird.
   der soll aber nicht SQLqueries usw. mit laden m�ssen
   (kann er im moment auch gar nicht, wg. -package HToolkit)

2) zum DB-schreiben m�ssten wir noch student- und aufg-Id �bertragen
   der Manager (Run) wei� das sowieso schon,
   also ist er die logische stelle, um das auszuf�hren

-}

import Reporter
import ToDoc

import Right
import Wrong

import qualified Exception

fname :: String
fname ="result.text"

wrapper :: Reporter Int -> IO ()
wrapper r = do
    let (res, com) = export r
    print ( com :: Doc )
    case res of
        Just i ->  do
	    right $ i
 	Nothing -> do
            wrong
    put res

put :: Maybe Int -> IO ()
put res = do
    writeFile fname $ show res
    
get :: IO ( Maybe Int )
get = do
       cs <- readFile $ fname
       return $ read cs
  `Exception.catch` 
       \ any -> return Nothing
    