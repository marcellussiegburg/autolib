module GVKnoten.Parser where

import GVKnoten.Type

data Field = Label String
	   | Pos ( Int, Int )
	    | Unknown
	   deriving Show

parse :: String -> [(Field,String)]
parse str = 
    let ( pre, '=' : post ) = span (/= '=') str
    in  case pre of
	     "label" -> do ( z, rest) <- reads post
			   return ( Label (show ( z :: Int ) ), rest )
	     "pos"   -> do ( s, rest) <- reads post
			   let (x, ',': y) = span (/= ',') s
                           return ( Pos (read x, read y) , rest )
	     _       -> []

instance Read Field where 
    readsPrec p str = parse str


build :: String -> [ Field ] -> GVKnoten
build = error "build undef."

readline :: String -> [GVKnoten]
readline str = do
    (i, rest) <- lex str 
    (fields, _ ) <- reads rest
    return $ build i fields

