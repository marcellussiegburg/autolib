module GVKnoten.Parser where

import GVKnoten.Type
import Set
import Monad (guard)
import Maybe

data Field =   Label  String
             | Pos    ( Int, Int )
             | Width  Float
             | Height Float
             | Unknown
             deriving Show

-- parse ein File in eine GVKnoten-Liste
parse15 :: String -> IO ( [ GVKnoten ] )
parse15 file = do
	string <- readFile file
	return ( parse14 string )	

-- parse ein File (als String) in eine GVKnoten-Liste
parse14 :: String -> [ GVKnoten ]
parse14 string
	| string == [] = []
	| otherwise    = if ( isJust parse_line )
	                  then [ GVKnoten { ident  = show       id
							                , label  = showLabel  l
												 , pos    = showPos    p
												 , width  = showWidth  w
												 , height = showHeight h
												 }
								  ] ++ parse_next_line
							else parse_next_line 
							where 
									(id,(l:p:w:h:rest)) = fromJust(parse5 $ head $ lines string)
									line                = head $ lines string
									parse_line          = parse5 line
									parse_next_line     = parse14 $ unlines $ tail $ lines string

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

parse7 :: String -> [ Field ] 
parse7 str = let ( pre, '=' : post ) = span (/= '=') str
   	       in case pre of
	     "label" -> let ( pre2, ',' : post2 ) = span (/= ',') post
		             in [ Label pre2 ] ++ parse7 post2 
	     "pos"   -> let 
                       ( pre2 , ' ' : post2 ) = span (/= ' ') post
                       ('\"' : px , ',' : py) = span (/= ',') 
							                           $ take ((length pre2)-2) pre2
		             in [ Pos((read px)::Int,(read py)::Int) ] ++ parse7 post2 
	     "width" -> let ( '\"' : pre2, ',' : post2 ) = span (/= ',') post
		             in [ Width ((read (init pre2))::Float) ] ++ parse7 post2 
	     "height"-> let ( '\"' : pre2, ']' : post2 ) = span (/= ']') post
		             in [ Height ((read (init pre2))::Float) ]
	     _       -> parse7 $ tail str 

-- bekommt eine Zeile aus dem File und splittet die vordere Zahl ab
parse5 :: String -> Maybe ( Int , [ Field ] )
parse5 str =
	let ( pre, post ) = span (/= '['	) str
	in if ( ( (mkSet str) `intersect` (mkSet "width=" )) == (mkSet "width=") )
	    then (Just( (read pre)::Int , parse7 (tail post) ))
	    else Nothing

parse12 :: String -> [ GVKnoten ]
parse12 string = do
	guard (isJust $ parse5 $ head $ lines string) 
	return GVKnoten { ident  = show       id
	                , label  = showLabel  l
						 , pos    = showPos    p
						 , width  = showWidth  w
						 , height = showHeight h
						 } ++ (parse12 $ unlines $ tail $ lines string)
	where (id,(l:p:w:h:rest)) = fromJust(parse5 $ head $ lines string)

instance Read Field where 
    readsPrec p str = parse str

build :: String -> [ Field ] -> GVKnoten	
build = error "build undef."

readline :: String -> [GVKnoten]
readline str = do
    (i, rest) <- lex str 
    (fields, _ ) <- reads rest
    return $ build i fields

showLabel  :: Field -> String
showLabel  (Label string) = string

showPos    :: Field -> (Int, Int)
showPos    (Pos pos) = pos

showWidth  :: Field -> Float
showWidth  (Width w) = w

showHeight :: Field -> Float
showHeight (Height h) = h
