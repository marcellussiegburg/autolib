module GVKnoten.Parser where

import GVKnoten.Type
import Set
import Monad (guard)
import Maybe
import Char  (isDigit)

data Field =   Label  String
             | Pos    ( Int, Int )
             | Width  Float
             | Height Float
             | Unknown
             deriving Show

-- parse file to GVNode-List
parse :: String -> IO ( [ GVKnoten ] )
parse file = do
	string <- readFile file
	return ( parse_all string )	

-- parse file as String to GVNode-List
parse_all :: String -> [ GVKnoten ]
parse_all string
   | string == [] = []
   | otherwise    
      = if ( isJust parseline )
         then [ GVKnoten { ident  = id
                         , label  = showLabel  l
                         , pos    = showPos    p
                         , width  = showWidth  w
                         , height = showHeight h
                         }
               ] ++ parse_next_line
         else parse_next_line 
        where 
           (id,(l:p:w:h:rest)) = fromJust(parseline)
           line                = head $ lines string
           parseline           = parse_line line
           parse_next_line     = parse_all $ unlines $ tail $ lines string

parse_joe :: String -> [(Field,String)]
parse_joe str = 
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
		             in if ( (head pre2) == '\"' )
						     then [ Label ( init $ tail pre2 ) ] ++ parse7 post2
							  else [ Label pre2 ] ++ parse7 post2 
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
parse_line :: String -> Maybe ( String , [ Field ] )
parse_line str =
	let 
		( pre, post ) = span (/= '['	) str  -- geht mit lex viel besser!!
		( pre2, post2)= head $ lex str
		( lbl, l_po ) = span (/= '='  ) post
	in if ( and [ ( length $ words pre ) == 1 , isDigit $ head pre2 ] )
	    then 
		    if ( (length lbl) < 5 ) -- dann kein label enthalten
			 then (Just( pre2 , (Label pre2 ) : (parse7 post2)))
			 else (Just( pre2 , parse7 (tail post2) ))
	    else Nothing

instance Read Field where 
    readsPrec p str = parse_joe str

build :: String -> [ Field ] -> GVKnoten	
build = error "build undef."

readline :: String -> [GVKnoten]
readline str = do
    (i, rest) <- lex str 
    (fields, _ ) <- reads rest
    return $ build i fields

showLabel  :: Field -> String
showLabel  (Label string) = 
           let ( pre , post ) = span (/= '\\') string
			  in if (and [( length post ) > 2, (take 2 post) == "\\n" ] )
			      then ( pre ++ "\n" ++ ( drop 2 post ) )
					else string

showPos    :: Field -> (Int, Int)
showPos    (Pos pos) = pos

showWidth  :: Field -> Float
showWidth  (Width w) = w

showHeight :: Field -> Float
showHeight (Height h) = h
