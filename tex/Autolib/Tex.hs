module Autolib.Tex where

--  $Id$

import Autolib.ToDoc

data Tex = Direct String
	 | Macro { name :: String
		 , args :: [ Arg ]
		 }
	 | Env   { name :: String
		 , args :: [ Arg ]
		 , contents :: Tex
		 }

instance ToDoc Tex where
    toDoc ( Direct cs ) = text cs
    toDoc ( m @ Macro {} ) 
	=  text ( "\\" ++ name m ) 
	<> cat ( map toDoc $ args m )
    toDoc ( e @ Env {} )
	= vcat [    text ( "\\begin{" ++ name e ++ "}" )
		 <> cat ( map toDoc $ args e )
	       , nest 2 $ toDoc ( contents e )
	       ,    text ( "\\end{" ++ name e ++ "}" )
	       ]

instance Show Tex where show = render . toDoc

data Arg = Opt Tex
	      | Req Tex

instance ToDoc Arg where
    toDoc ( Opt t ) = brackets $ toDoc t
    toDoc ( Req t ) = braces   $ toDoc t

instance Show Arg where show = render . toDoc