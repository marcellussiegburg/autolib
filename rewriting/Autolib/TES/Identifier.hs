module Autolib.TES.Identifier 

( Identifier -- abstract
, mknullary
, mkunary
, mkbinary
, mk
)

where

--   $Id$

import Autolib.Set

import qualified Autolib.ToTex 
import Autolib.ToDoc
import Autolib.Reader

import Autolib.TES.Parsec
import Autolib.Symbol

import Data.List (intersperse)
import Autolib.Util.Size
import Autolib.Util.Wort
import Autolib.Hash

import Data.Char
import Data.Int
import Data.Typeable

-- | don't derive Eq and Ord since arity should be ignored
data Identifier = Identifier 
		{ hash_code :: {-# UNPACK #-} ! Int32
		, name	    :: {-# UNPACK #-} ! String
		, i_arity   :: {-# UNPACK #-} ! Int
		} deriving Typeable

instance Hash Identifier where hash = hash_code

instance Size Identifier where size = const 1

escape [] = []
escape ( c : cs ) = case c of
    '<' -> "&lt;" ++ escape cs
    '>' -> "&gt;" ++ escape cs
    _   -> c :       escape cs


mk :: Int -> String -> Identifier
mk a cs = Identifier
	{ hash_code = hash cs
	, i_arity = a
	, name = cs
	}

instance Eq Identifier where 
   x == y = ( hash_code x , name x ) == ( hash_code y , name y )

instance Ord Identifier where
   compare x y = compare ( hash_code x , name x ) ( hash_code y , name y )

instance Symbol Identifier where
     arity = i_arity
     set_arity a x = x { i_arity = a }
     pool = do w <- alles ( pool :: [ Char ] ) 3
	       guard $ not $ null w
	       return $ mknullary w
     stringify    = concat 
		  . intersperse "+"
		  . map name

mknullary :: String -> Identifier
mknullary = mk 0 

mkunary :: String -> Identifier
mkunary = mk 1

mkbinary :: String -> Identifier
mkbinary = mk 2


type Signature = Set Identifier

mkusig :: [ String ] -> Signature
-- make unary signature ( for string rewriting )
mkusig fs = mkSet $ do
    f <- fs
    return $ mkunary f

instance Show Identifier where
    show = render . toDoc

instance ToDoc Identifier where 
    toDoc = text . name
instance Reader Identifier where 
    readerPrec p = do
        i <- many1 (     satisfy isAlphaNum 
                     <|> Autolib.Reader.char '_'
                     <|> Autolib.Reader.char '\'' -- TPDB needs that
                   )
	     <|> operator trs
	whiteSpace trs
	return $ mk (-1) i


instance Autolib.ToTex.ToTex Identifier where
    toTex i = Autolib.ToTex.Macro "mathit" 
	      [ Autolib.ToTex.Req $ Autolib.ToTex.Direct $ name i ]
