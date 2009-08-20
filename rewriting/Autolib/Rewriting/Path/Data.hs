-- -*- mode: haskell -*-
{-# LANGUAGE TemplateHaskell #-}

module Autolib.Rewriting.Path.Data   where

--  $Id$

import Autolib.Reader
import Autolib.ToDoc

class     ( Reader x, ToDoc x ) => RT x
instance  ( Reader x, ToDoc x ) => RT x

class    ( RT f, RT w, RT t ) => DataC f w t
instance ( RT f, RT w, RT t ) => DataC f w t

data Data f w t =
     Path { from :: {-# UNPACK #-} ! f
	  , walk :: {-# UNPACK #-} ! w
	  , to   :: {-# UNPACK #-} ! t
	  }

$(derives [makeReader, makeToDoc] [''Data])

instance DataC f w t => Show ( Data f w t ) where
    show = render . toDoc

instance DataC f w t => Read ( Data f w t ) where
    readsPrec = parsec_readsPrec



