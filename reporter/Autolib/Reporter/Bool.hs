module Autolib.Reporter.Bool where

-- -- $Id$

import Autolib.Reporter.Iterator
import Autolib.Reporter.Type
import Autolib.Informed
import Autolib.ToDoc

-- three valued logic, kind of

noh :: Iterator Bool -> Iterator Bool
noh = fmap not

data State a b = Both_Left a b -- beide da, links als nächster dran
	       | Both_Right a b -- ... rechts ...
	       | Single_Left a
	       | Single_Right b

und :: Iterator Bool -> Iterator Bool  -> Iterator Bool
und l @ ( Iterator ldoc lstep lstart )
    r @ ( Iterator rdoc rstep rstart )
  =  Iterator ( funni "und" [ ldoc, rdoc ] )
    ( \ state -> case state of
        Both_Left a b -> do
            x <- lstep a
            return $ case x of
                 Left  a'    -> Left $ Both_Right a' b
                 Right False -> Right False
                 Right True  -> Left $ Single_Right b
        Single_Left a -> do
            x <- lstep a
            return $ case x of
                 Left a' -> Left $ Single_Left a'
                 Right s -> Right s
        Both_Right a b -> do
            y <- rstep b
            return $ case y of
                 Left  b'    -> Left $ Both_Left a b'
                 Right False -> Right False
                 Right True  -> Left $ Single_Left  a
        Single_Right b -> do
            y <- rstep b
            return $ case y of
                 Left b' -> Left $ Single_Right b'
                 Right s -> Right s
    ) ( Both_Left lstart rstart )

oder :: Iterator Bool -> Iterator Bool  -> Iterator Bool
oder l @ ( Iterator ldoc lstep lstart )
    r @ ( Iterator rdoc rstep rstart )
  =  Iterator ( funni "und" [ ldoc, rdoc ] )
    ( \ state -> case state of
        Both_Left a b -> do
            x <- lstep a
            return $ case x of
                 Left  a'    -> Left $ Both_Right a' b
                 Right False -> Left $ Single_Right b
                 Right True  -> Right True
        Single_Left a -> do
            x <- lstep a
            return $ case x of
                 Left a' -> Left $ Single_Left a'
                 Right s -> Right s
        Both_Right a b -> do
            y <- rstep b
            return $ case y of
                 Left  b'    -> Left $ Both_Left a b'
                 Right False -> Left $ Single_Left  a
                 Right True  -> Right True
        Single_Right b -> do
            y <- rstep b
            return $ case y of
                 Left b' -> Left $ Single_Right b'
                 Right s -> Right s
    ) ( Both_Left lstart rstart )


