module Hash where

-- $Log$
-- Revision 1.1  2004-04-03 20:00:22  joe
-- file
--
-- Revision 1.1  2002/12/12 22:01:55  joe
-- ground
--
-- Revision 1.3  2002/03/25 14:29:06  joe
-- phutball reorg, flankengott
--
-- Revision 1.2  2002/03/20 11:52:27  joe
-- sumpf
--
-- Revision 1.1  2002/03/03 14:10:36  joe
-- added
--

class Eq a => Hash a where
      hash :: a -> Int	-- TODO: should be unboxed word or something

instance (Hash a, Hash b) => Hash (a, b) where
    -- not recommended (should be cached instead)
    hash (a, b) = hash a + 3 + 107 * hash b

instance (Hash a, Hash b, Hash c) => Hash (a, b, c) where
    -- not recommended (should be cached instead)
    hash (a, b, c) = hash (a, hash (b, c))


instance (Hash a) => Hash [a] where
    hash [] = 314159
    hash (x : xs) = hash (x, xs) 

instance Hash Int where 
    hash = id

instance Hash Char where 
    hash = fromEnum



    