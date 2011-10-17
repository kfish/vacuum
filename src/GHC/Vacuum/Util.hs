


module GHC.Vacuum.Util (
   module GHC.Vacuum.Util
) where

import Data.List
import Data.Char
import Data.Bits
import Data.Array.IArray hiding ((!))
import qualified Data.Array.IArray as A

hash :: String -> Int
hash [] = 0
hash  s = go 0 (fmap ord s)
  where go !h [] = h
        go !h (n:ns) =
          let a = (h `shiftL` 4)
              b = a + n
              c = b .&. 0xf0000000
              !d = case c==0 of
                    False -> let !e = c `shiftR` 24
                              in b `xor` e
                    True  -> b
              !e = complement c
              !f = d .&. e
          in go f ns

{-
unsigned long
elfhash(const char *s)
{
  unsigned long h=0, g;
  while (*s){
    h = (h << 4) + *s++;
    if((g = h & 0xf0000000))
      h ^= g >> 24;
    h &= ~g;
  }
  return h;
}
-}

------------------------------------------------
{-
look :: HValue -> [(HValue, a)] -> Maybe a
look _      [] = Nothing
look hval ((x,i):xs)
  | hval .==. x = Just i
  | otherwise   = look hval xs

(.==.) :: HValue -> HValue -> Bool
a .==. b = a `seq` b `seq`
  (0 /= I# (reallyUnsafePtrEquality# a b))

lookLazy :: HValue -> [(HValue, a)] -> Maybe a
lookLazy _      [] = Nothing
lookLazy hval ((x,i):xs)
  | hval =.= x = Just i
  | otherwise   = lookLazy hval xs

(=.=) :: HValue -> HValue -> Bool
a =.= b = (0 /= I# (reallyUnsafePtrEquality# a b))
-}

dumpArray :: Array Int a -> [a]
dumpArray a = let (m,n) = bounds a
              in fmap (a A.!) [m..n]

mapfst f = \(a,b) -> (f a,b)
mapsnd f = \(a,b) -> (a,f b)
f *** g = \(a, b) -> (f a, g b)

{-
p2i :: Ptr a -> Int
i2p :: Int -> Ptr a
p2i (Ptr a#) = I# (addr2Int# a#)
i2p (I# n#) = Ptr (int2Addr# n#)
-}

------------------------------------------------



