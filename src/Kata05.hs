{-# LANGUAGE OverloadedStrings #-}
module Kata05 where

import           Crypto.Hash
import           Crypto.Hash.MD2        as MD2
import           Crypto.Hash.MD4        as MD4
import           Crypto.Hash.MD5        as MD5
import           Crypto.Hash.RIPEMD160  as RIPEMD160
import           Crypto.Hash.SHA1       as SHA1
import           Crypto.Hash.SHA224     as SHA224
import           Crypto.Hash.SHA256     as SHA256
import           Crypto.Hash.SHA384     as SHA384
import           Crypto.Hash.SHA512     as SHA512
import           Crypto.Hash.Tiger      as Tiger
import           Crypto.Hash.Whirlpool  as Whirlpool
import           Data.Bits
import           Data.BitVector         hiding (foldr)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as B hiding (foldl')
import           Data.ByteString.Base16
import           Data.Char              (ord)
import           Data.Foldable          (foldl')
-- import           Data.Word8
import           Data.Word

{-
how do I represent the result of the hashes and put that into the bit vector?
I need some way to set particular bits on the vector, and I need enough bits to save a wordlist
I need to produce multiple bits, one for the result of each hash
so let's say I start out with 8k bits

simplest way to do it is hashresult `mod` bitvector sizey
read the result from a string and mod that value? terrible cheesy ain't it?

<Darius> so, i'd start with a function to set the bloom filter's bits given the bits from the hash value
<Darius> and of course another one to check them -- actually these could both use a function to generate a lazy list of the indiices into the array
-}

hashes = [MD4.hash, MD5.hash, Tiger.hash]

-- A Bloom filter with 1% error and an optimal value of k, in contrast, requires only about 9.6 bits per element, regardless of the size of the elements.
main = do
       -- print $ encode $ MD5.hash "foo"
       -- print $ encode $ MD2.hash "foo"
       -- print $ showHash $ MD4.hash "foo"
       -- print $ encode (B.append "b" $ MD4.hash "foo")
       print $ (read ("0x" ++ (showHash $ MD4.hash "foo")) :: Integer)
       print $ roll $ Prelude.reverse $ (B.unpack $ MD4.hash "foo")
       print $ roll' (B.unpack $ MD4.hash "foo")



mixit hashes bitvector word = map (mixword bitvector) hashes
      where mixword = mixone word

mixone word bitvector hash = ((roll' . B.unpack) (hash word))  `mod` fromIntegral (size bitvector)

setbit bitvector int = size bitvector

-- buildFilter :: String -> BV ->
-- insertWord :: ByteString -> BV -> BV
-- insertWord w bv =

-- take a hash function, a BitVector and a word, return the bit
-- hashWord :: Num a => (ByteString -> ByteString) -> ByteString -> a

-- set bloom filter bits from hash value
-- size of filter

getIndices s n = encode $ MD5.hash s

bar = MD5.hash "foo"


hexalise s = concatMap (\c -> [ hex $ c `div` 16, hex $ c `mod` 16 ]) s
        where hex i
                | i >= 0 && i <= 9   = fromIntegral (ord '0') + i
                | i >= 10 && i <= 15 = fromIntegral (ord 'a') + i - 10
                | otherwise          = 0

hexaliseB :: B.ByteString -> B.ByteString
hexaliseB = B.pack . hexalise . B.unpack

splitB :: Int -> ByteString -> [ByteString]
splitB l b =
    if B.length b > l
        then
            let (b1, b2) = B.splitAt l b in
            b1 : splitB l b2
        else
            [ b ]

showHash :: B.ByteString -> String
showHash = map (toEnum.fromEnum) . hexalise . B.unpack

foo = B.unpack "foobar"


-- unroll :: Integer -> [Word8]
-- unroll = unfoldr step
--   where
--     step 0 = Nothing
--     step i = Just (fromIntegral i, i `shiftR` 8)

roll :: [Word8] -> Integer
roll   = foldr unstep 0
  where
    unstep b a = a `shiftL` 8 .|. fromIntegral b

roll' :: [Word8] -> Integer
roll' = foldl' unstep 0
      where unstep a b = a `shiftL` 8 .|. fromIntegral b

{-
Local Variables:
compile-command: "stack -j8 install"
End:
-}
