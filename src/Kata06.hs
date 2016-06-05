module Kata06 where
-- from http://codekata.com/kata/kata06-anagrams/


import           Control.Monad       (join)
import           Data.Discrimination hiding (sort)
import           Data.List           (sort, zip)
import           Data.Map.Strict     (empty, insertWith, toList)

{-
20683
kata06  9.02s user 10.07s system 477% cpu 3.997 total
-}
-- main = do filetext <- readFile "wordlist2.txt"
--           print $ length (results filetext)

-- results ft = filter (\x -> length x > 1) (map snd . toList $ foozle (words ft))

-- foozle [] = empty
-- foozle (w:ws) = insertWith (++) (sort w) [w] (foozle ws)

{-
cat wordlist2.txt| time kata06 > /dev/null
kata06 > /dev/null  44.21s user 49.43s system 601% cpu 15.565 total
-}

main = interact $ unlines . map unwords . (runSort . sortingBag) sorting . join zip . lines

{-
Local Variables:
compile-command: "stack -j8 install"
End:
-}
