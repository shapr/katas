module Kata08 where

import           Control.Monad
import           Data.Monoid
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit


{-
similar to previous kata, except concatenated sub words
all six letter words composed of two smaller words
first, find six letter words, in the process, pick up smaller words

al bums -> albums
bar ely -> barely
be foul -> befoul

simple unit test is, given input words of be, foul, befoul, should find be + foul = befoul
-}
-- findConcats :: [String] -> [String]
-- findConcats (w:ws) =

-- pushTest = assertEqual "foo" (findConcats ["al","bums","albums"]) "al + bums = albums"
pushTest = assertEqual "foo" 1 1

main = defaultMainWithOpts [testCase "foo" pushTest] mempty

{-
Local Variables:
compile-command: "stack -j8 install"
End:
-}
