module XiangqiFormat where

-- DIESES MODUL NICHT ÄNDERN!!!!

import System.IO
import Util
import Data.List
import Control.Monad
import Test.HUnit
import qualified Data.Char as Char

import XiangqiBot

isRow, isCol :: Char -> Bool
isRow = between '0' '9' -- row
isCol = between 'a' 'i' -- column

between low high x = low <= x && x <= high

isValidMove :: String -> Bool
isValidMove [srcCol, srcRow, '-', dstCol, dstRow] =
    isCol srcCol && isRow srcRow && isCol dstCol && isRow dstRow

isValidMove _ = False


isValidListOf :: (String -> Bool) -> String -> Bool
isValidListOf validItem ('[' : str) = 
    (last str == ']') && all validItem (Util.splitOn ',' $ init str)


assertFormat :: String -> (String -> Bool) -> Assertion
assertFormat actual check =
    unless (check actual) (assertFailure msg)
    where msg = "Wrong format! Looks like: \"" ++ actual ++ "\""

--------------------------------------------------------------------------

testFormat = TestList
    [ (TestLabel "MOVE FORMAT WRONG!" (TestCase (assertFormat (XiangqiBot.getMove "rheagaehr/9/1c5c1/s1s1s1s1s/9/9/S1S1S1S1S/1C5C1/9/RHEAGAEHR r") isValidMove)))
    , (TestLabel "LIST FORMAT WRONG!" (TestCase (assertFormat (XiangqiBot.listMoves "rheagaehr/9/1c5c1/s1s1s1s1s/9/9/S1S1S1S1S/1C5C1/9/RHEAGAEHR r") $ isValidListOf isValidMove)))
    ]

main :: IO (Counts, Int)
main = runTestText (putTextToHandle stdout False) testFormat
