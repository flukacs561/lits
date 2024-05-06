module Main where

import Filter
import Test.Tasty
import Test.Tasty.HUnit
import qualified TestUtils as TU

main :: IO ()
main = defaultMain $ testGroup "testFilter" [testParseFilterInput, testRunFilter]

testParseFilterInput :: TestTree
testParseFilterInput =
  testGroup
    "test parseFilterInput"
    [ let testDescription = "all 3 fields provided"
          testInput = words "-t maths cat -T higher -a lurie"
          resultingFilterO = Just $ FilterO (Just "higher") (Just "lurie") ["maths", "cat"]
       in buildTest testDescription testInput resultingFilterO,
      let testDescription = "author missing"
          testInput = words "-t maths cat -T higher"
          resultingFilterO = Just $ FilterO (Just "higher") Nothing ["maths", "cat"]
       in buildTest testDescription testInput resultingFilterO,
      let testDescription = "tags missing"
          testInput = words "-T higher -a lurie"
          resultingFilterO = Just $ FilterO (Just "higher") (Just "lurie") []
       in buildTest testDescription testInput resultingFilterO,
      let testDescription = "tag flag present, but no tags provided -> should fail"
          testInput = words "-T higher -a lurie -t"
          resultingFilterO = Nothing
       in buildTest testDescription testInput resultingFilterO,
      let testDescription = "all 3 fields provided"
          testInput = words "-t maths cat -T higher -a lurie"
          resultingFilterO = Just $ FilterO (Just "higher") (Just "lurie") ["maths", "cat"]
       in buildTest testDescription testInput resultingFilterO
    ]
  where
    buildTest description input result = testCase description $ parseFilterInput input @?= result

testRunFilter :: TestTree
testRunFilter =
  testGroup
    "test runFilter"
    [ let testDescription = "only tags"
          testFilterO = Just $ FilterO Nothing Nothing ["american", "indian"]
          targetBook = "the-last-of-the-mohicans_j-f-cooper.epub"
       in testCase testDescription $ TU.bookInList targetBook (runFilter testFilterO books) @?= True,
      let testDescription = "non-matcher book is excluded"
          testFilterO = Just $ FilterO Nothing Nothing ["american", "whale"]
          targetBook = "the-last-of-the-mohicans_j-f-cooper.epub"
       in testCase testDescription $ TU.bookInList targetBook (runFilter testFilterO books) @?= False,
      let testDescription = "multiple hits"
          testFilterO = Just $ FilterO Nothing Nothing ["english", "novel"]
       in testCase testDescription $ runFilter testFilterO books @?= books,
      let testDescription = "match author first and last name"
          testFilterO = Just $ FilterO Nothing (Just "H") ["novel", "american"]
          targetBooks =
            [ "moby-dick_herman-melville.epub",
              "uncle-toms-cabin_harriet-beecher-stowe.epub"
            ]
       in testCase testDescription $ TU.booksAllInList targetBooks (runFilter testFilterO books) @?= True,
      let testDescription = "if empty FilterO, match all books"
          testFilterO = Nothing
       in testCase testDescription $ runFilter testFilterO books @?= books
    ]
  where
    books = TU.testDB
