module Main where

import DataBase
import EntryManager
import Filter
import System.IO
import Test.Tasty
import Test.Tasty.HUnit
import qualified TestUtils as TU

main :: IO ()
main = defaultMain $ testGroup "all tests" [testFilter, testCommands]

testFilter :: TestTree
testFilter = testGroup "testFilter" [testParseFilterInput, testRunFilter]

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

testCommands :: TestTree
testCommands = testGroup "integration tests for commands" [testAdd]

testAdd :: TestTree
testAdd =
  testGroup
    "test `add' command"
    [ let testResult = do
            mockIOHandle <- openFile "sampleTest" ReadMode
            discardHandle <- TU.getNullHandle
            prepareNewEntry mockIOHandle discardHandle "toldi_arany-janos.epub"
          newEntry = Book "toldi_arany-janos.epub" "Toldi" [Author (Just "János") "Arany"] ["petrencesrud", "folkish", "poetry", "hungarian"]
       in testCase "add a single book" $ testResult @?>>= pure newEntry
    ]

(@?>>=) :: (Eq a, Show a, HasCallStack) => IO a -> IO a -> Assertion
mx @?>>= my = do
  x <- mx
  y <- my
  x @?= y
