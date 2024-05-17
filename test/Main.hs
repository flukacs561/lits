module Main where

import qualified Data.Set as Set
import DataBase
import EntryManager
import Filter
import Test.Tasty
import Test.Tasty.HUnit
import TestUtils

main :: IO ()
main = defaultMain $ testGroup "Great American Novel Test Suite" [testFilter, testAdd, testAddTag, testRemoveTag]

testDirectory :: FilePath
testDirectory = "./test-data"

testFilter :: TestTree
testFilter = testGroup "test filtering functionality" [testParseFilterInput, testRunFilter]

testParseFilterInput :: TestTree
testParseFilterInput =
  testGroup
    "test filter input parser"
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
    "test `filter' command"
    [ let testDescription = "only tags"
          testFilterO = Just $ FilterO Nothing Nothing ["american", "indian"]
          targetBook = "the-last-of-the-mohicans_j-f-cooper.epub"
       in testCase testDescription $ bookInList targetBook (runFilter testFilterO testDB) @?= True,
      let testDescription = "non-matcher book is excluded"
          testFilterO = Just $ FilterO Nothing Nothing ["american", "whale"]
          targetBook = "the-last-of-the-mohicans_j-f-cooper.epub"
       in testCase testDescription $ bookInList targetBook (runFilter testFilterO testDB) @?= False,
      let testDescription = "multiple hits"
          testFilterO = Just $ FilterO Nothing Nothing ["english", "novel"]
       in testCase testDescription $ runFilter testFilterO testDB @?= testDB,
      let testDescription = "match author first and last name"
          testFilterO = Just $ FilterO Nothing (Just "H") ["novel", "american"]
          targetBooks =
            [ "moby-dick_herman-melville.epub",
              "uncle-toms-cabin_harriet-beecher-stowe.epub"
            ]
       in testCase testDescription $ booksAllInList targetBooks (runFilter testFilterO testDB) @?= True,
      let testDescription = "if empty FilterO, match all books"
          testFilterO = Nothing
       in testCase testDescription $ runFilter testFilterO testDB @?= testDB
    ]

testAdd :: TestTree
testAdd =
  testGroup
    "test `add' command"
    [ let mockInput = "add-gravitys-rainbow"
          expectedResult =
            Book
              "gravitys-rainbow_thomas-pynchon.epub"
              "Gravity's Rainbow"
              (Set.singleton $ Author (Just "Thomas") "Pynchon")
              $ Set.fromList
                ["satire", "english", "novel", "american"]
          testDescription = "check all fields filled"
       in buildTest testDescription mockInput expectedResult,
      let mockInput = "add-iliad"
          expectedResult =
            Book
              "iliad_homer.epub"
              "Iliad"
              (Set.singleton $ Author Nothing "Homer")
              $ Set.fromList
                ["epic", "ancient", "greek", "troy"]
          testDescription = "check author no first name"
       in buildTest testDescription mockInput expectedResult,
      let mockInput = "add-sicp"
          expectedResult =
            Book
              "sicp.pdf"
              "Structure and Interpretation of Computer Programs"
              ( Set.fromList
                  [ Author (Just "Harold") "Abelson",
                    Author (Just "Gerald Jay") "Sussman"
                  ]
              )
              $ Set.fromList
                ["cs", "wizzard", "lisp", "scheme"]
          testDescription = "check multiple authors"
       in buildTest testDescription mockInput expectedResult,
      let mockInput = "add-infinite-jest"
          expectedResult =
            Book
              "infinite-jest_david-foster-wallace.epub"
              "Infinite Jest"
              (Set.singleton $ Author (Just "David Foster") "Wallace")
              Set.empty
          testDescription = "check no tags"
       in buildTest testDescription mockInput expectedResult
    ]
  where
    buildTest :: TestName -> String -> Book -> TestTree
    buildTest testDescription mockInput expectedResult =
      let testResult = do
            mockIOHandle <- openFile (testDirectory <> "/" <> mockInputFolder <> mockInput) ReadMode
            discardHandle <- getNullHandle
            prepareNewEntry mockIOHandle discardHandle testDirectory (fileName expectedResult)
          testDescription' = "add " <> title expectedResult <> " (" <> testDescription <> ")"
       in testCase testDescription' $ testResult @?>>= expectedResult

testAddTag :: TestTree
testAddTag =
  testGroup
    "test `add tag' command"
    [ let file = "blood-meridian_cormac-mccarthy.epub"
          mockInput = "add-tag-blood-meridian"
          testDescription = "add a single tag to Blood Meridian"
          tagsAdded = ["violence"]
       in buildTest testDescription mockInput file tagsAdded,
      let file = "blood-meridian_cormac-mccarthy.epub"
          mockInput = "add-multiple-tags-blood-meridian"
          testDescription = "add multiple tags to Blood Meridian"
          tagsAdded = ["judge-holden", "the-kid"]
       in buildTest testDescription mockInput file tagsAdded,
      let file = "blood-meridian_cormac-mccarthy.epub"
          mockInput = "add-zero-tags-blood-meridian"
          testDescription = "add zero tags to Blood Meridian"
          originalTags = safeGetTags testDB file
       in testCase testDescription $
            do
              mockIOHandle <- getMockHandle mockInput
              discardHandle <- getNullHandle
              newDB <- runAddTags mockIOHandle discardHandle file testDB
              originalTags @?= safeGetTags newDB file,
      let file = "the-scarlett-letter_nathaniel-hawthorne.epub"
          mockInput = "add-while-add-tag-the-scarlett-letter"
          testDescription = "add nonexistent book The Scarlett Letter"
          newEntry =
            Book
              "the-scarlett-letter_nathaniel-hawthorne.epub"
              "The Scarlett Letter"
              (Set.singleton $ Author (Just "Nathaniel") "Hawthorne")
              (Set.fromList ["shame", "american", "novel", "english"])
       in testCase testDescription $ do
            mockIOHandle <- getMockHandle mockInput
            discardHandle <- getNullHandle
            newDB <- runAddTags mockIOHandle discardHandle file testDB
            assertBool "failed to add entry" $ newEntry `elem` newDB
    ]
  where
    buildTest testDescription mockInput file tagsAdded = testCase testDescription $
      do
        mockIOHandle <- getMockHandle mockInput
        discardHandle <- getNullHandle
        newDB <- runAddTags mockIOHandle discardHandle file testDB
        assertBool ("failed to add new tags: " <> show tagsAdded) (bookHasTags newDB file $ Set.fromList tagsAdded)

testRemoveTag :: TestTree
testRemoveTag =
  testGroup
    "test `remove-tag' command"
    [ let testDescription = "remove single tag from Uncle Tom's Cabin"
          file = "uncle-toms-cabin_harriet-beecher-stowe.epub"
          tagsToRemove = ["slaves"]
       in buildTest testDescription file tagsToRemove,
      let testDescription = "remove multiple tags from Uncle Tom's Cabin"
          file = "uncle-toms-cabin_harriet-beecher-stowe.epub"
          tagsToRemove = ["blacks", "novel"]
       in buildTest testDescription file tagsToRemove,
      let testDescription = "remove zero tags from Uncle Tom's Cabin"
          file = "uncle-toms-cabin_harriet-beecher-stowe.epub"
          tagsToRemove = []
       in buildTest testDescription file tagsToRemove
    ]
  where
    getInputFromTagsToRemove :: [Tag] -> Set.Set Tag -> [String]
    getInputFromTagsToRemove [] _ = [""]
    getInputFromTagsToRemove (t : ts) remainingTags =
      -- note that indexing in a Set starts at 0, while the numbering of tags in LiTS starts from 1.
      show (Set.findIndex t remainingTags + 1) : getInputFromTagsToRemove ts (Set.delete t remainingTags)

    buildTest :: TestName -> FilePath -> [Tag] -> TestTree
    buildTest testDescription file tagsToRemove = testCase testDescription $ do
      mockIOHandle <- prepareMockHandle $ getInputFromTagsToRemove tagsToRemove (safeGetTags testDB file)
      discardHandle <- getNullHandle
      newDB <- runRemoveTags mockIOHandle discardHandle file testDB
      let oldTags = safeGetTags testDB file
          newTags = safeGetTags newDB file
          errorMsg = unlines ["failed to remove tags:" <> show tagsToRemove,
                              "old tags:" <> show (Set.toAscList oldTags),
                              "new tags:" <> show (Set.toAscList newTags)]
       in assertBool errorMsg $ Set.difference oldTags (Set.fromList tagsToRemove) == newTags

