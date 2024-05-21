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
testDirectory = "./test-data/"

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
          testFilterO = Just $ FilterO Nothing Nothing ["english"]
       in testCase testDescription $
            runFilter testFilterO testDB @?= (testDB `except` "iliad_homer.epub"),
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
    [ let mockInput =
            [ "Gravity's Rainbow",
              "Thomas",
              "Pynchon",
              "",
              "american",
              "novel",
              "english",
              "satire",
              ""
            ]
          expectedResult =
            Book
              "gravitys-rainbow_thomas-pynchon.epub"
              "Gravity's Rainbow"
              (Set.singleton $ Author (Just "Thomas") "Pynchon")
              $ Set.fromList
                ["satire", "english", "novel", "american"]
          testDescription = "check all fields filled"
       in buildTest testDescription mockInput expectedResult,
      let mockInput =
            [ "Iliad",
              "",
              "Homer",
              "",
              "epic",
              "ancient",
              "greek",
              "troy",
              "",
              ""
            ]
          expectedResult =
            Book
              "iliad_homer.epub"
              "Iliad"
              (Set.singleton $ Author Nothing "Homer")
              $ Set.fromList
                ["epic", "ancient", "greek", "troy"]
          testDescription = "check author no first name"
       in buildTest testDescription mockInput expectedResult,
      let mockInput =
            [ "Structure and Interpretation of Computer Programs",
              "Harold",
              "Abelson",
              "y",
              "Gerald Jay",
              "Sussman",
              "n",
              "cs",
              "wizzard",
              "lisp",
              "scheme",
              ""
            ]
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
      let mockInput =
            [ "Infinite Jest",
              "David Foster",
              "Wallace",
              "",
              ""
            ]
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
    buildTest :: TestName -> [String] -> Book -> TestTree
    buildTest testDescription mockInput expectedResult =
      let testResult = do
            mockInputHandle <- prepareMockHandle mockInput
            discardHandle <- getNullHandle
            prepareNewEntry mockInputHandle discardHandle testDirectory (fileName expectedResult)
          testDescription' = "add " <> title expectedResult <> " (" <> testDescription <> ")"
       in testCase testDescription' $ testResult @?>>= expectedResult

testAddTag :: TestTree
testAddTag =
  testGroup
    "test `add tag' command"
    [ let file = "blood-meridian_cormac-mccarthy.epub"
          testDescription = "add a single tag to Blood Meridian"
          tagsToAdd = ["violence"]
       in buildTest testDescription file tagsToAdd,
      let file = "blood-meridian_cormac-mccarthy.epub"
          testDescription = "add multiple tags to Blood Meridian"
          tagsAdded = ["judge-holden", "the-kid"]
       in buildTest testDescription file tagsAdded,
      let file = "blood-meridian_cormac-mccarthy.epub"
          testDescription = "add zero tags to Blood Meridian"
          originalTags = safeGetTags testDB file
       in testCase testDescription $
            do
              mockInputHandle <- prepareMockHandle $ buildInputFromTagsToAdd []
              discardHandle <- getNullHandle
              newDB <- runAddTags mockInputHandle discardHandle testDirectory file testDB
              originalTags @?= safeGetTags newDB file,
      let file = "the-scarlett-letter_nathaniel-hawthorne.epub"
          mockInput =
            [ "y",
              "The Scarlett Letter",
              "Nathaniel",
              "Hawthorne",
              "",
              "shame",
              "american",
              "novel",
              "english",
              ""
            ]
          testDescription = "add nonexistent book The Scarlett Letter"
          newEntry =
            Book
              "the-scarlett-letter_nathaniel-hawthorne.epub"
              "The Scarlett Letter"
              (Set.singleton $ Author (Just "Nathaniel") "Hawthorne")
              (Set.fromList ["shame", "american", "novel", "english"])
       in testCase testDescription $ do
            mockInputHandle <- prepareMockHandle mockInput
            discardHandle <- getNullHandle
            newDB <- runAddTags mockInputHandle discardHandle testDirectory file testDB
            assertBool "failed to add entry" $ newEntry `elem` newDB
    ]
  where
    buildInputFromTagsToAdd :: [Tag] -> [String]
    buildInputFromTagsToAdd [] = [""]
    buildInputFromTagsToAdd (t : ts) = t : buildInputFromTagsToAdd ts

    buildTest :: TestName -> FilePath -> [Tag] -> TestTree
    buildTest testDescription file tagsToAdd = testCase testDescription $
      do
        mockInputHandle <- prepareMockHandle $ buildInputFromTagsToAdd tagsToAdd
        discardHandle <- getNullHandle
        newDB <- runAddTags mockInputHandle discardHandle testDirectory file testDB
        let errorMsg =
              unlines
                [ "failed to add new tags: " <> show tagsToAdd,
                  "current tags: " <> (show . Set.toAscList . safeGetTags newDB) file
                ]
         in assertBool errorMsg (bookHasTags newDB file $ Set.fromList tagsToAdd)

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
      mockInputHandle <- prepareMockHandle $ getInputFromTagsToRemove tagsToRemove (safeGetTags testDB file)
      discardHandle <- getNullHandle
      newDB <- runRemoveTags mockInputHandle discardHandle file testDB
      let oldTags = safeGetTags testDB file
          newTags = safeGetTags newDB file
          errorMsg =
            unlines
              [ "failed to remove tags:" <> show tagsToRemove,
                "old tags:" <> show (Set.toAscList oldTags),
                "new tags:" <> show (Set.toAscList newTags)
              ]
       in assertBool errorMsg $ Set.difference oldTags (Set.fromList tagsToRemove) == newTags

-- Errors should also be tested.

{-
Mit csinál az `import'?
- Megnézi, hogy vannak-e az aktuális directory-ban olyan file-ok, amikhez nem tartozik adatbázisbejegyzés
- Mindegyik ilyen file-lal kapcsolatban megkérdezi, hogy hozzá akarom-e adni az adatbázishoz
- Ha igen, akkor létrehoz neki egy bejegyzést
- Ha nem, akkor megy tovább a következő bejegyzésre
- Ha s, akkor leáll és nem kérdezi meg a maradékot

Milyen tesztek adódnak ebből?
- Nincs adatbázisbejegyzés nélküli file
- Több is van, az elsőt nem importálom, a másodikat igen, a maradékot nem.

És akkor úgy tűnik, hogy ezen a ponton már szükség lesz arra, hogy egy megadott mappában futtassuk a függvényeket.
-}
testImport :: TestTree
testImport = testGroup "test `import' command" []

testClean :: TestTree
testClean = testGroup "test `clean' command" []
