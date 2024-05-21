module TestUtils where

import Data.List (delete)
import qualified Data.Set as Set
import LiTS.DataBase
import System.IO
import System.Process (createPipe)
import Test.Tasty.HUnit

{- The test database consists of the following entries:
Little Women - Louise May Alcott (american, english, novel, women)
The Last of the Mohicans - James Fenimore Cooper (american, english, indian, novel)
Absalom, Absalom! - William Faulkner (american, english, gothic, novel, southern)
The Great Gatsby - F. Scott Fitzgerald (american, americandream, english, novel)
The Scarlett Letter - Nathaniel Hawthorne (american, english, novel, shame)
Blood Meridian - Cormac McCarthy (american, english, novel, southern, western)
Moby-Dick - Herman Melville (american, english, ishmael, novel, whale)
The Grapes of Wrath - John Steinbeck (american, california, english, famine, novel)
Uncle Tom's Cabin - Harriet Beecher Stowe (american, blacks, english, novel, slaves)
Adventures of Huckleberry Finn - Mark Twain (adventure, american, english, novel) -}
testDB :: [Book]
testDB =
  [ Book
      { fileName = "blood-meridian_cormac-mccarthy.epub",
        title = "Blood Meridian",
        author = Set.singleton (Author {firstName = Just "Cormac", lastName = "McCarthy"}),
        tags = Set.fromList ["english", "novel", "american", "western", "southern"]
      },
    Book
      { fileName = "the-grapes-of-wrath_john-steinbeck.epub",
        title = "The Grapes of Wrath",
        author = Set.singleton (Author {firstName = Just "John", lastName = "Steinbeck"}),
        tags = Set.fromList ["famine", "california", "english", "novel", "american"]
      },
    Book
      { fileName = "absalom-absalom_william-faulkner.epub",
        title = "Absalom, Absalom!",
        author = Set.singleton (Author {firstName = Just "William", lastName = "Faulkner"}),
        tags = Set.fromList ["gothic", "southern", "novel", "english", "american"]
      },
    Book
      { fileName = "little-women_louisa-may-alcott.epub",
        title = "Little Women",
        author = Set.singleton (Author {firstName = Just "Louise May", lastName = "Alcott"}),
        tags = Set.fromList ["women", "novel", "english", "american"]
      },
    Book
      { fileName = "uncle-toms-cabin_harriet-beecher-stowe.epub",
        title = "Uncle Tom's Cabin",
        author = Set.singleton (Author {firstName = Just "Harriet Beecher", lastName = "Stowe"}),
        tags = Set.fromList ["slaves", "blacks", "american", "novel", "english"]
      },
    Book
      { fileName = "moby-dick_herman-melville.epub",
        title = "Moby-Dick",
        author = Set.singleton (Author {firstName = Just "Herman", lastName = "Melville"}),
        tags = Set.fromList ["ishmael", "whale", "novel", "american", "english"]
      },
    Book
      { fileName = "the-last-of-the-mohicans_j-f-cooper.epub",
        title = "The Last of the Mohicans",
        author = Set.singleton (Author {firstName = Just "James Fenimore", lastName = "Cooper"}),
        tags = Set.fromList ["novel", "american", "english", "indian"]
      },
    Book
      { fileName = "iliad_homer.epub",
        title = "Iliad",
        author = Set.singleton (Author {firstName = Nothing, lastName = "Homer"}),
        tags = Set.fromList ["epic", "ancient", "greek"]
      },
    Book
      { fileName = "sicp.pdf",
        title = "Structure and Interpretation of Computer Programs",
        author =
          Set.fromList
            [ Author {firstName = Just "Harold", lastName = "Abelson"},
              Author {firstName = Just "Gerald Jay", lastName = "Sussman"}
            ],
        tags = Set.fromList ["cs", "wizzard", "lisp", "scheme", "english"]
      }
  ]

bookInList :: String -> [Book] -> Bool
bookInList _target [] = False
bookInList target (b : bs) = (target == fileName b) || bookInList target bs

booksAllInList :: [String] -> [Book] -> Bool
booksAllInList target books = all (`bookInList` books) target

getNullHandle :: IO Handle
getNullHandle = openFile "/dev/null" WriteMode -- For Unix-like systems

getMockHandle :: FilePath -> IO Handle
getMockHandle file = openFile file ReadMode

getBookByFileName :: [Book] -> FilePath -> Maybe Book
getBookByFileName [] _ = Nothing
getBookByFileName (b : bs) file = if fileName b == file then Just b else getBookByFileName bs file

bookHasTags :: [Book] -> FilePath -> Set.Set Tag -> Bool
bookHasTags books file tagsToCheck = case getBookByFileName books file of
  Nothing -> False
  (Just b) -> tagsToCheck `Set.isSubsetOf` tags b

(@?>>=) :: (Eq a, Show a, HasCallStack) => IO a -> a -> Assertion
ioX @?>>= y = do
  x <- ioX
  x @?= y

prepareMockHandle :: [String] -> IO Handle
prepareMockHandle cmds = do
  (readHandle, writeHandle) <- createPipe
  mapM_ (hPutStrLn writeHandle) cmds
  hClose writeHandle
  return readHandle

convertBookToMockInstructions :: Book -> [String]
convertBookToMockInstructions (Book _thisFileName thisTitle theseAuthors theseTags) =
  [thisTitle]
    <> authorInstructions (Set.toAscList theseAuthors)
    <> Set.toAscList theseTags
    <> [""]
  where
    authorInstructions :: [Author] -> [String]
    authorInstructions [] = ["n"]
    authorInstructions (a : as) = singleAuthorInstructions a <> authorInstructions as

    singleAuthorInstructions :: Author -> [String]
    singleAuthorInstructions (Author Nothing thisLastName) = ["", thisLastName]
    singleAuthorInstructions (Author (Just thisFirstName) thisLastName) = [thisFirstName, thisLastName]

safeGetTags :: [Book] -> FilePath -> Set.Set Tag
safeGetTags db file = maybe Set.empty tags (getBookByFileName db file)

except :: [Book] -> FilePath -> [Book]
except db file = maybe db (`delete` db) (getBookByFileName db file)
