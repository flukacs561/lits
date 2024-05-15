module Formatting
  ( printBooks,
    printMetaData,
  )
where

import Data.List (sortBy)
import Data.Set
import DataBase
import Utilities

printBooks :: [Book] -> IO ()
printBooks books = putStrLn $ unlines (printMetaData <$> sortByAuthorLastname books)

sortByAuthorLastname :: [Book] -> [Book]
sortByAuthorLastname = sortBy compareAuthorLastName

compareAuthorLastName :: Book -> Book -> Ordering
compareAuthorLastName book1 book2 = compare author1 author2
  where
    author1 = maybe "" lastName $ safeHead $ author book1
    author2 = maybe "" lastName $ safeHead $ author book2

printMetaData :: Book -> String
printMetaData (Book _ thisTitle thisAuthor theseTags) = thisTitle ++ " - " ++ printAllAuthors thisAuthor ++ " (" ++ printTags (toAscList theseTags) ++ ")"

printAuthor :: Author -> String
printAuthor (Author Nothing thisLastName) = thisLastName
printAuthor (Author (Just thisFirstName) thisLastName) = thisFirstName ++ " " ++ thisLastName

printAllAuthors :: [Author] -> String
printAllAuthors [] = ""
printAllAuthors [a] = printAuthor a
printAllAuthors (a : as) = printAuthor a ++ ", " ++ printAllAuthors as

printTags :: [Tag] -> String
printTags [] = ""
printTags [tag] = tag
printTags (t : ts) = t ++ ", " ++ printTags ts
