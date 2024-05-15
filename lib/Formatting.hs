module Formatting
  ( printBooks,
    printMetaData,
  )
where

import Data.List (sortOn)
import Data.Set
import DataBase

printBooks :: [Book] -> IO ()
printBooks books = putStrLn $ unlines (printMetaData <$> sortOn author books)

printMetaData :: Book -> String
printMetaData (Book _ thisTitle thisAuthor theseTags) =
  thisTitle <> " - " <> printAllAuthors (toAscList thisAuthor) <> " (" <> printTags (toAscList theseTags) <> ")"

printAuthor :: Author -> String
printAuthor (Author Nothing thisLastName) = thisLastName
printAuthor (Author (Just thisFirstName) thisLastName) = thisFirstName <> " " <> thisLastName

printAllAuthors :: [Author] -> String
printAllAuthors [] = ""
printAllAuthors [a] = printAuthor a
printAllAuthors (a : as) = printAuthor a <> ", " <> printAllAuthors as

printTags :: [Tag] -> String
printTags [] = ""
printTags [tag] = tag
printTags (t : ts) = t <> ", " <> printTags ts
