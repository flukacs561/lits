module Formatting where

import Data.List (sortBy)
import Types

printBooks :: Maybe [Book] -> IO ()
printBooks Nothing = putStrLn ""
printBooks (Just books) = putStrLn $ unlines (printMetaData <$> sortByAuthorLastname books)

sortByAuthorLastname :: [Book] -> [Book]
sortByAuthorLastname = sortBy compareAuthorLastName

compareAuthorLastName :: Book -> Book -> Ordering
compareAuthorLastName book1 book2 = compare author1 author2
  where
    author1 = maybe "" lastName $ safeHead $ author book1
    author2 = maybe "" lastName $ safeHead $ author book2

printMetaData :: Book -> String
printMetaData (Book _ thisTitle thisAuthor _) = thisTitle ++ " - " ++ printAllAuthors thisAuthor

printAuthor :: Author -> String
printAuthor (Author Nothing thisLastName) = thisLastName
printAuthor (Author (Just thisFirstName) thisLastName) = thisFirstName ++ " " ++ thisLastName

printAllAuthors :: [Author] -> String
printAllAuthors [] = ""
printAllAuthors [a] = printAuthor a
printAllAuthors (a : as) = printAuthor a ++ ", " ++ printAllAuthors as
