module EntryManager (prepareNewEntry, removeEntry) where

import DataBase
import Data.Char (isAlphaNum)
import FileManager
import System.FilePath

-- The argument should contain exactly one string: the name of the file for which the entry is to be generated.
prepareNewEntry :: String -> IO Book
prepareNewEntry path = do
  isFilePresent <- isFileInWorkingDirectory path
  if isFilePresent
    then do
      putStrLn "Title: "
      thisTitle <- getLine
      putStrLn "Author(s):"
      thisAuthor <- getAuthor
      Book (takeFileName path) thisTitle thisAuthor <$> getTags
    else error "No such file in working directory."

-- There might be more than one author for a single book, and we want to note all of them.
getAuthor :: IO [Author]
getAuthor = run []
  where
    run authorList = do
      newAuthor <- getOneAuthor
      putStrLn "Are there any more authors? [y/N] "
      moreAuthors <- getLine
      if moreAuthors == "y" then run (newAuthor : authorList) else return $ newAuthor : authorList

getOneAuthor :: IO Author
getOneAuthor = do
  putStrLn "First name: "
  thisFirstName <- getLine
  putStrLn "Last Name: "
  Author (if thisFirstName == "" then Nothing else Just thisFirstName) <$> getLine

data TagValidationResult = Valid | Invalid | Empty

-- Tags should be nonempty and contain only alphanumeric characters.
validateTag :: Tag -> TagValidationResult
validateTag "" = Empty
validateTag tag = if all isAlphaNum tag then Valid else Invalid

-- Tags can be inputed until an empty string is submitted.
getTags :: IO [Tag]
getTags = run []
  where
    run tagList = do
      putStrLn "Next tag: "
      newTag <- getLine
      case validateTag newTag of
        Valid -> run (newTag : tagList)
        Invalid -> do
          putStrLn "Invalid tag"
          run tagList
        Empty -> return tagList

removeEntry :: String -> Maybe [Book] -> Maybe [Book]
removeEntry file = fmap (filter (\book -> fileName book /= file))
