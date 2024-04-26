module DataBase where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Char (isAlphaNum)
import System.Directory
import System.FilePath
import Types

dataBaseFileName :: FilePath
dataBaseFileName = "test-data.json"

isFileInWorkingDirectory :: String -> IO Bool
isFileInWorkingDirectory path = do
  files <- getDirectoryContents "."
  pure $ path `elem` files

findLitsDBFile :: IO Bool
findLitsDBFile = isFileInWorkingDirectory dataBaseFileName

dataBaseFile :: IO B.ByteString
dataBaseFile = B.readFile dataBaseFileName

validateDBFile :: IO (Maybe [Book])
validateDBFile = do
  isDBFilePresent <- findLitsDBFile
  if isDBFilePresent
    then fmap decode dataBaseFile
    else pure Nothing

prepareNewEntry :: [String] -> IO Book
prepareNewEntry [] = error "No file specified."
prepareNewEntry [path] = do
  isFilePresent <- isFileInWorkingDirectory path
  if isFilePresent
    then do
      putStrLn "Title: "
      thisTitle <- getLine
      putStrLn "Author(s):"
      thisAuthor <- getAuthor
      Book (takeFileName path) thisTitle thisAuthor <$> getTags
    else error "No such file in working directory."
prepareNewEntry _ = error "Too many arguments."

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

validateTag :: Tag -> TagValidationResult
validateTag "" = Empty
validateTag tag = if all isAlphaNum tag then Valid else Invalid

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

removeEntry :: [String] -> Maybe [Book] -> Maybe [Book]
removeEntry [] _ = error "No file specified."
removeEntry [file] db = fmap (filter (\book -> fileName book == file)) db
removeEntry _ _ = error "Too many arguments."
