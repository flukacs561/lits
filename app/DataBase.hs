module DataBase where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import System.Directory
import Types

dataBaseFileName :: FilePath
dataBaseFileName = "test-data.json"

findLitsDBFile :: IO Bool
findLitsDBFile = do
  files <- getDirectoryContents "."
  pure $ dataBaseFileName `elem` files

dataBaseFile :: IO B.ByteString
dataBaseFile = B.readFile dataBaseFileName

validateDBFile :: IO (Maybe [Book])
validateDBFile = do
  isDBFilePresent <- findLitsDBFile
  if isDBFilePresent 
    then fmap decode dataBaseFile
    else pure Nothing

printMetaData :: Book -> String
printMetaData (Book _ title author _) = title ++ " - " ++ printAllAuthors author

printAuthor :: Author -> String
printAuthor (Author Nothing lastName) = lastName
printAuthor (Author (Just firstName) lastName) = firstName ++ " " ++ lastName

printAllAuthors :: [Author] -> String
printAllAuthors [] = ""
printAllAuthors (a:as) = printAuthor a ++ ", " ++ printAllAuthors as
