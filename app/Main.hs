module Main where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import DataBase
import Filter
import System.Environment
import Types

throwError :: String -> String
throwError cmd = "Invalid argument: \"" ++ cmd ++ "\" is not an actual command in LiTS."

dataBaseError :: a
dataBaseError = error "An error occured when reading the database."

printBooks :: Maybe [Book] -> IO ()
printBooks Nothing = putStrLn ""
printBooks (Just books) = putStrLn $ unlines $ fmap printMetaData books

printMetaData :: Book -> String
printMetaData (Book _ title author _) = title ++ " - " ++ printAllAuthors author

printAuthor :: Author -> String
printAuthor (Author Nothing lastName) = lastName
printAuthor (Author (Just firstName) lastName) = firstName ++ " " ++ lastName

printAllAuthors :: [Author] -> String
printAllAuthors [] = ""
printAllAuthors (a : as) = printAuthor a ++ ", " ++ printAllAuthors as

removeEntry :: [String] -> Maybe [Book] -> Maybe [Book]
removeEntry [] _ = error "No file specified."
removeEntry [file] db = fmap (filter (\book -> fileName book == file)) db
removeEntry _ _ = error "Too many arguments."

main :: IO ()
main = do
  args <- getArgs
  dataBase <- validateDBFile
  case safeHead args of
    Nothing -> putStrLn "No argument was provided."
    (Just "init") -> undefined
    (Just "list") -> printBooks dataBase
    (Just "filter") -> printBooks $ runFilterCmd (tail args) <$> dataBase
    (Just "add") -> do
      newBook <- prepareNewEntry (safeTail args)
      case fmap (encode . (newBook :)) dataBase of
        Nothing -> dataBaseError
        (Just newJSON) -> B.writeFile dataBaseFileName newJSON
    (Just "delete") -> case encode <$> removeEntry (safeTail args) dataBase of
      Nothing -> dataBaseError
      (Just newJSON) -> B.writeFile dataBaseFileName newJSON
    (Just cmd) -> putStrLn $ throwError cmd

testFilter :: IO ()
testFilter = do
  dataBase <- validateDBFile
  printBooks $ runFilterCmd ["-t", "novel", "english", "-a", "John"] <$> dataBase
