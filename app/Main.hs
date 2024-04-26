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
printAllAuthors [a] = printAuthor a
printAllAuthors (a : as) = printAuthor a ++ ", " ++ printAllAuthors as

doList :: [String] -> Maybe [Book] -> IO ()
doList [] = printBooks
doList _ = error "Too many arguments."

doFilter :: [String] -> Maybe [Book] -> IO ()
doFilter args db = printBooks $ runFilterCmd args <$> db

doAdd :: [String] -> Maybe [Book] -> IO ()
doAdd args db = do
  newBook <- prepareNewEntry args
  case fmap (encode . (newBook :)) db of
    Nothing -> dataBaseError
    (Just newJSON) -> B.writeFile dataBaseFileName newJSON

doDelete :: [String] -> Maybe [Book] -> IO ()
doDelete args db = case encode <$> removeEntry args db of
  Nothing -> dataBaseError
  (Just newJSON) -> B.writeFile dataBaseFileName newJSON

main :: IO ()
main = do
  args <- getArgs
  dataBase <- validateDBFile
  case safeHead args of
    Nothing -> putStrLn "No argument was provided."
    (Just "init") -> undefined
    (Just "list") -> doList (safeTail args) dataBase
    (Just "check") -> undefined
    (Just "filter") -> doFilter (safeTail args) dataBase
    (Just "add") -> doAdd (safeTail args) dataBase
    (Just "add-tag") -> undefined
    (Just "delete") -> doDelete (safeTail args) dataBase
    (Just cmd) -> putStrLn $ throwError cmd

testFilter :: IO ()
testFilter = do
  dataBase <- validateDBFile
  printBooks $ runFilterCmd ["-t", "novel", "english", "-a", "John"] <$> dataBase
