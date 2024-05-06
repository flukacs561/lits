module Commands
  ( invalidCommandError,
    doList,
    doFilter,
    doAdd,
    doDelete,
    doInit,
    doImport,
    doRemoveDups,
    doClean,
  )
where

import Data.List (nub)
import DataBase (Book, fileName, writeToDataBase)
import EntryManager (prepareNewEntry, removeEntry, runCleanCommand, runImportCommand)
import FileManager (createDBFile, removeFile)
import Filter (runFilterCmd)
import Formatting (printBooks)

invalidCommandError :: String -> String
invalidCommandError cmd = "Invalid argument: \"" ++ cmd ++ "\" is not an actual command in LiTS."

doInit :: [String] -> IO ()
doInit [] = createDBFile
doInit _ = error "Too many arguments."

doList :: [String] -> [Book] -> IO ()
doList [] = printBooks
doList _ = error "Too many arguments."

-- Input validation happend in `parseFilterInput`.
doFilter :: [String] -> [Book] -> IO ()
doFilter args db = printBooks $ runFilterCmd args db

doAdd :: [String] -> [Book] -> IO ()
doAdd [] _ = error "No file specified."
doAdd [file] db =
  if hasFileEntry file db
    then error "This file already has an entry in the database."
    else do
      newBook <- prepareNewEntry file
      writeToDataBase (newBook : db)
doAdd _ _ = error "Too many arguments."

hasFileEntry :: FilePath -> [Book] -> Bool
hasFileEntry file = any (\book -> fileName book == file)

doDelete :: [String] -> [Book] -> IO ()
doDelete [] _ = error "No file specified."
doDelete [file] db = do
  writeToDataBase $ removeEntry file db
  removeFile file
doDelete _ _ = error "Too many arguments."

doImport :: [String] -> [Book] -> IO ()
doImport [] db = do
  newDB <- runImportCommand db
  writeToDataBase newDB
doImport _ _ = error "Too many arguments."

doRemoveDups :: [String] -> [Book] -> IO ()
doRemoveDups [] = writeToDataBase . nub
doRemoveDups _ = error "Too many arguments."

doClean :: [String] -> [Book] -> IO ()
doClean [] db = do
  newDB <- runCleanCommand db
  writeToDataBase newDB
doClean _ _ = error "Too many arguments."
