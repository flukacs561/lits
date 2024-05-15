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
    doAddTags,
    doRemoveTags,
  )
where

import Data.List (nub)
import DataBase (Book, fileName, writeToDataBase)
import EntryManager (prepareNewEntry, removeEntry, runAddTags, runCleanCommand, runImportCommand, runRemoveTags)
import FileManager (createDBFile, removeFile)
import Filter (runFilterCmd)
import Formatting (printBooks)
import GHC.IO.StdHandles (stdin, stdout)

inputErrorTooMany :: a
inputErrorTooMany = error "Too many arguments"

inputErrorNoFile :: a
inputErrorNoFile = error "No file specified"

invalidCommandError :: String -> String
invalidCommandError cmd = "Invalid argument: \"" <> cmd <> "\" is not an actual command in LiTS."

doInit :: [String] -> IO ()
doInit [] = createDBFile
doInit _ = inputErrorTooMany

doList :: [String] -> [Book] -> IO ()
doList [] = printBooks
doList _ = inputErrorTooMany

-- Input validation happend in `parseFilterInput`.
doFilter :: [String] -> [Book] -> IO ()
doFilter args db = printBooks $ runFilterCmd args db

doAdd :: [String] -> [Book] -> IO ()
doAdd [] _ = inputErrorNoFile
doAdd [file] db =
  if hasFileEntry file db
    then error "This file already has an entry in the database."
    else do
      newBook <- prepareNewEntry stdin stdout file
      writeToDataBase (newBook : db)
doAdd _ _ = inputErrorTooMany

hasFileEntry :: FilePath -> [Book] -> Bool
hasFileEntry file = any (\book -> fileName book == file)

doDelete :: [String] -> [Book] -> IO ()
doDelete [] _ = inputErrorNoFile
doDelete [file] db = do
  writeToDataBase $ removeEntry file db
  removeFile file
doDelete _ _ = inputErrorTooMany

doImport :: [String] -> [Book] -> IO ()
doImport [] db = do
  newDB <- runImportCommand stdin stdout db
  writeToDataBase newDB
doImport _ _ = inputErrorTooMany

doRemoveDups :: [String] -> [Book] -> IO ()
doRemoveDups [] = writeToDataBase . nub
doRemoveDups _ = inputErrorTooMany

doClean :: [String] -> [Book] -> IO ()
doClean [] db = do
  newDB <- runCleanCommand stdin stdout db
  writeToDataBase newDB
doClean _ _ = inputErrorTooMany

doAddTags :: [FilePath] -> [Book] -> IO ()
doAddTags [] _ = inputErrorNoFile
doAddTags [file] db = do
  newDB <- runAddTags stdin stdout file db
  writeToDataBase newDB
doAddTags _ _ = inputErrorTooMany

doRemoveTags :: [FilePath] -> [Book] -> IO ()
doRemoveTags [] _ = inputErrorNoFile
doRemoveTags [file] db = do
  newDB <- runRemoveTags stdin stdout file db
  writeToDataBase newDB
doRemoveTags _ _ = inputErrorTooMany
