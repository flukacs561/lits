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

doInit :: FilePath -> [String] -> IO ()
doInit workingDirectory [] = createDBFile workingDirectory
doInit _ _ = inputErrorTooMany

doList :: [String] -> [Book] -> IO ()
doList [] = printBooks
doList _ = inputErrorTooMany

-- Input validation happend in `parseFilterInput`.
doFilter :: [String] -> [Book] -> IO ()
doFilter args db = printBooks $ runFilterCmd args db

doAdd :: FilePath -> [String] -> [Book] -> IO ()
doAdd _directory [] _db = inputErrorNoFile
doAdd directory [file] db =
  if hasFileEntry file db
    then error "This file already has an entry in the database."
    else do
      newBook <- prepareNewEntry stdin stdout directory file
      writeToDataBase directory (newBook : db)
doAdd _ _ _ = inputErrorTooMany

hasFileEntry :: FilePath -> [Book] -> Bool
hasFileEntry file = any (\book -> fileName book == file)

doDelete :: FilePath -> [String] -> [Book] -> IO ()
doDelete _ [] _ = inputErrorNoFile
doDelete directory [file] db = do
  writeToDataBase directory $ removeEntry file db
  removeFile file
doDelete _ _ _ = inputErrorTooMany

doImport :: FilePath -> [String] -> [Book] -> IO ()
doImport directory [] db = do
  newDB <- runImportCommand stdin stdout directory db
  writeToDataBase directory newDB
doImport _ _ _ = inputErrorTooMany

doRemoveDups :: FilePath -> [String] -> [Book] -> IO ()
doRemoveDups directory [] = writeToDataBase directory . nub
doRemoveDups _ _ = inputErrorTooMany

doClean :: FilePath -> [String] -> [Book] -> IO ()
doClean directory [] db = do
  newDB <- runCleanCommand stdin stdout directory db
  writeToDataBase directory newDB
doClean _ _ _ = inputErrorTooMany

doAddTags :: FilePath -> [FilePath] -> [Book] -> IO ()
doAddTags _ [] _ = inputErrorNoFile
doAddTags directory [file] db = do
  newDB <- runAddTags stdin stdout directory file db
  writeToDataBase directory newDB
doAddTags _ _ _ = inputErrorTooMany

doRemoveTags :: FilePath -> [FilePath] -> [Book] -> IO ()
doRemoveTags _ [] _ = inputErrorNoFile
doRemoveTags directory [file] db = do
  newDB <- runRemoveTags stdin stdout file db
  writeToDataBase directory newDB
doRemoveTags _ _ _ = inputErrorTooMany
