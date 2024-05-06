module Commands
  ( invalidCommandError,
    doList,
    doFilter,
    doAdd,
    doDelete,
    doInit,
  )
where

import DataBase (Book, writeToDataBase)
import EntryManager (prepareNewEntry, removeEntry)
import FileManager (createDBFile, deleteFile)
import Filter (runFilterCmd)
import Formatting (printBooks)

invalidCommandError :: String -> String
invalidCommandError cmd = "Invalid argument: \"" ++ cmd ++ "\" is not an actual command in LiTS."

doInit :: [String] -> IO ()
doInit [] = createDBFile
doInit _ = error "Too many arguments."

doList :: [String] -> Maybe [Book] -> IO ()
doList [] = printBooks
doList _ = error "Too many arguments."

doFilter :: [String] -> Maybe [Book] -> IO ()
doFilter args db = printBooks $ runFilterCmd args <$> db

doAdd :: [String] -> Maybe [Book] -> IO ()
doAdd args db = do
  newBook <- prepareNewEntry args
  writeToDataBase $ fmap (newBook :) db

doDelete :: [String] -> Maybe [Book] -> IO ()
doDelete args db = do
  writeToDataBase $ removeEntry args db
  deleteFile args
