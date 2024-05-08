module EntryManager
  ( prepareNewEntry,
    removeEntry,
    addUnsavedBookDialog,
    runImportCommand,
    runCleanCommand,
  )
where

import Data.Char (isAlphaNum)
import Data.List ((\\))
import DataBase
import FileManager
import System.FilePath
import Utilities ((|||))
import Formatting

inputErrorFileNotFound :: a
inputErrorFileNotFound = error "No such file in current directory."

-- The argument should contain exactly one string: the name of the file for which the entry is to be generated.
prepareNewEntry :: String -> IO Book
prepareNewEntry file = do
  isFilePresent <- isFileInWorkingDirectory file
  if isFilePresent
    then do
      putStrLn "Title: "
      thisTitle <- getLine
      putStrLn "Author(s):"
      thisAuthor <- getAuthor
      Book (takeFileName file) thisTitle thisAuthor <$> getTags
    else inputErrorFileNotFound

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

data TagValidationResult = ValidTag | InvalidTag | EmptyTag

-- Tags should be nonempty and contain only alphanumeric characters.
validateTag :: Tag -> TagValidationResult
validateTag "" = EmptyTag
validateTag tag = if all (isAlphaNum ||| (== '-')) tag then ValidTag else InvalidTag

-- Tags can be inputed until an empty string is submitted.
getTags :: IO [Tag]
getTags = run []
  where
    run tagList = do
      putStrLn "Next tag: "
      newTag <- getLine
      case validateTag newTag of
        ValidTag -> run (newTag : tagList)
        InvalidTag -> do
          putStrLn "Invalid tag"
          run tagList
        EmptyTag -> return tagList

removeEntry :: FilePath -> [Book] -> [Book]
removeEntry file = filter (\book -> fileName book /= file)

runImportCommand :: [Book] -> IO [Book]
runImportCommand db = do
  booksInDirectory <- getBookFilesFromDirectory
  booksToAdd <- addUnsavedBookDialog $ getUnsavedBooks booksInDirectory db
  return $ booksToAdd ++ db

addUnsavedBookDialog :: [FilePath] -> IO [Book]
addUnsavedBookDialog [] = do
  putStrLn "No more unsaved files."
  return []
addUnsavedBookDialog (file : rest) = do
  putStrLn $ "Do you want to create a database entry for " ++ file ++ "? [y/N/s/d/?]"
  wantToSave <- getLine
  case wantToSave of
    "y" -> myCombine (prepareNewEntry file) $ addUnsavedBookDialog rest
    "n" -> addUnsavedBookDialog rest
    "s" -> return []
    "d" -> do
      removeFile file
      addUnsavedBookDialog rest
    "?" -> do
      putStrLn $
        unlines
          [ "y - yes",
            "n - no",
            "s - stop (say no to all consequent)",
            "d - delete file",
            "? - print this help"
          ]
      addUnsavedBookDialog rest
    _ -> addUnsavedBookDialog rest

myCombine :: IO a -> IO [a] -> IO [a]
myCombine ioElement ioList = do
  element <- ioElement
  list <- ioList
  return $ element : list

getUnsavedBooks :: [FilePath] -> [Book] -> [FilePath]
getUnsavedBooks files books = filter (not . hasEntry books) files

hasEntry :: [Book] -> FilePath -> Bool
hasEntry books file = any (\book -> file == fileName book) books

runCleanCommand :: [Book] -> IO [Book]
runCleanCommand db = do
  booksInDirectory <- getBookFilesFromDirectory
  entriesToRemove <- removeOrphanEntryDialog (getOrphanEntries booksInDirectory db) db
  return $ db \\ entriesToRemove

removeOrphanEntryDialog :: [Book] -> [Book] -> IO [Book]
removeOrphanEntryDialog [] _ = do
  putStrLn "No orphan database entries."
  return []
removeOrphanEntryDialog (book : rest) db = do
  putStrLn "The following entry doesn't have an actual file associated to it in the current directory . Do you want to remove it? [y/N/s/a/?]"
  putStrLn $ printMetaData book
  wantToRemove <- getLine
  case wantToRemove of
    "y" -> myCombine (pure book) $ removeOrphanEntryDialog rest db
    "n" -> removeOrphanEntryDialog rest db
    "s" -> return []
    "a" -> return (book : rest)
    "?" -> do
      putStrLn $
        unlines
          [ "y - yes",
            "n - no",
            "s - stop (say no to all consequent)",
            "a - auto (say yes to all consequent)",
            "? - print this help"
          ]
      removeOrphanEntryDialog rest db
    _ -> removeOrphanEntryDialog rest db

getOrphanEntries :: [FilePath] -> [Book] -> [Book]
getOrphanEntries files = filter (not . hasFile files)

hasFile :: [FilePath] -> Book -> Bool
hasFile files book = any (\file -> fileName book == file) files
