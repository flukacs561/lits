module EntryManager
  ( prepareNewEntry,
    removeEntry,
    addUnsavedBookDialog,
    runImportCommand,
    runCleanCommand,
    runAddTags,
    runRemoveTags,
  )
where

import Data.Char (isAlphaNum)
import Data.List (nub, (\\))
import DataBase
import FileManager
import Formatting
import System.FilePath
import Text.Read (readMaybe)
import Utilities ((|||))
import GHC.IO.Handle

inputErrorFileNotFound :: a
inputErrorFileNotFound = error "No such file in current directory."

-- The argument should contain exactly one string: the name of the file for which the entry is to be generated.
prepareNewEntry ::  Handle -> String -> IO Book
prepareNewEntry input file = do
  isFilePresent <- isFileInWorkingDirectory file
  if isFilePresent
    then do
      putStrLn "Title: "
      thisTitle <- hGetLine input
      putStrLn "Author(s):"
      thisAuthor <- getAuthor input
      Book (takeFileName file) thisTitle thisAuthor <$> getTags input
    else inputErrorFileNotFound

-- There might be more than one author for a single book, and we want to note all of them.
getAuthor :: Handle -> IO [Author]
getAuthor input  = run []
  where
    run authorList = do
      newAuthor <- getOneAuthor input
      putStrLn "Are there any more authors? [y/N] "
      moreAuthors <- hGetLine input
      if moreAuthors == "y" then run (newAuthor : authorList) else return $ newAuthor : authorList

getOneAuthor :: Handle -> IO Author
getOneAuthor input = do
  putStrLn "First name: "
  thisFirstName <- hGetLine input
  putStrLn "Last Name: "
  Author (if thisFirstName == "" then Nothing else Just thisFirstName) <$> hGetLine input

data TagValidationResult = ValidTag | InvalidTag | EmptyTag

-- Tags should be nonempty and contain only alphanumeric characters.
validateTag :: Tag -> TagValidationResult
validateTag "" = EmptyTag
validateTag tag = if all (isAlphaNum ||| (== '-')) tag then ValidTag else InvalidTag

-- Tags can be inputed until an empty string is submitted.
getTags :: Handle -> IO [Tag]
getTags input = run []
  where
    run tagList = do
      putStrLn "Next tag: "
      newTag <- hGetLine input
      case validateTag newTag of
        ValidTag -> run (newTag : tagList)
        InvalidTag -> do
          putStrLn "Invalid tag"
          run tagList
        EmptyTag -> return tagList

removeEntry :: FilePath -> [Book] -> [Book]
removeEntry file = filter (\book -> fileName book /= file)

runImportCommand :: Handle -> [Book] -> IO [Book]
runImportCommand input db = do
  booksInDirectory <- getBookFilesFromDirectory
  booksToAdd <- addUnsavedBookDialog input $ getUnsavedBooks booksInDirectory db
  return $ booksToAdd ++ db

addUnsavedBookDialog :: Handle -> [FilePath] -> IO [Book]
addUnsavedBookDialog _ [] = do
  putStrLn "No more unsaved files."
  return []
addUnsavedBookDialog input (file : rest) = do
  putStrLn $ "Do you want to create a database entry for " ++ file ++ "? [y/N/s/d/?]"
  wantToSave <- hGetLine input
  case wantToSave of
    "y" -> myCombine (prepareNewEntry input file) $ addUnsavedBookDialog input rest
    "n" -> addUnsavedBookDialog input rest
    "s" -> return []
    "d" -> do
      removeFile file
      addUnsavedBookDialog input rest
    "?" -> do
      putStrLn $
        unlines
          [ "y - yes",
            "n - no",
            "s - stop (say no to all consequent)",
            "d - delete file",
            "? - print this help"
          ]
      addUnsavedBookDialog input rest
    _ -> addUnsavedBookDialog input rest

myCombine :: IO a -> IO [a] -> IO [a]
myCombine ioElement ioList = do
  element <- ioElement
  list <- ioList
  return $ element : list

getUnsavedBooks :: [FilePath] -> [Book] -> [FilePath]
getUnsavedBooks files books = filter (not . hasEntry books) files

hasEntry :: [Book] -> FilePath -> Bool
hasEntry books file = any (\book -> file == fileName book) books

runCleanCommand :: Handle -> [Book] -> IO [Book]
runCleanCommand input db = do
  booksInDirectory <- getBookFilesFromDirectory
  entriesToRemove <- removeOrphanEntryDialog input (getOrphanEntries booksInDirectory db) db
  return $ db \\ entriesToRemove

removeOrphanEntryDialog :: Handle -> [Book] -> [Book] -> IO [Book]
removeOrphanEntryDialog _ [] _ = do
  putStrLn "No orphan database entries."
  return []
removeOrphanEntryDialog input (book : rest) db = do
  putStrLn "The following entry doesn't have an actual file associated to it in the current directory . Do you want to remove it? [y/N/s/a/?]"
  putStrLn $ printMetaData book
  wantToRemove <- hGetLine input
  case wantToRemove of
    "y" -> (book :) <$> removeOrphanEntryDialog input rest db
    "n" -> removeOrphanEntryDialog input rest db
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
      removeOrphanEntryDialog input (book : rest) db
    _ -> removeOrphanEntryDialog input rest db

getOrphanEntries :: [FilePath] -> [Book] -> [Book]
getOrphanEntries files = filter (not . hasFile files)

hasFile :: [FilePath] -> Book -> Bool
hasFile files book = any (\file -> fileName book == file) files

runAddTags :: Handle -> FilePath -> [Book] -> IO [Book]
runAddTags input file [] = do
  putStrLn "This file does not have a corresponding database entry. Do you wish to create one? [y/N]"
  wantToCreate <- hGetLine input
  if wantToCreate == "y"
    then pure <$> prepareNewEntry input file
    else return []
runAddTags input file (b@(Book thisFileName thisTitle thisAuthor theseTags) : bs) =
  if file == thisFileName
    then do
      newTags <- getTags input
      return $ Book thisFileName thisTitle thisAuthor (nub $ newTags ++ theseTags) : bs
    else (b :) <$> runAddTags input file bs

runRemoveTags :: Handle -> FilePath -> [Book] -> IO [Book]
runRemoveTags _input _file [] = error "This file does not have a corresponding database entry, hence no tag can be removed."
runRemoveTags input file (b@(Book thisFileName thisTitle thisAuthor theseTags) : bs) =
  if file == thisFileName
    then do
      tagsToRemove <- getTagsToRemove input theseTags
      return $ Book thisFileName thisTitle thisAuthor (theseTags \\ tagsToRemove) : bs
    else (b :) <$> runRemoveTags input file bs

getTagsToRemove :: Handle -> [Tag] -> IO [Tag]
getTagsToRemove input allTags = do
  putStrLn "Enter the number of the tag you wish to remove. If you do not wish to remove any (more) of the tags, hit Enter."
  putStrLn $ unlines [show i ++ ") " ++ tag | (i, tag) <- zip [1 :: Int ..] allTags]
  numberOfTag <- hGetLine input
  case isValidInteger (length allTags) numberOfTag of
    EmptyInput -> return []
    ValidInput n -> let tagToRemove = allTags !! (n - 1) in (tagToRemove :) <$> getTagsToRemove input (allTags \\ [tagToRemove])
    InvalidInput -> do
      putStrLn $ "Invalid input. Please enter an integer between 1 and " ++ show (length allTags) ++ "!"
      getTagsToRemove input allTags

data InputValidation a = ValidInput a | EmptyInput | InvalidInput

isValidInteger :: Int -> String -> InputValidation Int
isValidInteger _ "" = EmptyInput
isValidInteger upperLimit x = case readMaybe x :: Maybe Int of
  Just n -> if n > 0 && n <= upperLimit then ValidInput n else InvalidInput
  Nothing -> InvalidInput
