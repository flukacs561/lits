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
import Data.List ((\\))
import qualified Data.Set as Set
import DataBase
import FileManager
import Formatting (printMetaData)
import System.FilePath (takeFileName)
import System.IO (Handle, hGetLine, hPutStrLn)
import Text.Read (readMaybe)
import Utilities ((|||), monadCons)

inputErrorFileNotFound :: a
inputErrorFileNotFound = error "No such file in current directory."

-- The argument should contain exactly one string: the name of the file for which the entry is to be generated.
prepareNewEntry :: Handle -> Handle -> FilePath -> String -> IO Book
prepareNewEntry input output directory file = do
  isFilePresent <- isFileInDirectory directory file
  if isFilePresent
    then do
      hPutStrLn output "Title: "
      thisTitle <- hGetLine input
      hPutStrLn output "Author(s):"
      thisAuthor <- getAuthor input output
      Book (takeFileName file) thisTitle thisAuthor <$> getTags input output
    else inputErrorFileNotFound

-- There might be more than one author for a single book, and we want to note all of them.
getAuthor :: Handle -> Handle -> IO (Set.Set Author)
getAuthor input output = run Set.empty
  where
    run authorList = do
      newAuthor <- getOneAuthor input output
      hPutStrLn output "Are there any more authors? [y/N] "
      moreAuthors <- hGetLine input
      if moreAuthors == "y" then run (Set.insert newAuthor authorList) else return $ Set.insert newAuthor authorList

getOneAuthor :: Handle -> Handle -> IO Author
getOneAuthor input output = do
  hPutStrLn output "First name: "
  thisFirstName <- hGetLine input
  hPutStrLn output "Last Name: "
  Author (if thisFirstName == "" then Nothing else Just thisFirstName) <$> hGetLine input

data TagValidationResult = ValidTag | InvalidTag | EmptyTag

-- Tags should be nonempty and contain only alphanumeric characters.
validateTag :: Tag -> TagValidationResult
validateTag "" = EmptyTag
validateTag tag = if all (isAlphaNum ||| (== '-')) tag then ValidTag else InvalidTag

-- Tags can be inputed until an empty string is submitted.
getTags :: Handle -> Handle -> IO (Set.Set Tag)
getTags input output = run Set.empty
  where
    run tagList = do
      hPutStrLn output "Next tag: "
      newTag <- hGetLine input
      case validateTag newTag of
        ValidTag -> run (Set.insert newTag tagList)
        InvalidTag -> do
          hPutStrLn output "Invalid tag"
          run tagList
        EmptyTag -> return tagList

removeEntry :: FilePath -> [Book] -> [Book]
removeEntry file = filter (\book -> fileName book /= file)

runImportCommand :: Handle -> Handle -> FilePath -> [Book] -> IO [Book]
runImportCommand input output directory db = do
  booksInDirectory <- getBookFilesFromDirectory directory
  booksToAdd <- addUnsavedBookDialog input output directory $ getUnsavedBooks booksInDirectory db
  return $ booksToAdd <> db

addUnsavedBookDialog :: Handle -> Handle -> FilePath -> [FilePath] -> IO [Book]
addUnsavedBookDialog _ output _ [] = do
  hPutStrLn output "No more unsaved files."
  return []
addUnsavedBookDialog input output directory (file : rest) = do
  hPutStrLn output $ "Do you want to create a database entry for " <> file <> "? [y/N/s/d/?]"
  wantToSave <- hGetLine input
  case wantToSave of
    "y" -> monadCons (prepareNewEntry input output directory file) $ addUnsavedBookDialog input output directory rest
    "n" -> addUnsavedBookDialog input output directory rest
    "s" -> return []
    "d" -> do
      removeFile file
      addUnsavedBookDialog input output directory rest
    "?" -> do
      hPutStrLn output $
        unlines
          [ "y - yes",
            "n - no",
            "s - stop (say no to all consequent)",
            "d - delete file",
            "? - print this help"
          ]
      addUnsavedBookDialog input output directory rest
    _ -> addUnsavedBookDialog input output directory rest

getUnsavedBooks :: [FilePath] -> [Book] -> [FilePath]
getUnsavedBooks files books = filter (not . hasEntry books) files

hasEntry :: [Book] -> FilePath -> Bool
hasEntry books file = any (\book -> file == fileName book) books

runCleanCommand :: Handle -> Handle -> FilePath -> [Book] -> IO [Book]
runCleanCommand input output directory db = do
  booksInDirectory <- getBookFilesFromDirectory directory
  entriesToRemove <- removeOrphanEntryDialog input output (getOrphanEntries booksInDirectory db) db
  return $ db \\ entriesToRemove

removeOrphanEntryDialog :: Handle -> Handle -> [Book] -> [Book] -> IO [Book]
removeOrphanEntryDialog _input output [] _ = do
  hPutStrLn output "No orphan database entries."
  return []
removeOrphanEntryDialog input output (book : rest) db = do
  hPutStrLn output "The following entry doesn't have an actual file associated to it in the current directory . Do you want to remove it? [y/N/s/a/?]"
  hPutStrLn output $ printMetaData book
  wantToRemove <- hGetLine input
  case wantToRemove of
    "y" -> (book :) <$> removeOrphanEntryDialog input output rest db
    "n" -> removeOrphanEntryDialog input output rest db
    "s" -> return []
    "a" -> return (book : rest)
    "?" -> do
      hPutStrLn output $
        unlines
          [ "y - yes",
            "n - no",
            "s - stop (say no to all consequent)",
            "a - auto (say yes to all consequent)",
            "? - print this help"
          ]
      removeOrphanEntryDialog input output (book : rest) db
    _ -> removeOrphanEntryDialog input output rest db

getOrphanEntries :: [FilePath] -> [Book] -> [Book]
getOrphanEntries files = filter (not . hasFile files)

hasFile :: [FilePath] -> Book -> Bool
hasFile files book = any (\file -> fileName book == file) files

runAddTags :: Handle -> Handle -> FilePath -> FilePath -> [Book] -> IO [Book]
runAddTags input output directory file [] = do
  hPutStrLn output "This file does not have a corresponding database entry. Do you wish to create one? [y/N]"
  wantToCreate <- hGetLine input
  if wantToCreate == "y"
    then pure <$> prepareNewEntry input output directory file
    else return []
runAddTags input output directory file (b@(Book thisFileName thisTitle thisAuthor theseTags) : bs) =
  if takeFileName file == thisFileName
    then do
      newTags <- getTags input output
      return $ Book thisFileName thisTitle thisAuthor (Set.union newTags theseTags) : bs
    else (b :) <$> runAddTags input output directory file bs

runRemoveTags :: Handle -> Handle -> FilePath -> [Book] -> IO [Book]
runRemoveTags _input _output _file [] = error "This file does not have a corresponding database entry, hence no tag can be removed."
runRemoveTags input output file (b@(Book thisFileName thisTitle thisAuthor theseTags) : bs) =
  if takeFileName file == thisFileName
    then do
<<<<<<< HEAD
      tagsToRemove <- getTagsToRemove input output theseTags
      return $ Book thisFileName thisTitle thisAuthor (Set.difference theseTags tagsToRemove) : bs
    else (b :) <$> runRemoveTags input output file bs

-- TODO Implement new lookup
getTagsToRemove :: Handle -> Handle -> Set.Set Tag -> IO (Set.Set Tag)
getTagsToRemove input output allTags = do
  hPutStrLn output "Enter the number of the tag you wish to remove. If you do not wish to remove any (more) of the tags, hit Enter."
  hPutStrLn output $ unlines [show i <> ") " <> tag | (i, tag) <- zip [1 :: Int ..] (Set.toAscList allTags)]
  numberOfTag <- hGetLine input
  case isValidInteger (length allTags) numberOfTag of
    EmptyInput -> return Set.empty
    ValidInput n ->
      -- note that indexing in a Set starts at 0, while the numbering of tags in LiTS starts from 1.
      let tagToRemove = Set.elemAt (n - 1) allTags
       in Set.insert tagToRemove <$> getTagsToRemove input output (Set.delete tagToRemove allTags)
    InvalidInput -> do
      hPutStrLn output $ "Invalid input. Please enter an integer between 1 and " <> show (length allTags) <> "!"
      getTagsToRemove input output allTags

data InputValidation a = ValidInput a | EmptyInput | InvalidInput

isValidInteger :: Int -> String -> InputValidation Int
isValidInteger _ "" = EmptyInput
isValidInteger upperLimit x = case readMaybe x :: Maybe Int of
  Just n -> if n > 0 && n <= upperLimit then ValidInput n else InvalidInput
  Nothing -> InvalidInput
