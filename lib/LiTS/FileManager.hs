module LiTS.FileManager
  ( isFileInDirectory,
    createDBFile,
    dataBaseFileName,
    dataBaseFile,
    getBookFilesFromDirectory,
    removeFile
  )
where

import qualified Data.ByteString as BS
import System.Directory ( getDirectoryContents, removeFile )
import System.FilePath ( takeExtension, takeFileName )

dataBaseFileName :: FilePath
dataBaseFileName = "test-data.json"

dataBaseFile :: FilePath -> IO BS.ByteString
dataBaseFile workingDirectory = BS.readFile $ workingDirectory <> "/" <> dataBaseFileName

isFileInDirectory :: FilePath -> FilePath -> IO Bool
isFileInDirectory directory file = do
  files <- getDirectoryContents directory
  return $ takeFileName file `elem` files

getBookFilesFromDirectory :: FilePath -> IO [String]
getBookFilesFromDirectory directory = do
  files <- getDirectoryContents directory
  pure $ filter (\file -> takeExtension file `elem` bookExtensions) files
  where
    bookExtensions = [".pdf", ".epub", ".mobi", ".djvi", ".dvi", ".ps"]

createDBFile :: FilePath -> IO ()
createDBFile workingDirectory = writeFile (workingDirectory <> "/" <> dataBaseFileName) ""
