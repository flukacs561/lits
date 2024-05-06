module FileManager
  ( isFileInWorkingDirectory,
    createDBFile,
    dataBaseFileName,
    dataBaseFile,
    getBookFilesFromDirectory,
    removeFile
  )
where

import qualified Data.ByteString as BS
import System.Directory
import System.FilePath

dataBaseFileName :: FilePath
dataBaseFileName = "test-data.json"

dataBaseFile :: IO BS.ByteString
dataBaseFile = BS.readFile dataBaseFileName

isFileInWorkingDirectory :: String -> IO Bool
isFileInWorkingDirectory path = do
  files <- getDirectoryContents "."
  pure $ path `elem` files

getBookFilesFromDirectory :: IO [String]
getBookFilesFromDirectory = do
  files <- getDirectoryContents "."
  pure $ filter (\file -> takeExtension file `elem` bookExtensions) files
  where
    bookExtensions = [".pdf", ".epub", ".mobi", ".djvi", ".dvi", ".ps"]

createDBFile :: IO ()
createDBFile = writeFile dataBaseFileName ""
