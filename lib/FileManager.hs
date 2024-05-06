module FileManager
  ( isFileInWorkingDirectory,
    deleteFile,
    createDBFile,
    dataBaseFileName,
    dataBaseFile,
  )
where

import qualified Data.ByteString.Lazy as B
import System.Directory
import System.FilePath

dataBaseFileName :: FilePath
dataBaseFileName = "test-data.json"

dataBaseFile :: IO B.ByteString
dataBaseFile = B.readFile dataBaseFileName

isFileInWorkingDirectory :: String -> IO Bool
isFileInWorkingDirectory path = do
  files <- getDirectoryContents "."
  pure $ path `elem` files

deleteFile :: [String] -> IO ()
deleteFile [] = error "No file specified for deletion"
deleteFile [path] = removeFile path
deleteFile _ = error "Too many arguments provided"

createDBFile :: IO ()
createDBFile = writeFile dataBaseFileName ""
