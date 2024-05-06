module FileManager (isFileInWorkingDirectory, deleteFile) where

import System.Directory

isFileInWorkingDirectory :: String -> IO Bool
isFileInWorkingDirectory path = do
  files <- getDirectoryContents "."
  pure $ path `elem` files

deleteFile :: [String] -> IO ()
deleteFile [] = error "No file specified for deletion"
deleteFile [path] = removeFile path
deleteFile _ = error "Too many arguments provided"
