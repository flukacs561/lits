module Main where

import LiTS.Commands
  ( doAdd,
    doAddTags,
    doClean,
    doDelete,
    doFilter,
    doImport,
    doInit,
    doList,
    doRemoveDups,
    doRemoveTags,
    invalidCommandError,
  )
import LiTS.DataBase (validateDBFile)
import LiTS.Utilities (safeHead, safeTail)
import System.Environment (getArgs)

main :: IO ()
main =
  let workingDirectory = "test-data"
   in do
        args <- getArgs
        dataBase <- validateDBFile workingDirectory
        case safeHead args of
          Nothing -> putStrLn "No argument was provided."
          (Just "init") -> doInit workingDirectory (safeTail args)
          (Just "list") -> doList (safeTail args) dataBase
          (Just "add") -> doAdd workingDirectory (safeTail args) dataBase
          (Just "delete") -> doDelete workingDirectory (safeTail args) dataBase
          (Just "import") -> doImport workingDirectory (safeTail args) dataBase
          (Just "clean") -> doClean workingDirectory (safeTail args) dataBase
          (Just "add-tags") -> doAddTags workingDirectory (safeTail args) dataBase
          (Just "at") -> doAddTags workingDirectory (safeTail args) dataBase
          (Just "remove-tags") -> doRemoveTags workingDirectory (safeTail args) dataBase
          (Just "rt") -> doRemoveTags workingDirectory (safeTail args) dataBase
          (Just "filter") -> doFilter (safeTail args) dataBase
          (Just "remove-dups") -> doRemoveDups workingDirectory (safeTail args) dataBase
          (Just cmd) -> putStrLn $ invalidCommandError cmd
