module Main where

import Commands
    ( doInit, doAdd, doDelete, doFilter, doList, invalidCommandError, doImport, doRemoveDups, doClean, doAddTags, doRemoveTags)
import DataBase (validateDBFile)
import System.Environment (getArgs)
import Utilities (safeHead, safeTail)

main :: IO ()
main = do
  args <- getArgs
  dataBase <- validateDBFile
  case safeHead args of
    Nothing -> putStrLn "No argument was provided."
    (Just "init") -> doInit (safeTail args)
    (Just "list") -> doList (safeTail args) dataBase
    (Just "add") -> doAdd (safeTail args) dataBase
    (Just "delete") -> doDelete (safeTail args) dataBase
    (Just "import") -> doImport (safeTail args) dataBase
    (Just "clean") -> doClean (safeTail args) dataBase
    (Just "add-tags") -> doAddTags (safeTail args) dataBase
    (Just "at") -> doAddTags (safeTail args) dataBase
    (Just "remove-tags") -> doRemoveTags (safeTail args) dataBase
    (Just "rt") -> doRemoveTags (safeTail args) dataBase
    (Just "filter") -> doFilter (safeTail args) dataBase
    (Just "remove-dups") -> doRemoveDups (safeTail args) dataBase
    (Just cmd) -> putStrLn $ invalidCommandError cmd
