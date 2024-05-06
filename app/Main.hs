module Main where

import Commands
    ( doInit, doAdd, doDelete, doFilter, doList, invalidCommandError )
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
    (Just "check") -> undefined
    (Just "clean-up") -> undefined
    (Just "add-tag") -> undefined
    (Just "remove-tag") -> undefined
    (Just "filter") -> doFilter (safeTail args) dataBase
    (Just cmd) -> putStrLn $ invalidCommandError cmd
