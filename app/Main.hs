module Main where

import qualified DataBase as DB
import System.Environment
import Types
import Filter

dispatch :: Maybe String -> String -> IO ()
dispatch Nothing = putStrLn . const "No argument was provided."
dispatch (Just cmd)
  | cmd == "check" = error "Not implemented yet"
  | cmd == "add" = error "Not implemented yet"
  | otherwise = putStrLn . throwError

throwError :: String -> String
throwError cmd = "Invalid argument: \"" ++ cmd ++ "\" is not an actual command in LiTS."

printBooks :: Maybe [Book] -> IO ()
printBooks Nothing = putStrLn ""
printBooks (Just books) = putStrLn $ unlines $ fmap DB.printMetaData books

main :: IO ()
main = do
  args <- getArgs
  -- input <- getContents
  dataBase <- DB.validateDBFile
  case safeHead args of
    Nothing -> putStrLn "No argument was provided."
    (Just "list") -> printBooks dataBase
    (Just "filter") -> printBooks $ runFilterCmd (tail args) <$> dataBase
    (Just cmd) -> putStrLn $ throwError cmd

testFilter :: IO ()
testFilter = do
  dataBase <- DB.validateDBFile
  printBooks $ runFilterCmd ["-t", "novel", "english", "-a", "John"] <$> dataBase
