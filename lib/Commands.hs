module Commands where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import DataBase
import Filter
import Formatting
import Types

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

safeTail :: [a] -> [a]
safeTail [] = []
safeTail l = tail l

invalidCommandError :: String -> String
invalidCommandError cmd = "Invalid argument: \"" ++ cmd ++ "\" is not an actual command in LiTS."

doList :: [String] -> Maybe [Book] -> IO ()
doList [] = printBooks
doList _ = error "Too many arguments."

doFilter :: [String] -> Maybe [Book] -> IO ()
doFilter args db = printBooks (sortByAuthorLastname . runFilterCmd args <$> db)

doAdd :: [String] -> Maybe [Book] -> IO ()
doAdd args db = do
  newBook <- prepareNewEntry args
  case fmap (encode . (newBook :)) db of
    Nothing -> dataBaseError
    (Just newJSON) -> B.writeFile dataBaseFileName newJSON

doDelete :: [String] -> Maybe [Book] -> IO ()
doDelete args db = case encode <$> removeEntry args db of
  Nothing -> dataBaseError
  (Just newJSON) -> B.writeFile dataBaseFileName newJSON
