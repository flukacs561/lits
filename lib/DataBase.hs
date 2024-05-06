{-# LANGUAGE OverloadedStrings #-}

module DataBase where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import FileManager

type Tag = String
type Title = String

data Author = Author
  { firstName :: Maybe String,
    lastName :: String
  }
  deriving (Show, Eq)

instance FromJSON Author where
  parseJSON (Object v) =
    Author
      <$> v .:? "firstName"
      <*> v .: "lastName"
  parseJSON _ = mempty

instance ToJSON Author where
  toJSON (Author Nothing thisLastName) =
    object
      [ "lastName" .= thisLastName
      ]
  toJSON (Author (Just thisFirstName) thisLastName) =
    object
      [ "firstName" .= thisFirstName,
        "lastName" .= thisLastName
      ]

data Book = Book
  { fileName :: String,
    title :: Title,
    author :: [Author],
    tags :: [Tag]
  }
  deriving (Show, Eq)

instance FromJSON Book where
  parseJSON (Object v) =
    Book
      <$> v .: "fileName"
      <*> v .: "title"
      <*> v .: "author"
      <*> v .: "tags"
  parseJSON _ = mempty

instance ToJSON Book where
  toJSON (Book thisFileName thisTitle thisAuthor thisTags) =
    object
      [ "fileName" .= thisFileName,
        "title" .= thisTitle,
        "author" .= toJSON thisAuthor,
        "tags" .= thisTags
      ]

dataBaseError :: a
dataBaseError = error "An error occured when reading the database."

dataBaseFileName :: FilePath
dataBaseFileName = "test-data.json"

dataBaseFile :: IO B.ByteString
dataBaseFile = B.readFile dataBaseFileName

-- Check whether the database file is present in the current directory and is valid JSON.
validateDBFile :: IO (Maybe [Book])
validateDBFile = do
  isDBFilePresent <- isFileInWorkingDirectory dataBaseFileName
  if isDBFilePresent
    then fmap decode dataBaseFile
    else pure Nothing

writeToDataBase :: Maybe [Book] -> IO ()
writeToDataBase newDB = case fmap encode newDB of
  Nothing -> dataBaseError
  (Just newJSON) -> B.writeFile dataBaseFileName newJSON
