{-# LANGUAGE OverloadedStrings #-}

module DataBase where

import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Set as Set
import FileManager

type Tag = String

type Title = String

data Author = Author
  { firstName :: Maybe String,
    lastName :: String
  }
  deriving (Show, Eq)

instance Ord Author where
  (Author _ lastName1) <= (Author _ lastName2) = lastName1 <= lastName2

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
    author :: Set.Set Author,
    tags :: Set.Set Tag
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

dataBaseReadError :: a
dataBaseReadError = error "An error occured when reading the database. Its JSON structure might be compromised."

dataBaseWriteError :: String -> a
dataBaseWriteError newDB = error $ "An error occured when writing the database." <> show newDB

dataBaseNotFoundError :: a
dataBaseNotFoundError = error $ "No database file found: " <> dataBaseFileName

-- Check whether the database file is present in the current directory and is valid JSON.
validateDBFile :: FilePath -> IO [Book]
validateDBFile directory  = do
  isDBFilePresent <- isFileInDirectory directory dataBaseFileName
  if isDBFilePresent
    then decodeDBFile <$> dataBaseFile directory
    else dataBaseNotFoundError
  where
    decodeDBFile :: BS.ByteString -> [Book]
    decodeDBFile "" = []
    decodeDBFile dbContentString = case decodeStrict dbContentString of
      Nothing -> dataBaseReadError
      (Just dbContent) -> dbContent

writeToDataBase :: FilePath -> [Book] -> IO ()
writeToDataBase workingDirectory newDB = BL.writeFile (workingDirectory <> "/" <> dataBaseFileName) $ encode newDB

-- writeToDataBase newDB = case fmap encode newDB of
--   Nothing -> error "An error occured when writing "
--   (Just newJSON) -> B.writeFile dataBaseFileName newJSON
