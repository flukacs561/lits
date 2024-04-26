{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Aeson

type Tag = String
type Title = String

data Author = Author
  { firstName :: Maybe String,
    lastName :: String
  }
  deriving (Show)

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
  deriving (Show)

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

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

safeTail :: [a] -> [a]
safeTail [] = []
safeTail l = tail l
