{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Aeson

type Tag = String

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
  toJSON (Author Nothing lastName) =
    object
      [ "lastName" .= lastName
      ]
  toJSON (Author (Just firstName) lastName) =
    object
      [ "firstName" .= firstName,
        "lastName" .= lastName
      ]

data Book = Book
  { fileName :: String,
    title :: String,
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
  toJSON (Book fileName title author tags) =
    object
      [ "fileName" .= fileName,
        "title" .= title,
        "author" .= toJSON author,
        "tags" .= tags
      ]


safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

safeTail :: [a] -> [a]
safeTail [] = []
safeTail l = tail l
