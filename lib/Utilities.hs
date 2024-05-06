module Utilities
  ( safeHead,
    safeTail,
  )
where

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

safeTail :: [a] -> [a]
safeTail [] = []
safeTail l = tail l
