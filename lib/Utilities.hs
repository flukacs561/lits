module Utilities
  ( safeHead,
    safeTail,
    (|||)
  )
where

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

safeTail :: [a] -> [a]
safeTail [] = []
safeTail l = tail l

(|||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(|||) p1 p2 a = p1 a || p2 a
