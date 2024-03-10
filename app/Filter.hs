module Filter where

import Types

data FilterO = FilterO
  { titleF :: Maybe String,
    authorF :: Maybe String,
    tagsF :: [Tag]
  } deriving Show

runFilterCmd :: [String] -> [Book] -> [Book]
runFilterCmd = runFilter . parseFilterInput

runFilter :: Maybe FilterO -> [Book] -> [Book]
runFilter Nothing = id
runFilter (Just (FilterO titleM authorM tagsM)) = runFilterTags tagsM . runFilterAuthor authorM . runFilterTitle titleM

filterField :: (Book -> a) -> (String -> a -> Bool) -> Maybe String -> [Book] -> [Book]
filterField _ _ Nothing = id
filterField fieldSelector matcherFunction (Just match) = filter (\book -> match `matcherFunction` fieldSelector book)

runFilterTags :: [Tag] -> [Book] -> [Book]
runFilterTags [] books = books
runFilterTags (t : ts) books = runFilterTags ts $ filterTag (Just t) books
  where
    filterTag :: Maybe Tag -> [Book] -> [Book]
    filterTag = filterField tags elem

runFilterAuthor :: Maybe String -> [Book] -> [Book]
runFilterAuthor = filterField author authorMatcher

authorMatcher :: String -> [Author] -> Bool
authorMatcher match = any matchSingleAuthor
  where
    matchSingleAuthor author = matchFirstName author || matchLastName author
    matchFirstName author = case firstName author of
      Nothing -> False
      (Just name) -> textMatcher match name
    matchLastName author = textMatcher match $ lastName author

runFilterTitle :: Maybe String -> [Book] -> [Book]
runFilterTitle = filterField title textMatcher

textMatcher :: String -> String -> Bool
textMatcher = isPrefixOf

isPrefixOf :: String -> String -> Bool
isPrefixOf [] _ = True
isPrefixOf (_:_) [] = False
isPrefixOf (m:ms) (s:ss) = (m == s) && isPrefixOf ms ss

-- lits filter -a hartsho -T alg -t maths ag
parseFilterInput :: [String] -> Maybe FilterO
parseFilterInput input = run input $ Just (FilterO Nothing Nothing [])
  where
    run [] (Just (FilterO Nothing Nothing [])) = Nothing
    run [] (Just filterO) = Just filterO
    run ("-T":strs) (Just (FilterO _ authorM tagsM)) = case safeHead strs of
      Nothing -> Nothing
      (Just titleM') -> run (safeTail strs) $ Just (FilterO (Just titleM') authorM tagsM)
    run ("-a":strs) (Just (FilterO titleM _ tagsM)) = case safeHead strs of
      Nothing -> Nothing
      (Just authorM') -> run (safeTail strs) $ Just (FilterO titleM (Just authorM') tagsM)
    run ("-t":strs) (Just (FilterO titleM authorM _)) = case takeWhile (\str -> head str /= '-') strs of 
      [] -> Nothing
      tagsM' -> run (dropWhile (\str -> head str /= '-') strs) $ Just (FilterO titleM authorM tagsM')
    run _ _ = Nothing
