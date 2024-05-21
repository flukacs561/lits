module LiTS.Filter where

import Data.Char (toLower)
import Data.List (isInfixOf)
import qualified Data.Set as Set
import LiTS.DataBase
import LiTS.Utilities

data FilterO = FilterO
  { titleF :: Maybe String,
    authorF :: Maybe String,
    tagsF :: [Tag]
  }
  deriving (Show, Eq)

runFilterCmd :: [String] -> [Book] -> [Book]
runFilterCmd = runFilter . parseFilterInput

-- Applies filters to every field of a Book structure.
runFilter :: Maybe FilterO -> [Book] -> [Book]
runFilter Nothing = id
runFilter (Just (FilterO titleM authorM tagsM)) = runFilterTags tagsM . runFilterAuthor authorM . runFilterTitle titleM

-- Generic function that applies filter to a specific field of a Book structure.
filterField :: (Book -> a) -> (String -> a -> Bool) -> Maybe String -> [Book] -> [Book]
filterField _ _ Nothing = id
filterField fieldSelector matcherFunction (Just match) = filter (\book -> match `matcherFunction` fieldSelector book)

-- Filters out the books that are tagged with each of the tags provided.
runFilterTags :: [Tag] -> [Book] -> [Book]
runFilterTags [] books = books
runFilterTags (t : ts) books = runFilterTags ts $ filterTag (Just t) books
  where
    -- Filters out the books that are tagged with a specific tag.
    filterTag :: Maybe Tag -> [Book] -> [Book]
    filterTag = filterField tags elem

runFilterAuthor :: Maybe String -> [Book] -> [Book]
runFilterAuthor = filterField author authorMatcher

-- Filters out those books where the provided string matches either the first or last name of any of the authors.
authorMatcher :: String -> Set.Set Author -> Bool
authorMatcher match = any matchSingleAuthor
  where
    matchSingleAuthor thisAuthor = matchFirstName thisAuthor || matchLastName thisAuthor
    matchFirstName thisAuthor = case firstName thisAuthor of
      Nothing -> False
      (Just name) -> textMatcher match name
    matchLastName thisAuthor = textMatcher match $ lastName thisAuthor

runFilterTitle :: Maybe String -> [Book] -> [Book]
runFilterTitle = filterField title textMatcher

-- This function specifies the algorithm that decides whether a piece of string matches any of the fields.
textMatcher :: String -> String -> Bool
textMatcher s1 s2 = isInfixOf (fmap toLower s1) (fmap toLower s2)

{- Parses a list of strings into a FilterO structure, which is then used to perform the actual filtering.
The expected structure of the input is bash-like with the following flags:
 * "-T": title
 * "-a": author
 * "-t": tags
Zero or more of these flags may be provided.
Example:
lits filter -a hartsho -T alg -t maths ag -}
parseFilterInput :: [String] -> Maybe FilterO
parseFilterInput input = run input $ Just (FilterO Nothing Nothing [])
  where
    run [] (Just (FilterO Nothing Nothing [])) = Nothing
    run [] (Just filterO) = Just filterO
    run ("-T" : strs) (Just (FilterO _ authorM tagsM)) = case safeHead strs of
      Nothing -> Nothing
      (Just titleM') -> run (safeTail strs) $ Just (FilterO (Just titleM') authorM tagsM)
    run ("-a" : strs) (Just (FilterO titleM _ tagsM)) = case safeHead strs of
      Nothing -> Nothing
      (Just authorM') -> run (safeTail strs) $ Just (FilterO titleM (Just authorM') tagsM)
    run ("-t" : strs) (Just (FilterO titleM authorM _)) = case takeWhile (\str -> head str /= '-') strs of
      [] -> Nothing
      tagsM' -> run (dropWhile (\str -> head str /= '-') strs) $ Just (FilterO titleM authorM tagsM')
    run _ _ = Nothing
