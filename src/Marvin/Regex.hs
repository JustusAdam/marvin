module Marvin.Regex where


import           ClassyPrelude
import qualified Data.Text.ICU as Re


-- | Abstract Wrapper for a reglar expression implementation. Has an 'IsString' implementation, so literal strings can be used to create a 'Regex'.
-- Alternatively use 'r' to create one with custom options.

newtype Regex = Regex { unwrapRegex :: Re.Regex }


instance Show Regex where
    show (Regex r) = "Regex " ++ show (Re.pattern r)


-- | A match to a 'Regex'. Index 0 is the full match, all other indexes are match groups.
type Match = [Text]

-- | Compile a regex with options
--
-- Normally it is sufficient to just write the regex as a plain string and have it be converted automatically, but if you wnat certain match options you can use this function.
r :: [Re.MatchOption] -> Text -> Regex
r opts s = Regex $ Re.regex opts s


instance IsString Regex where
    fromString = r [] . pack


-- | Match a regex against a string and return the first match found (if any).
match :: Regex -> Text -> Maybe Match
match re = fmap (Re.unfold Re.group) . Re.find (unwrapRegex re)
