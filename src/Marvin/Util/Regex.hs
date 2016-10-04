{-|
Module      : $Header$
Description : Regular expression wrapper for marvin.
Copyright   : (c) Justus Adam, 2016
License     : BSD3
Maintainer  : dev@justus.science
Stability   : experimental
Portability : POSIX
-}
module Marvin.Util.Regex
    ( Regex, Match, r, match, MatchOption(..)
    -- ** Unstable
    , unwrapRegex
    ) where


import           ClassyPrelude
import qualified Data.Text.ICU as Re
import Data.Text.ICU (MatchOption(..))


-- | Abstract Wrapper for a reglar expression implementation. Has an 'IsString' implementation, so literal strings can be used to create a 'Regex'.
-- Alternatively use 'r' to create one with custom options.
newtype Regex = Regex Re.Regex

-- Warning: This exposes the underlying repreentation of a 'Regex' and under no curcumstances should be considered stable.
unwrapRegex :: Regex -> Re.Regex
unwrapRegex (Regex r) = r


instance Show Regex where
    show = show . unwrapRegex


-- | A match to a 'Regex'. Index 0 is the full match, all other indexes are match groups.
type Match = [Text]

-- | Compile a regex with options
--
-- Normally it is sufficient to just write the regex as a plain string and have it be converted automatically, but if you want certain match options you can use this function.
r :: [Re.MatchOption] -> Text -> Regex
r opts s = Regex $ Re.regex opts s


instance IsString Regex where
    fromString "" = error "Empty regex is not permitted, use '.*' or similar instead"
    fromString s = r [] $ pack s


-- | Match a regex against a string and return the first match found (if any).
match :: Regex -> Text -> Maybe Match
match re = fmap (Re.unfold Re.group) . Re.find (unwrapRegex re)
