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
    ( Regex, Match, r, match
    , Re.MatchOption(..)
    ) where

import           Control.DeepSeq
import           Data.String
import qualified Data.Text.ICU   as Re
import qualified Data.Text.Lazy  as L


-- | Abstract Wrapper for a reglar expression implementation. Has an 'IsString' implementation, so literal strings can be used to create a 'Regex'.
-- Alternatively use 'r' to create one with custom options.
newtype Regex = Regex Re.Regex

instance NFData Regex where
    rnf (Regex a) = a `seq` ()

-- Warning: This exposes the underlying repreentation of a 'Regex' and under no curcumstances should be considered stable.
unwrapRegex :: Regex -> Re.Regex
unwrapRegex (Regex r) = r


instance Show Regex where
    show = show . unwrapRegex


-- | A match to a 'Regex'. Index 0 is the full match, all other indices are match groups.
type Match = [L.Text]

-- | Compile a regex with options
--
-- Normally it is sufficient to just write the regex as a plain string and have it be converted automatically, but if you want certain match options you can use this function.
r :: [Re.MatchOption] -> L.Text -> Regex
r opts = Regex . Re.regex opts . L.toStrict


instance IsString Regex where
    fromString "" = error "Empty regex is not permitted, use '.*' or similar instead"
    fromString s = r [] (L.pack s)


-- | Match a regex against a string and return the first match found (if any).
match :: Regex -> L.Text -> Maybe Match
match re s = map L.fromStrict . Re.unfold Re.group <$> Re.find (unwrapRegex re) (L.toStrict s)
