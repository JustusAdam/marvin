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
    -- * Compile time regex options
    , Re.PCREOption

    , Re.anchored, Re.auto_callout
    , Re.caseless, Re.dollar_endonly
    , Re.dotall, Re.dupnames, Re.extended
    , Re.extra, Re.firstline, Re.multiline
    , Re.newline_cr
    , Re.newline_crlf, Re.newline_lf, Re.no_auto_capture
    , Re.ungreedy, Re.utf8, Re.no_utf8_check

    -- * Runtime regex options
    , Re.PCREExecOption

    , Re.exec_anchored
    , Re.exec_newline_cr, Re.exec_newline_crlf, Re.exec_newline_lf
    , Re.exec_notbol, Re.exec_noteol, Re.exec_notempty
    , Re.exec_no_utf8_check, Re.exec_partial
    -- ** Unstable
    , unwrapRegex
    ) where

import           Data.String
import qualified Text.Regex.PCRE.Light.Char8 as Re


-- | Abstract Wrapper for a reglar expression implementation. Has an 'IsString' implementation, so literal strings can be used to create a 'Regex'.
-- Alternatively use 'r' to create one with custom options.
newtype Regex = Regex Re.Regex

-- Warning: This exposes the underlying repreentation of a 'Regex' and under no curcumstances should be considered stable.
unwrapRegex :: Regex -> Re.Regex
unwrapRegex (Regex r) = r


instance Show Regex where
    show = show . unwrapRegex


-- | A match to a 'Regex'. Index 0 is the full match, all other indexes are match groups.
type Match = [String]

-- | Compile a regex with options
--
-- Normally it is sufficient to just write the regex as a plain string and have it be converted automatically, but if you want certain match options you can use this function.
r :: [Re.PCREOption] -> String -> Regex
r opts s = Regex $ Re.compile s opts


instance IsString Regex where
    fromString "" = error "Empty regex is not permitted, use '.*' or similar instead"
    fromString s = r [] s


-- | Match a regex against a string and return the first match found (if any).
match :: [Re.PCREExecOption] -> Regex -> String -> Maybe Match
match opts re s = Re.match (unwrapRegex re) s opts
