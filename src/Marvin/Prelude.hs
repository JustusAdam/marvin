{-|
Module      : $Header$
Description : A collection of all modules useful for using marvin.
Copyright   : (c) Justus Adam, 2016
License     : BSD3
Maintainer  : dev@justus.science
Stability   : experimental
Portability : POSIX
-}
module Marvin.Prelude
    ( 
    -- | For exposing parameterised and generalised versions of prelude functions
    module ClassyPrelude
    -- | The best JSON library for Haskell     
    , module Data.Aeson
    -- | For automatically generating JSON conversions  
    , module Data.Aeson.TH
    -- | For Regular expressions which work with 'Text'
    , module Data.Text.ICU
    -- | Marvin :3   
    -- 
    -- Common functions and Types         
    , module Marvin
    -- | Mutable references in marvin scripts 
    , module Marvin.Mutable
    -- | Running marvin  
    , module Marvin.Run
    -- | Format strings which resolve to efficient Strings, aka 'Text'
    , module Data.Text.Format
    ) where

import           ClassyPrelude  
import           Data.Aeson     
import           Data.Aeson.TH  
import           Data.Text.ICU   (MatchOption (..))
import           Marvin         
import           Marvin.Mutable 
import           Marvin.Run  
import           Data.Text.Format (format)