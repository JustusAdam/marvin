{-|
Module      : $Header$
Description : Working with json in marvin
Copyright   : (c) Justus Adam, 2016
License     : BSD3
Maintainer  : dev@justus.science
Stability   : experimental
Portability : POSIX

This is provisionary, this might get properly wrapped at some point.
-}
module Marvin.Util.JSON
    ( readJSON, writeJSON
    , module Data.Aeson
    , module Data.Aeson.TH
    ) where


import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString.Lazy   as B


readJSON :: (MonadIO m, FromJSON a) => FilePath -> m (Either String a)
readJSON = fmap eitherDecode . liftIO . B.readFile


writeJSON :: (MonadIO m, ToJSON a) => FilePath -> a -> m ()
writeJSON fp = liftIO . B.writeFile fp . encode
