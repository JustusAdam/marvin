{-|
Module      : $Header$
Description : Performing http/https requests from scripts
Copyright   : (c) Justus Adam, 2016
License     : BSD3
Maintainer  : dev@justus.science
Stability   : experimental
Portability : POSIX


-}
module Marvin.Util.HTTP where


import ClassyPrelude
import qualified Network.Wreq as W
import Control.Lens
import Data.Aeson
import Data.Text.Format


type Error = LText


mapEither :: (a -> c) -> (b -> d) -> Either a b -> Either c d
mapEither m _ (Left a) = Left $ m a
mapEither _ m (Right b) = Right $ m b


getAs :: MonadIO m => (LByteString -> Either Error a) -> LText -> m (Either Error a)
getAs as url = do 
    res <- liftIO $ W.get (unpack url)
    return $ case res ^. W.responseStatus . W.statusCode of
                200 -> as $ res ^. W.responseBody
                code -> Left $ format "Error when trying to get. Code {} Message: {} " (code, msg)
                  where
                    msg = decodeUtf8 $ fromStrict $ res ^. W.responseStatus . W.statusMessage


get :: MonadIO m => LText -> m (Either Error LText)
get = getAs (Right . decodeUtf8)


getJSON :: (FromJSON a, MonadIO m) => LText -> m (Either Error a)
getJSON = getAs (mapEither pack id . eitherDecode)