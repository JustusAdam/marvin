{-# LANGUAGE UndecidableInstances #-}
module Util where


import           Control.Monad
import           Data.Aeson.Types
import           Data.String                 (IsString(fromString))
import           Data.Proxy
import qualified Data.Text                   as T
import qualified Data.Text.Lazy              as L
import           Data.Time.Clock.POSIX
import           Labels
import           Labels.Internal
import           Marvin.Internal.Types
import           Marvin.Interpolate.Text
import           Text.Read                   (readMaybe)


notImplemented :: a
notImplemented = error "Not implemented"


addPrefix :: T.Text -> T.Text -> T.Text
addPrefix prefix source
    | T.null source = prefix
    | otherwise = $(isT "#{prefix}.#{source}")


adaptLoggingSource :: (d -> b)  -> (a -> b -> c) -> a -> d -> c
adaptLoggingSource adapt old loc source = old loc (adapt source)


loggingAddSourcePrefix :: T.Text -> (a -> T.Text -> c) -> a -> T.Text -> c
loggingAddSourcePrefix = adaptLoggingSource . addPrefix


timestampFromNumber :: Value -> Parser (TimeStamp a)
timestampFromNumber (Number n) = return $ TimeStamp $ posixSecondsToUTCTime $ realToFrac n
timestampFromNumber (String s) = maybe mzero (return . TimeStamp . posixSecondsToUTCTime . realToFrac) (readMaybe (T.unpack s) :: Maybe Double)
timestampFromNumber _ = fail "Timestamp must be number or string"



newtype SimpleWrappedUsername = SimpleWrappedUsername { unwrapUser :: L.Text } deriving Eq

instance IsString SimpleWrappedUsername where fromString = SimpleWrappedUsername . fromString
instance Show SimpleWrappedUsername where show = L.unpack . unwrapUser

instance Has "username" L.Text SimpleWrappedUsername where 
    get _ = unwrapUser
    set _ = const . SimpleWrappedUsername
instance Has "firstName" (Maybe L.Text) SimpleWrappedUsername  where 
    get _ _ = Nothing
    set _ _ = id
instance Has "lastName" (Maybe L.Text) SimpleWrappedUsername where 
    get _ _ = Nothing
    set _ _ = id
instance Has "name" (Maybe L.Text) SimpleWrappedUsername where 
    get _ _ = Nothing
    set _ _ = id


newtype SimpleWrappedChannelName = SimpleWrappedChannelName { unwrapChannelName :: L.Text } deriving Eq

instance IsString SimpleWrappedChannelName where fromString = SimpleWrappedChannelName . fromString
instance Show SimpleWrappedChannelName where show = L.unpack . unwrapChannelName

instance Has "name" (Maybe L.Text) SimpleWrappedChannelName where 
    get _ = Just . unwrapChannelName
    set _ v r = maybe r SimpleWrappedChannelName v


mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f = either (Left . f) Right


mapRight :: (b -> c) -> Either a b -> Either a c
mapRight = fmap


ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM cond then_ else_ = cond >>= \c -> if c then then_ else else_

newtype ParseRecordStd a = ParseRecordStd { unwrapParseRecordStd :: a }

instance (Cons label value record, FromJSON value, FromJSON (ParseRecordStd record), a ~ (Consed label value record)) => FromJSON (ParseRecordStd a ) where
    parseJSON = withObject "" $ \o -> do
        ParseRecordStd inner <- parseJSON o
        let labelP = undefined :: Proxy label
        field <- o .: symbolVal labelP
        pure $ cons (labelP := field) inner