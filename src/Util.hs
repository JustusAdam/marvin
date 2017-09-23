module Util where


import           Control.Monad
import           Data.Aeson.Types
import           Data.String                 (IsString(fromString))
import qualified Data.Text                   as T
import qualified Data.Text.Lazy              as L
import           Data.Time.Clock.POSIX
import           Lens.Micro.Platform
import           Marvin.Internal.LensClasses
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

instance HasUsername SimpleWrappedUsername L.Text where username = lens unwrapUser (const SimpleWrappedUsername)
instance HasFirstName SimpleWrappedUsername (Maybe L.Text) where firstName = lens (const Nothing) const
instance HasLastName SimpleWrappedUsername (Maybe L.Text) where lastName = lens (const Nothing) const
instance HasName SimpleWrappedUsername (Maybe L.Text) where name = lens (const Nothing) const


newtype SimpleWrappedChannelName = SimpleWrappedChannelName { unwrapChannelName :: L.Text } deriving Eq

instance IsString SimpleWrappedChannelName where fromString = SimpleWrappedChannelName . fromString
instance Show SimpleWrappedChannelName where show = L.unpack . unwrapChannelName

instance HasName SimpleWrappedChannelName (Maybe L.Text) where name = lens (Just . unwrapChannelName) (\a -> maybe a SimpleWrappedChannelName)


mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f = either (Left . f) Right


mapRight :: (b -> c) -> Either a b -> Either a c
mapRight = fmap
