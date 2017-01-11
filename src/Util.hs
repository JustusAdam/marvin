module Util where


import           Control.Monad
import           Data.Aeson.Types
import qualified Data.Text               as T
import           Data.Time.Clock.POSIX
import           Marvin.Internal.Types
import           Marvin.Interpolate.Text
import           Text.Read               (readMaybe)


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


timestampFromNumber :: Value -> Parser TimeStamp
timestampFromNumber (Number n) = return $ TimeStamp $ posixSecondsToUTCTime $ realToFrac n
timestampFromNumber (String s) = maybe mzero (return . TimeStamp . posixSecondsToUTCTime . realToFrac) (readMaybe (T.unpack s) :: Maybe Double)
timestampFromNumber _ = mzero
