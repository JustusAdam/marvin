module Util where


import           Control.Monad.Logger
import qualified Data.Text               as T
import           Marvin.Interpolate.Text


notImplemented :: a
notImplemented = error "Not implemented"


addPrefix :: T.Text -> T.Text -> T.Text
addPrefix prefix source
        | T.null source = prefix
        | otherwise = $(isT "%{prefix}.%{source}")


adaptLoggingSource :: (d -> b)  -> (a -> b -> c) -> a -> d -> c
adaptLoggingSource adapt old loc source = old loc (adapt source)


loggingAddSourcePrefix :: T.Text -> (a -> T.Text -> c) -> a -> T.Text -> c
loggingAddSourcePrefix = adaptLoggingSource . addPrefix


type LoggingFn = Loc -> LogSource -> LogLevel -> LogStr -> IO ()
