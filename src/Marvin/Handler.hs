{-|
Module      : $Header$
Description : Some generic handlers for marvin bots
Copyright   : (c) Justus Adam, 2017
License     : BSD3
Maintainer  : dev@justus.science
Stability   : experimental
Portability : POSIX

Generic handlers for marvin scripts.
You can use them directly or have a look at the source as an example of how to use marvins features.
-}
module Marvin.Handler where

import qualified Data.Text.Lazy   as L
import qualified Data.Version     as V
import           Marvin.Prelude
import           Marvin.Types
import qualified Paths_marvin     as P
import           System.Directory
import           System.FilePath


-- | Sends the name of the bot and the version of the marvin library as a chat message
echoVersion :: (IsAdapter a, Has "channel" (Channel a) d) => BotReacting a d ()
echoVersion = do
    botname <- getBotName
    send $(isL "I am #{botname}, a bot built with the marvin library version #{V.showVersion P.version}.")


-- | Download any shared file which was not shared by the bot itself (@uploader^.username /= botname@)
--
-- The boolean decides whether to send a message of success or failure to the originating channel.
downloadFile :: (IsAdapter a, SupportsFiles a, Has "file" (RemoteFile a) d, Has "user" (User a) d, Has "channel" (Channel a) d) => Bool -> FilePath -> BotReacting a d ()
downloadFile report directory = do
    botname <- getBotName
    uploader <- getUser
    unless (botname == uploader^.username) $ do
        f <- getRemoteFile
        res <- saveFileToDir f directory
        let msg = case res of
                    Left err   -> $(isL "Failed to save file: #{err}")
                    Right path -> $(isL "File saved to path: #{path}")
        when report $ send msg
        logInfoN $ L.toStrict msg


-- | Upload a file referenced by a command.
--
-- The boolean decides whether to send a message of success or failure to the originating channel.
-- The @Int@ is the index for the filepath in the regex match.
uploadFile :: (IsAdapter a, SupportsFiles a, Has "match" Match d, Has "channel" (Channel a) d) => Bool -> Int -> BotReacting a d ()
uploadFile report index = (^?ix index) <$> getMatch >>= \case
    Nothing -> logErrorN "Could not find expected index in match"
    Just rawPath
        | isAbsolute path -> send "Please provide a relative path"
        | ".." `elem` splitDirectories path -> send "'..' is not allowed in the upload path"
        | otherwise -> do
            e <- liftIO $ doesFileExist path

            if e
                then do
                    chan <- getChannel
                    f <- newLocalFile path' (FileOnDisk path)
                    res <- shareFile f [chan]
                    case res of
                        Left err -> reporter $(isL "Failed to share file: #{err}")
                        Right _  -> reporter "File successfully uploaded"
                else send $(isL "Sorry, but there is no file with the path #{path} here.")
      where
        path' = L.strip rawPath
        path = L.unpack path'
  where
    reporter | report    = send
             | otherwise = logInfoN . L.toStrict
