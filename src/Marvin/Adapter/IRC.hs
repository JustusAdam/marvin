{-|
Module      : $Header$
Description : Adapter for communicating with IRC.
Copyright   : (c) Justus Adam, 2017
License     : BSD3
Maintainer  : dev@justus.science
Stability   : experimental
Portability : POSIX

See caveats and potential issues with this adapter here <https://marvin.readthedocs.io/en/latest/adapters.html#irc>.
-}
{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE ViewPatterns #-}
module Marvin.Adapter.IRC
    ( IRCAdapter, IRCChannel
    ) where


import           Control.Concurrent.Async.Lifted
import           Control.Concurrent.Chan.Lifted
import           Control.Exception.Lifted
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Data.ByteString                 (ByteString)
import           Data.Conduit
import           Data.Maybe
import           Data.Monoid                     ((<>))
import qualified Data.Text.Encoding              as T
import qualified Data.Text.Lazy                  as L
import           Data.Time.Clock                 (getCurrentTime)
import           Marvin.Adapter
import           Marvin.Interpolate.All
import           Marvin.Types                    as MT
import           Network.IRC.Conduit             as IRC
import           Util


type MarvinIRCMsg = IRC.Message L.Text


-- Im not happy with this yet, we need to distinguish users and channels somehow
data IRCChannel
    = RealChannel { chanName :: L.Text }
    | Direct      { chanName :: L.Text }

instance HasName IRCChannel L.Text where name = lens chanName (\a b -> a {chanName = b})


data IRCAdapter = IRCAdapter
    { msgOutChan :: Chan MarvinIRCMsg
    }


producer :: Chan MarvinIRCMsg -> Producer IO IrcMessage
producer chan = forever $ do
    msg <- readChan chan
    yield $ T.encodeUtf8 . L.toStrict <$> msg


consumer :: Chan i -> ConduitM i o IO ()
consumer = awaitForever . writeChan


-- NOTE: Maybe we can add some verification of how the server was coping with a message of ours.
-- Perhaps save queries to the server in a queue and associate incoming numerics and such with them?
processor :: Chan (Either ByteString IrcEvent) -> EventConsumer IRCAdapter -> AdapterM IRCAdapter ()
processor inChan handler = do
    IRCAdapter{msgOutChan} <- getAdapter
    let handleOneMessage = readChan inChan >>= \case
            Left bs -> logInfoN $(isT "Undecodable message: #{T.decodeUtf8 bs}")
            Right rawEv -> do
                let ev = fmap (L.fromStrict . T.decodeUtf8) rawEv
                ts <- liftIO $ TimeStamp <$> getCurrentTime
                let origin = case _source ev of
                                        User nick -> Just (SimpleWrappedUsername nick, Direct nick)
                                        Channel chan user -> Just (SimpleWrappedUsername user, RealChannel chan)
                                        Server _ -> Nothing
                                        -- TODO What do we do if the server sends a message here?
                case origin of
                    Nothing -> logInfoN "Ignoring message from server"
                    Just (user, channel) ->
                        case _message ev of
                            Privmsg target (Right msg) -> do
                                -- Privmsg can either be a private message directly to
                                -- the user, or it could be a message going to a
                                -- channel.
                                botname <- getBotname
                                let (cmd, msg') = isMention botname target msg
                                handler $ cmd user channel msg' ts
                            Notice target (Right msg) -> do
                                botname <- getBotname
                                -- Check if bot is addressed
                                handler $ (if target == botname then CommandEvent else MessageEvent) user channel msg ts
                            Join channel' -> handler $ ChannelJoinEvent user (RealChannel channel') ts
                            Part channel' _ -> handler $ ChannelLeaveEvent user (RealChannel channel') ts
                            Kick channel' nick _ -> handler $ ChannelLeaveEvent (SimpleWrappedUsername nick) (RealChannel channel') ts
                            Topic channel' t -> handler $ TopicChangeEvent user (RealChannel channel') t ts
                            Ping a b -> writeChan msgOutChan $ Pong $ fromMaybe a b
                            Invite chan _ -> writeChan msgOutChan $ Join chan
                            _ -> logDebugN $(isT "Unhandled event #{rawEv}")
    forever $
        handleOneMessage `catch` (\e -> logErrorN $(isT "UserError: #{e :: ErrorCall}"))

-- If the bot is the target of the message, it's a command. Also we should
-- treat the message as a command if the message starts with the name of the
-- bot followed by a colon or a comma.
--
-- For such message we strip off the name, separator and whitespace and return
-- just the important bit.
--
--  * "marvin: hello" returns (CommandEvent, "hello")
--  * "hey everyone"  returns (MessageEvent, "hey everyone")
isMention :: IsAdapter a
          => L.Text     -- ^Bot name
          -> L.Text     -- ^Target of message
          -> L.Text     -- ^The actual message text
          -> (User a -> Channel a -> L.Text -> TimeStamp a -> MT.Event a, L.Text)
isMention botname target msg
  | L.head target /= '#' = (CommandEvent, msg)
  | otherwise = case msg of
      (L.stripPrefix (botname <> ", ") -> Just msg') -> (CommandEvent, L.stripStart msg')
      (L.stripPrefix (botname <> ": ") -> Just msg') -> (CommandEvent, L.stripStart msg')
      _                                              -> (MessageEvent, msg)


setUp :: Chan MarvinIRCMsg -> L.Text -> [L.Text] -> IO ()
setUp chan username channels = do
    writeChan chan (Nick username)
    writeChan chan (RawMsg $ "User " <> username <> " 0 * :" <> username)
    writeList2Chan chan $ map Join channels

instance IsAdapter IRCAdapter where
    -- | Stores the username
    type User IRCAdapter = SimpleWrappedUsername
    -- | Stores channel name
    type Channel IRCAdapter = IRCChannel

    adapterId = "irc"
    messageChannel chan msg = do
        IRCAdapter{msgOutChan} <- getAdapter
        writeChan msgOutChan $ msgType $ Right msg
      where
        msgType = case chan of
                      Direct n      -> Privmsg n
                      RealChannel c -> Notice c

    -- TODO Perhaps these resolving funtions should be changed such that
    -- they return Nothing if the user doesn't exist.
    resolveChannel = return . Just . RealChannel
    -- | Just returns the value again
    resolveUser = return . Just . SimpleWrappedUsername
    initAdapter = IRCAdapter <$> newChan
    runAdapter handler = do
        port <- fromMaybe 7000 <$> lookupFromAdapterConfig "port"
        host <- requireFromAdapterConfig "host"
        user <- getBotname
        channels <- requireFromAdapterConfig "channels"
        IRCAdapter{msgOutChan} <- getAdapter
        inChan <- newChan
        a <- async $ processor inChan handler
        link a
        liftIO $ do
            setUp msgOutChan user channels
            ircClient port host (return ()) (consumer inChan) (producer msgOutChan)
