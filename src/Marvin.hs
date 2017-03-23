{-|
Module      : $Header$
Description : Marvin the modular bot
Copyright   : (c) Justus Adam, 2016
License     : BSD3
Maintainer  : dev@justus.science
Stability   : experimental
Portability : POSIX

For the proper, verbose documentation see <https://marvin.readthedocs.org/en/latest/scripting.html>.

-}
module Marvin
    (
    -- * The Script
      Script, defineScript, ScriptInit
    , ScriptId
    , ScriptDefinition, IsAdapter
    -- * Reacting
    , BotReacting
    -- ** Reaction Functions
    , hear, respond, enter, exit, enterIn, exitFrom, topic, topicIn, customTrigger
    -- ** Getting data
    -- | Some documentation
    , getData, getMessage, getMatch, getTopic, getChannel, getUser, resolveUser, resolveChannel
    -- *** File interactions
    , readTextFile, readFileBytes, newLocalFile, shareFile
    -- ** Sending messages
    , send, reply, messageChannel, messageChannel'
    -- ** Interaction with the config
    , getConfigVal, requireConfigVal, getBotName
    -- ** Handler Types
    , Message, User, Channel, Topic
    -- Lenses
    , HasActions(actions), HasUsername(username), HasName(name), HasFirstName(firstName), HasLastName(lastName), HasType_(type_), HasUrl(url), HasCreationDate(creationDate), HasSize(size), HasContent(content)
    -- ** Advanced actions
    , extractAction, extractReaction
    ) where


import           Marvin.Adapter        (IsAdapter)
import           Marvin.Internal
import           Marvin.Internal.Types hiding (getChannelName, getFileName, getFileSize,
                                        getFileType, getFileUrl, getUsername, messageChannel,
                                        readFileContents, resolveChannel, resolveUser,
                                        shareLocalFile, readTextFile, readFileBytes, newLocalFile, shareFile)
