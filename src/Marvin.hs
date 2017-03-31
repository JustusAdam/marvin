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
    , ScriptDefinition, IsAdapter, HasFiles
    -- * Reacting
    , BotReacting
    -- ** Reaction Functions
    , hear, respond, enter, exit, enterIn, exitFrom, topic, topicIn, fileShared, fileSharedIn, customTrigger
    -- ** Getting data
    -- | The type signature for the functions in this section is so large to allow this function to be used both in 'BotReacting' and 'ScriptDefinition'.
    , getData, getMessage, getMatch, getTopic, getChannel, getUser, getRemoteFile, getTimeStamp, resolveUser, resolveChannel
    -- *** File interactions
    , readTextFile, readFileBytes, newLocalFile, shareFile
    -- ** Sending messages
    , send, reply, messageChannel, messageChannel'
    -- ** Interaction with the config
    , getConfigVal, requireConfigVal, getBotName
    -- ** Handler Types
    , Message, User, Channel, Topic
    -- ** Lenses
    , HasActions(actions), HasUsername(username), HasName(name), HasFirstName(firstName), HasLastName(lastName), HasFileType(fileType), HasUrl(url), HasCreationDate(creationDate), HasSize(size), HasContent(content)
    -- ** Advanced actions
    , extractAction, extractReaction
    ) where


import           Marvin.Adapter        (IsAdapter)
import           Marvin.Internal
import           Marvin.Internal.Types hiding (getChannelName, getFileName, getFileSize,
                                        getFileType, getFileUrl, getUsername, messageChannel,
                                        newLocalFile, readFileBytes, readFileContents, readTextFile,
                                        resolveChannel, resolveUser, shareFile, shareLocalFile)
