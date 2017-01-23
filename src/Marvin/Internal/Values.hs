module Marvin.Internal.Values where


import qualified Data.Text             as T
import qualified Data.Text.Lazy        as L
import           Marvin.Internal.Types


-- | Script id sed for the bot itself
applicationScriptId :: ScriptId
applicationScriptId = ScriptId "bot"


defaultBotName :: L.Text
defaultBotName = "marvin"


scriptConfigKey :: T.Text
scriptConfigKey = "script"


adapterConfigKey :: T.Text
adapterConfigKey = "adapter"
