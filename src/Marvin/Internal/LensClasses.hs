module Marvin.Internal.LensClasses where

import           Lens.Micro.Platform

-- | Used to contrain 'Marvin.LocalFile' and make the content available to handler functions.
class HasContent s a | s -> a where content :: Lens' s a
-- | Used to constrain 'Marvin.RemoteFile' and make a download url available to handler functions.
class HasUrl s a | s -> a where url :: Lens' s a
-- | Constrains 'Marvin.User' and 'Marvin.Channel' and more to make a name accessible to handler functions.
class HasName s a | s -> a where name :: Lens' s a
-- | Constrains 'Marvin.User' and make an optional last name available to handler functions.
class HasLastName s a | s -> a where lastName :: Lens' s a
-- | Constrains 'Marvin.User' and make an optional first name available to handler functions.
class HasFirstName s a | s -> a where firstName :: Lens' s a
-- | Constrains the 'Marvin.RemoteFiles' and makes a creation date available to handler functions.
class HasCreationDate s a | s -> a where creationDate :: Lens' s a
-- | Constrains 'Marvin.RemoteFile' to have a size.
class HasSize s a | s -> a where size :: Lens' s a
-- | Constrains 'Marvin.User' to have a username.
class HasUsername s a | s -> a where username :: Lens' s a
-- | Constrains files with an optional type
class HasFileType s a | s -> a where fileType :: Lens' s a

class HasScriptId s a | s -> a where scriptId :: Lens' s a
class HasConfig s a | s -> a where config :: Lens' s a
class HasAdapter s a | s -> a where adapter :: Lens' s a
class HasPayload s a | s -> a where payload :: Lens' s a
class HasTopicChangeIn s a | s -> a where topicChangeIn :: Lens' s a
class HasTopicChange s a | s -> a where topicChange :: Lens' s a
class HasResponds s a | s -> a where responds :: Lens' s a
class HasLeavesFrom s a | s -> a where leavesFrom :: Lens' s a
class HasLeaves s a | s -> a where leaves :: Lens' s a
class HasJoinsIn s a | s -> a where joinsIn :: Lens' s a
class HasJoins s a | s -> a where joins :: Lens' s a
class HasHears s a | s -> a where hears :: Lens' s a
class HasFileShares s a | s -> a where fileShares :: Lens' s a
class HasFileSharesIn s a | s -> a where fileSharesIn :: Lens' s a
class HasCustoms s a | s -> a where customs :: Lens' s a
class HasActions s a | s -> a where actions :: Lens' s a
