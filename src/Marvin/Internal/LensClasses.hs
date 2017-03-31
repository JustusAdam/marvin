module Marvin.Internal.LensClasses where

import           Control.Lens

class HasContent s a | s -> a where content :: Lens' s a
class HasUrl s a | s -> a where url :: Lens' s a
class HasName s a | s -> a where name :: Lens' s a
class HasLastName s a | s -> a where lastName :: Lens' s a
class HasFirstName s a | s -> a where firstName :: Lens' s a
class HasCreationDate s a | s -> a where creationDate :: Lens' s a
class HasSize s a | s -> a where size :: Lens' s a
class HasUsername s a | s -> a where username :: Lens' s a
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
