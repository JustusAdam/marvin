# 0.3 - not yet released

- Added optional support for recieving remote files and uploading local files.
- Added files support to the Shell and Slack adapter
- Changed bot runner in IsAdapter to be simpler to implement.
- Changed the IsAdapter API.
    - The new API is lens based. Adapter dependend types are constrained via the requirement for certain lens class instances.
    - Deprecated getUsername and getChannelName, these are now accessible via the lense class instances.
- Changed Util API
    - Marvin.Util.HTTP has been removed (was empty) until an API has been found
    - Marvin.Util.Mutable has been removed as BotReacting now has a `MonadBase` and `MonadBaseControl` instance meaning the functions from `lifted-base` can be used without modification.

# 0.2.3 - 05.03.2017

- A big thanks to @lubomir for testing the IRC adapter and contributing the following changes
    - Better command recognition in IRC adapter
    - Bot now properly registers in IRC
    - Added config setting to IRC for channels which the bot should join

# 0.2.2 - 05.03.2017

- Removed the integration tests from the cabal package config

# 0.2.1 - 04.03.2017

- Fixed the templates for the initializer
- added tests to ensure integration test bot and project created by initializer actually compile
- Fixed the travis configurations

# 0.2.0 - 25.02.2017

- Implemented an adapter for IRC
- Added manual lens class instances to expose the datatyes and instances using them.

# 0.1.2, 0.1.3, 0.1.4, 0.1.5 - 17.02.2017

- Added manual version bounds to be compatible with stackage and the hackage build (sorry for the version spam)

# 0.1.1 - 17.02.2017

- Added changelog
- Added version bounds
