# Marvin, the paranoid bot (⍺ stage)

Marvin is an attempt to combine the ease of use of [hubot](https://hubot.github.com) with the typesafety and easy syntax of Haskell and the performance gains from compiled languages.

## Installation

Marvin is not on Hackage yet, so to get it you can use [stack](https://docs.haskellstack.org) and add this repository to your `stack.yaml` file.
Stack will take care of downloading and building it for you then.

For the regex module marvin further requires the `-dev` version of the `icu` library.

## TLDR

```Haskell
{-# LANGUAGE NoImplicitPrelude #-}
module MyScript where

import Marvin.Prelude

script = defineScript "my-script" $ do
    hear "I|i can't stand this (\w+)" $ do
        match <- getMatch

        let thing = match `indexEx` 1

        reply $ "I'm sorry to tell you but you'll have to do " ++ thing
    
    respond "open the (\w+) door" $ do
        match <- getMatch
        let door = match `indexEx` 1
        openDoor door
        send $ "Door " ++ door ++ " opened"
    
    respond "what is in file (\w+)" $ do
        match <- getMatch 
        let file = match `indexEx` 1

        liftIO $ readFile file

        send file
```

## How to Marvin

The best way to use Marvin is very much taken from hubot.

Marvin composes of a collection of scripts which are reactions or actions on certain messages posted in slack.
Each script is a Haskell source file. 
They get compiled into one single static binary, which is a HTTP server that listens for slack's event calls.

### Defining scripts

Defining scripts is very easy.

Create a new Haskell source file like "MyScript.hs" and import marvins prelude `Marvin.Prelude`.
This provides you with all the tools you need to interact with marvin.
Since marvins prelude overwrites functions from the Haskell prelude you should hide the Haskell Prelude with either `{-# LANGUAGE NoImplicitPrelude #-}` at the top of the file or by using `import Prelude ()`.
For more information why this is necessary see section [Why no prelude?](#why-no-prelude).

Now you can start to define your script with `defineScript` which produces a script initializer.
If you wish to use marvins automatic script discovery your script initializer should be named `script`  

```Haskell
{-# LANGUAGE NoImplicitPrelude #-}
module MyScript where

import Marvin.Prelude

script = defineScript "my-script" $ do
    ...
```

The script id, "my-script" in this case, is the name used for this script when repoting loggin messages as well as the key for this scripts configuration, see [configuration](#configuration).

In the define script block you can have marvin react to certain events with `hear` and `respond`.
More information on those in the section [reacting](#reacting)

Finally after you have defined your scripts you have to tie them together.
You can do this [manually](#wiring-manually) or you can have marvin create the boilerplate code for you.

To do this simply place a main file (this is the file you'll be compiling later) in the same directory the scripts are placed in.
Leave the file empty except for this line at the top `{-# OPTIONS_GHC -F -pgmF marvin-pp #-}`.
When you compile the file marvin will look for any other ".hs" and ".lhs" files in the same directory, import them and define a server which runs with the `script` from each.
If you wish to hide a file from the auto discovery either place it in a different directory or prefix it with "." or "_".

### Reacting

There are two main ways (currently) of reacting to events, `hear` and `respond`.

`hear` is for matching any incoming message. The provided regex is tried against all incomming messages, if one matches the handler is called.

`repond` only triggers on message which have the bot name, or a case variation thereof as the first word.


Once a handler has triggered it may perform arbitrary IO actions (using `liftIO`) and send messages using `reply` and `send`.

- `reply` addresses the message to the original sender of the message that triggered the handler.
- `send` sends it to the same room the tiggering message weas sent to.
- `messageRoom` sends a message to a room specified by the user.

### Configuration

Configuration for marvin is written in the [configurator](https://hackage.haskell.com/package/configurator) syntax.

Configuration pertaining to the bot is stored under the "bot" key.

```
bot {
    port = 8080
    name = "my-bot"
    logging = "INFO"
}
```

By default each script has access to a configuration stored under its script id.
And of course these scripts can have nested config groups.

```
bot {
    port = 8080
    name = "my-bot"

}
script-1 {
    some-string = "foo"
    some-int = 1337
    bome-bool = true
}
script 2 {
    nested-group {
        val = false
    }
    name = "Trump"
    capable = false
}
``` 

### Wiring manually

Currently marvin only supports creating a HTTP server.
You can create the server yourself with the `runServer` function.
You'll want to collect all `ScriptInit` initializers and hand them to `runServer` which runs a server with the provided scripts.
In the future it is planned to make the server more modular.

### Why no prelude?

To be more efficient this library uses the `Text` type for strings, rather than Haskells `String`.
Most of the usual prelude functions work only on lists/strings but not `Text`.
Plus the Haskell prelude is quite bad.
Marvin therefore exports the `ClassyPrelude` as well wich exports sequence manipulation operations that work on lists and strings as well as text and have the same name as the prelude functions.
Therefore you should be able to write your code as usual using the same functions you would in prelude.

### Utilities

#### Regex

Implemented in `Marvin`, documentation coming soon.

#### Mutable variables

Implemented in `Marvin.Mutable`, documentation coming soon.

#### Format strings

Coming soon.

#### JSON

Exposed in `Marvin` documentation coming soon. Until the refer to [aeson](https://hackage.haskell.org/package/aeson).

#### Logging

Marvin comes with a logging facility built in. 
`Marvin.Logging` (and `Marvin.Prelude`) expose the logging facility. 
Several functions are available, depending on the urgency of your message, like `errorM`, `infoM` and `criticalM`.
Logging messages made this way are automatically formatted and tagged with the scripts that reported them.

By default all logging messages with higher priority `NOTICE` or higher are shown. Using the command line parameter `verbose` also adds `INFO` messages and `debug` adds `DEBUG` messages. You can select the exact logging level in your config file (see also [configuration](#configuration)).
 
