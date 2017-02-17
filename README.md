# Marvin, the paranoid bot (⍺ stage)

[![Travis](https://travis-ci.org/JustusAdam/marvin.svg?branch=master)](https://travis-ci.org/JustusAdam/marvin)
[![Hackage](https://img.shields.io/hackage/v/marvin.svg)](http://hackage.haskell.org/package/marvin)

Marvin is an attempt to combine the ease of use of [hubot](https://hubot.github.com) with the typesafety and easy syntax of Haskell and the performance gains from compiled languages.

## Installing and using marvin

The verbose documentation can be found on readthedocs https://marvin.readthedocs.io.
It should hopefully answer all your questions.

Installation instructions are on [this](http://marvin.readthedocs.io/en/latest/getting-started.html) documentation page.

## Links

- [Documentation](https://marvin.readthedocs.io)
- [Hackage](https://hackage.haskell.org/package/marvin)
- [Repository](https://github.com/JustusAdam/marvin)
- [Bugtracker](https://github.com/JustusAdam/marvin/issues)
- [Documentation repository](https://github.com/JustusAdam/marvin-docs) and [bugtracker](https://github.com/JustusAdam/marvin-docs/issues)

## A teaser

```Haskell
module MyScript where

import Marvin.Prelude

script :: IsAdapter a => ScriptInit a
script = defineScript "my-script" $ do
    hear "sudo (.+)" $ do
        match <- getMatch

        reply $(isL "All right, i'll do #{match !! 1}")
    
    respond "repeat" $ do
        message <- getMessage

        send $(isL "You wrote #{message}")
    
    respond "what is in file (\\w+)\\??" $ do
        match <- getMatch 
        let file = match !! 1

        contents <- liftIO $ readFile file

        send contents
    
    enterIn "#random" $ do
        user <- getUser
        username <- getUsername user

        send $(isL "Hello #{username} welcome to the random channel!")
```

## Contributing

Any kind of contribution is very welcome.

### Issues and errors

If you are a marvin user, please report any error, issues, or improvement suggestions to the [issue section](https://github.com/JustusAdam/marvin/issues) or [write me an email](mailto:dev@justus.science).

### Testing

I welcome anybody who tests marvin by deploying it. 
Especially testing different adapters is a huge help.

### Hacking

If you want to hack on marvin, feel free to do so and send me your changes as pull requests.

Current hot places to get started:

- Convenient HTTP requests API in `Marvin.Util.HTTP`.
- Convenient JSON handling API in `Marvin.Util.JSON`.
- New adapters as submodules of `Marvin.Adapter`.
- A basic library of scripts (maybe as a `Marvin.Scripts.Prelude` module?) (inspiration: https://github.com/github/hubot-scripts)

