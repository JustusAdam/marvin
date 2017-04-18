# Contribution guidelines for marvin

When contributing a new feature please make sure you code satisfies the following checklist:

1. Haddocks

    Ensure all exported functions and data types have a short haddock documentation attached to them explaining what they do.

    Also if the internals of your code are not self explanatory add comments to enable others to reason about the code.

2. Proper docs

    Make sure your feature is properly verbosely documented in the [marvin docs](https://marvin.readthedocs.io).

    This should include:

    - How to use the feature, preferrably with a short example
    - Any caveats and to the feature, for instance unimplemented features in a new adapter
    - known bugs/issues

3. Style

    This is a minor point but try to follow the general code style of the repository, including running `stylish-haskell` on any newly submitted code.

4. Tests

    Try to add tests to the code.
    Unittests for pure code and integration tests for impure code.
    If there is no easy way to test your code semantically at least add some lines to the integration tests to ensure it compiles.

## Additional hints for special types of code

### Async

If your code includes asynchronous operations particularly those using producer/consumer model with `Mvar`s or `Chan`s make sure you use `Control.Concurrent.Async.Lifted.link` to propagate exceptions from the forked thread back to the main thread which ensures exceptions thrown by the forked thread appear in the log.
