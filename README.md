cabal-hoogle
=======
An experimental wrapper around [Hoogle](https://github.com/ndmitchell/hoogle) and Cabal to generate project specific indexes.

Inspired by [cabal-extras](https://github.com/phadej/cabal-extras) and intended to
fix this old Cabal Issue - [Automatic hoogle database for installed packages](https://github.com/haskell/cabal/issues/395).

While we are here we should fix, [Document --hoogle more thoroughly in the users guide](https://github.com/haskell/haddock/issues/807)

Usage
----------

``` shell
## Build
cabal build all

## Run and build index
cabal exec -- cabal-hoogle hoogle "fmap"

## Or cabal install and run the executable
```

All tools are highly experimental, although I (Tim McGilchrist) use them regularily.

Resources
----------

* [Haddock](https://haskell-haddock.readthedocs.io/en/latest/index.html) - Haskell documentation generation from source.
* [Cabal 3.4](https://cabal.readthedocs.io/en/3.4/index.html) - Haskell build/package management tool
* [State of Cabal 2021](https://discourse.haskell.org/t/state-of-the-cabal-q1-q2-2021/2548)
* [Standalone Haddock](https://github.com/ktvoelker/standalone-haddock)
