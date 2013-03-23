# Lambda-Saturday 2013-03-23: Functional Reactive Programming

## How to get started

### Installing Haskell

Install the [Haskell Platform](http://www.haskell.org/platform/) and get
the latest repository information by running

    cabal update

### Getting the source

Clone the `lambda-frp` repo

    git clone git://github.com/leonidas/lambda-frp.git
    cd lambda-frp


### Using cabal

Cabal is used to install all the Haskell dependencies.

    cabal configure
    cabal install --only-dependencies
    cabal build


## Editing Haskell sources

In order to get proper auto-complete and other goodies, you should install [`ghc-mod`](http://www.mew.org/~kazu/proj/ghc-mod/en/).

    cabal install ghc-mod

The tool works with either vim, emacs or Sublime Text (via [SublimeHaskell](https://github.com/SublimeHaskell/SublimeHaskell)). For SublimeHaskell, you also need to run

    cabal install aeson haskell-src-exts haddock haskell-docs hdevtools
