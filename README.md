simple-scheme
=============

A simple Scheme interpreter, written in Haskell.

To build:

    cabal configure --prefix=$HOME --user
    cabal build

To install 

    cabal install
    
To run (after installation):

    ~/.cabal/bin/simple-scheme

or

    dist/build/simple-scheme/simple-scheme

Prerequisites
-------------

You need to have cabal (and some libraries: parsec and mtl) installed:

    sudo apt-get install cabal-install
    cabal update
    cabal install mtl split array alex happy

Warning! GHC is in conflict with binutils-gold, don't install it, or you will get an error like that:

    /usr/bin/ld: --hash-size=31: unknown option
