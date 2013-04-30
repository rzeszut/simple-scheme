simple-scheme
=============

A simple Scheme interpreter, written in Haskell.

To build:

    runhaskell Setup.hs configure --prefix=$HOME --user
    runhaskell Setup.hs build
    runhaskell Setup.hs install
    
To run:

    simple-scheme

or

    dist/build/simple-scheme/simple-scheme
