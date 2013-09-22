elevators
=========

This is a toy program that I used to play with the Haskell gloss library.



To compile and run it, you need to first get the Haskell plaform, either from your package manager (e.g., `apt-get install haskell-platform`), or from [Haskell.org](http://www.haskell.org/platform/). Then do the following in your shell:

    $ git clone git://github.com/tdox/elevators
    $ cd elevators
    $ cabal configure
    $ cabal build
    $ dist/build/elevators/elevators test_input/in1.txt

