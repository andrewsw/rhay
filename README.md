Just hacking around with a ray tracer in haskell.

Building
========

Building with ghc 7.4.1 using only standard libraries.

    ghc --make src/Main.hs -O2 -isrc -o rhay

will generate an executable, ./rhay, which will produce a test image.

