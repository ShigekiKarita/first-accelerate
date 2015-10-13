cabal update
cabal install --only-dependencies --enable-tests
cabal configure --enable-tests # --enable-coverage
cabal build
cabal test
