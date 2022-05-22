PACKAGE=${1:-voxel}
ghcid -c "cabal new-repl --ghc-options='-Wall' $PACKAGE "
