PACKAGE=${1:-voxel-ui}
ghcid -c "cabal new-repl --ghc-options='-Wall' $PACKAGE "
