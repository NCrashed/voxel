PACKAGE=${1:-voxel-viewer}
ghcid -c "cabal new-repl --ghc-options='-Wall' $PACKAGE "
