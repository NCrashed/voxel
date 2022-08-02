PACKAGE=${1:-voxel-gameloop}
ghcid -c "cabal new-repl --ghc-options='-Wall' $PACKAGE "
