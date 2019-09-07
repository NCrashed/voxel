# voxel

Unfinished library to work with voxel grids:

* Data types to store voxels;
* Generation of chunked meshes from grids;
* Fast update of poly grids when voxel grid is modified.

# Compile

* You need [nix](https://nixos.org/nix/)

* Enter shell with `nix-shell`

* Now you can use ordinary cabal to build and test. `cabal new-build` or
`cabal new-run voxel-viewer`.

* If you get segmentation fault on non NixOS. Try [nixGL](https://github.com/guibou/nixGL)
