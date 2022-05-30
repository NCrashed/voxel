# voxel

Unfinished library to work with voxel grids:

* Data types to store voxels;
* Generation of chunked meshes from grids;
* Fast update of poly grids when voxel grid is modified.

# Compile

* You need [nix](https://nixos.org/nix/)

* Enter shell with `nix-shell`

* Now you can use ordinary cabal to build and test. `cabal new-build` or 
`cd voxel-viewer && cabal new-run`.

* If you get segmentation fault on non NixOS. Try [nixGL](https://github.com/guibou/nixGL)

![photo_2022-05-23_02-53-18](https://user-images.githubusercontent.com/2116167/169724414-07d17422-05aa-47d2-90d2-20d75a7b1e50.jpg)
 
