let
  pkgs = import ./pkgs.nix { inherit config; };
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override { overrides = haskOverrides; };
    };
  };
  gitignore = pkgs.callPackage (pkgs.fetchFromGitHub {
    owner  = "siers";
    repo   = "nix-gitignore";
    rev    = "ce0778ddd8b1f5f92d26480c21706b51b1af9166";
    sha256 = "1d7ab78i2k13lffskb23x8b5h24x7wkdmpvmria1v3wb9pcpkg2w";
  }) {};
  ignore = gitignore.gitignoreSourceAux ''
    .stack-work
    dist
    dist-newstyle
    .ghc.environment*
    '';
  haskOverrides = new: old: rec {
    GPipe = new.callPackage ./derivations/GPipe.nix {};
    GPipe-GLFW = new.callCabal2nix "GPipe-GLFW" (ignore ./GPipe-GLFW) {};
    # MagicaVoxel-vox = new.callCabal2nix "MagicaVoxel-vox" (ignore ./MagicaVoxel-vox) {};
    # vector-sized = new.callPackage ./derivations/vector-sized.nix {};
    # voxel = new.callCabal2nix "voxel" (ignore ./voxel) {};
    # voxel-app = new.callCabal2nix "voxel-app" (ignore ./voxel-app) {};
    # voxel-gameloop = new.callCabal2nix "voxel-gameloop" (ignore ./examples/gameloop) {};
    # voxel-ui = new.callCabal2nix "voxel-ui" (ignore ./voxel-ui) {};
    # voxel-example-ui = new.callCabal2nix "voxel-example-ui" (ignore ./examples/ui) {};
    # voxel-GPipe = new.callCabal2nix "voxel-GPipe" (ignore ./voxel-GPipe) {};
    # voxel-MagicaVoxel = new.callCabal2nix "voxel-MagicaVoxel" (ignore ./voxel-MagicaVoxel) {};
    # voxel-render = new.callCabal2nix "voxel-render" (ignore ./voxel-render) {};
    # voxel-viewer = new.callCabal2nix "voxel-viewer" (ignore ./voxel-viewer) {};
  };
in {
  inherit pkgs;
  # packages = { inherit (pkgs.haskellPackages) voxel voxel-app voxel-GPipe voxel-MagicaVoxel voxel-render voxel-viewer MagicaVoxel-vox voxel-gameloop voxel-ui voxel-example-ui; };
  packages = { inherit (pkgs.haskellPackages) GPipe GPipe-GLFW; };
}
