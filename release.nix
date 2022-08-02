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
    fir = new.callPackage ./derivations/fir.nix {};
    vector-sized = new.callPackage ./derivations/vector-sized.nix {};
    GPipe = new.callPackage ./derivations/GPipe.nix {};
    GPipe-GLFW = new.callCabal2nix "GPipe-GLFW" (ignore ./GPipe-GLFW) {};
    MagicaVoxel-vox = new.callCabal2nix "MagicaVoxel-vox" (ignore ./MagicaVoxel-vox) {};
    voxel = new.callCabal2nix "voxel" (ignore ./voxel) {};
    voxel-app = new.callCabal2nix "voxel-app" (ignore ./voxel-app) {};
    voxel-fir = new.callCabal2nix "voxel-fir" (ignore ./voxel-fir) {};
    voxel-GPipe = new.callCabal2nix "voxel-GPipe" (ignore ./voxel-GPipe) {};
    voxel-MagicaVoxel = new.callCabal2nix "voxel-MagicaVoxel" (ignore ./voxel-MagicaVoxel) {};
    voxel-viewer = new.callCabal2nix "voxel-viewer" (ignore ./voxel-viewer) {};
    voxel-gameloop = new.callCabal2nix "voxel-gameloop" (ignore ./examples/gameloop) {};
  };
in {
  inherit pkgs;
  packages = { inherit (pkgs.haskellPackages) voxel voxel-app voxel-fir voxel-GPipe voxel-MagicaVoxel voxel-viewer MagicaVoxel-vox voxel-gameloop; };
}
