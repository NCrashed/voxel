let
  release = import ./release.nix;
  pkgs = release.pkgs;
in pkgs.haskellPackages.shellFor {
  nativeBuildInputs = with pkgs.haskellPackages; [
    cabal-install
    ghcid
    haskell-language-server
  ];
  packages = _: pkgs.lib.attrValues release.packages;
}
