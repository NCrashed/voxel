# To update nix-shell -p nix-prefetch-git --command 'nix-prefetch-git https://github.com/NixOS/nixpkgs'
import ((import <nixpkgs> {}).fetchFromGitHub {
  owner = "NixOS";
  repo = "nixpkgs";
  rev = "5aa13af2ee24976d157f6bc652c9f0f98f23b2c7";
  sha256  = "0g2xc6ljiq879mqlfba2yk2lfq7z548k4xd1xajz95xqr9fqkidk";
})
