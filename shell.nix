let
  release = import ./release.nix;
  pkgs = release.pkgs;
in pkgs.haskellPackages.shellFor {
  nativeBuildInputs = with pkgs.haskellPackages; [
    # Developing tools
    cabal-install
    ghcid
    haskell-language-server

    # GLFW and OpenGL
    pkgs.glfw
    pkgs.libGL
    pkgs.libGLU
    pkgs.libX11
    pkgs.libXcursor
    pkgs.libXi
    pkgs.libXinerama
    pkgs.libXrandr
    pkgs.libXxf86vm
    pkgs.libXext

    # Build dependencies
    pkgs.pkg-config
    pkgs.zlib
  ];
  packages = _: pkgs.lib.attrValues release.packages;

  # Runtime paths for finding OpenGL
  LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [
    pkgs.libGL
    pkgs.libGLU
    pkgs.glfw
    pkgs.libX11
    pkgs.libXcursor
    pkgs.libXi
    pkgs.libXinerama
    pkgs.libXrandr
    pkgs.libXxf86vm
  ];
}
