{
  description = "Voxel - Library for voxel grids and mesh generation";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    GPipe-Core = {
      url = "github:NCrashed/GPipe-Core/7ab6ef7d453aa930298f1466d0c0ffbd87280b9f";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, GPipe-Core }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        lib = pkgs.lib;

        haskellPackages = pkgs.haskell.packages.ghc9141.override {
          overrides = final: prev: {
            # Custom GPipe from fork
            GPipe = final.callCabal2nix "GPipe" "${GPipe-Core}/GPipe-Core" {};

            # Local packages
            GPipe-GLFW = final.callCabal2nix "GPipe-GLFW" ./GPipe-GLFW {};
            MagicaVoxel-vox = final.callCabal2nix "MagicaVoxel-vox" ./MagicaVoxel-vox {};
            voxel = final.callCabal2nix "voxel" ./voxel {};
            voxel-app = final.callCabal2nix "voxel-app" ./voxel-app {};
            voxel-GPipe = final.callCabal2nix "voxel-GPipe" ./voxel-GPipe {};
            voxel-MagicaVoxel = final.callCabal2nix "voxel-MagicaVoxel" ./voxel-MagicaVoxel {};
            voxel-render = final.callCabal2nix "voxel-render" ./voxel-render {};
            voxel-viewer = final.callCabal2nix "voxel-viewer" ./voxel-viewer {};
            voxel-ui = final.callCabal2nix "voxel-ui" ./voxel-ui {};
            voxel-example-ui = final.callCabal2nix "voxel-example-ui" ./examples/ui {};
            voxel-gameloop = final.callCabal2nix "voxel-gameloop" ./examples/gameloop {};
            voxel-atlas-demo = final.callCabal2nix "voxel-atlas-demo" ./examples/atlas-demo {};
            voxel-pixel-art-demo = final.callCabal2nix "voxel-pixel-art-demo" ./examples/pixel-art-demo {};

            # Jailbreak packages with overly restrictive bounds for GHC 9.14
            assoc = pkgs.haskell.lib.doJailbreak prev.assoc;
            bifunctors = pkgs.haskell.lib.doJailbreak prev.bifunctors;
            these = pkgs.haskell.lib.doJailbreak prev.these;
            strict = pkgs.haskell.lib.doJailbreak prev.strict;
            lens = pkgs.haskell.lib.doJailbreak prev.lens;
            semigroupoids = pkgs.haskell.lib.doJailbreak prev.semigroupoids;
            free = pkgs.haskell.lib.doJailbreak prev.free;
            comonad = pkgs.haskell.lib.doJailbreak prev.comonad;
            distributive = pkgs.haskell.lib.doJailbreak prev.distributive;
            indexed-traversable = pkgs.haskell.lib.doJailbreak prev.indexed-traversable;
            indexed-traversable-instances = pkgs.haskell.lib.doJailbreak prev.indexed-traversable-instances;
            witherable = pkgs.haskell.lib.doJailbreak prev.witherable;
            semialign = pkgs.haskell.lib.doJailbreak prev.semialign;
            kan-extensions = pkgs.haskell.lib.doJailbreak prev.kan-extensions;
            adjunctions = pkgs.haskell.lib.doJailbreak prev.adjunctions;
            profunctors = pkgs.haskell.lib.doJailbreak prev.profunctors;
            invariant = pkgs.haskell.lib.doJailbreak prev.invariant;
            data-default = pkgs.haskell.lib.doJailbreak prev.data-default;
            data-default-class = pkgs.haskell.lib.doJailbreak prev.data-default-class;
            vector-th-unbox = pkgs.haskell.lib.doJailbreak prev.vector-th-unbox;
            reflex = pkgs.haskell.lib.doJailbreak prev.reflex;
            Noise = pkgs.haskell.lib.doJailbreak prev.Noise;
            some = pkgs.haskell.lib.doJailbreak prev.some;
            dependent-sum = pkgs.haskell.lib.doJailbreak prev.dependent-sum;
            dependent-map = pkgs.haskell.lib.doJailbreak prev.dependent-map;
            constraints-extras = pkgs.haskell.lib.doJailbreak prev.constraints-extras;
            patch = pkgs.haskell.lib.doJailbreak prev.patch;
            monoidal-containers = pkgs.haskell.lib.doJailbreak prev.monoidal-containers;
            th-abstraction = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.doJailbreak prev.th-abstraction);
            th-lift = pkgs.haskell.lib.doJailbreak prev.th-lift;
            th-expand-syns = pkgs.haskell.lib.doJailbreak prev.th-expand-syns;
            th-orphans = pkgs.haskell.lib.doJailbreak prev.th-orphans;
            th-reify-many = pkgs.haskell.lib.doJailbreak prev.th-reify-many;
            th-compat = pkgs.haskell.lib.doJailbreak prev.th-compat;
            aeson = pkgs.haskell.lib.doJailbreak prev.aeson;
            generic-deriving = pkgs.haskell.lib.doJailbreak prev.generic-deriving;
            haskell-src-meta = pkgs.haskell.lib.doJailbreak prev.haskell-src-meta;
            haskell-src-exts = pkgs.haskell.lib.doJailbreak prev.haskell-src-exts;
            parallel = pkgs.haskell.lib.doJailbreak prev.parallel;
            tagged = pkgs.haskell.lib.doJailbreak prev.tagged;
            linear = pkgs.haskell.lib.doJailbreak prev.linear;
            bytes = pkgs.haskell.lib.doJailbreak prev.bytes;
            tasty = pkgs.haskell.lib.doJailbreak prev.tasty;
            tasty-quickcheck = pkgs.haskell.lib.doJailbreak prev.tasty-quickcheck;
            quickcheck-instances = pkgs.haskell.lib.doJailbreak prev.quickcheck-instances;
            boring = pkgs.haskell.lib.doJailbreak prev.boring;
            test-framework = pkgs.haskell.lib.doJailbreak prev.test-framework;
            test-framework-hunit = pkgs.haskell.lib.doJailbreak prev.test-framework-hunit;
            test-framework-quickcheck2 = pkgs.haskell.lib.doJailbreak prev.test-framework-quickcheck2;
            async = pkgs.haskell.lib.doJailbreak prev.async;
            cereal = pkgs.haskell.lib.doJailbreak prev.cereal;
            case-insensitive = pkgs.haskell.lib.doJailbreak prev.case-insensitive;
            exception-transformers = pkgs.haskell.lib.doJailbreak prev.exception-transformers;
            Diff = pkgs.haskell.lib.doJailbreak prev.Diff;
            Glob = pkgs.haskell.lib.doJailbreak prev.Glob;
            inspection-testing = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.doJailbreak prev.inspection-testing);
            vector = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.doJailbreak prev.vector);
            generically = pkgs.haskell.lib.doJailbreak prev.generically;
            integer-conversion = pkgs.haskell.lib.doJailbreak prev.integer-conversion;
            text-iso8601 = pkgs.haskell.lib.doJailbreak prev.text-iso8601;
            ChasingBottoms = pkgs.haskell.lib.doJailbreak prev.ChasingBottoms;
            unordered-containers = pkgs.haskell.lib.doJailbreak prev.unordered-containers;
            hashable = pkgs.haskell.lib.doJailbreak prev.hashable;
            # doctest requires ghc <9.14 - mark broken and disable tests for packages using it
            doctest = null;
            foldl = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.doJailbreak prev.foldl);
            generic-lens = pkgs.haskell.lib.dontCheck prev.generic-lens;
            pcg-random = pkgs.haskell.lib.dontCheck prev.pcg-random;
            relude = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.doJailbreak prev.relude);
            trial = pkgs.haskell.lib.dontCheck prev.trial;
            validation-selective = pkgs.haskell.lib.dontCheck prev.validation-selective;
            hw-fingertree = pkgs.haskell.lib.dontCheck prev.hw-fingertree;
            slist = pkgs.haskell.lib.dontCheck prev.slist;
            trial-optparse-applicative = pkgs.haskell.lib.doJailbreak prev.trial-optparse-applicative;
            lucid = pkgs.haskell.lib.doJailbreak prev.lucid;
            megaparsec = pkgs.haskell.lib.doJailbreak prev.megaparsec;
            tomland = pkgs.haskell.lib.doJailbreak prev.tomland;
            row-types = pkgs.haskell.lib.dontCheck prev.row-types;
            config-ini = pkgs.haskell.lib.doJailbreak prev.config-ini;
            string-interpolate = pkgs.haskell.lib.doJailbreak prev.string-interpolate;
            brick = pkgs.haskell.lib.doJailbreak prev.brick;
            algebraic-graphs = pkgs.haskell.lib.doJailbreak prev.algebraic-graphs;
            Cabal-syntax = pkgs.haskell.lib.overrideCabal
              (pkgs.haskell.lib.doJailbreak prev.Cabal-syntax) (old: {
              postPatch = (old.postPatch or "") + ''
                sed -i 's/time >=1\.4\.0\.1 && <1\.15/time >=1.4.0.1/' Cabal-syntax.cabal
              '';
            });
            data-fix = pkgs.haskell.lib.doJailbreak prev.data-fix;
            OneTuple = pkgs.haskell.lib.doJailbreak prev.OneTuple;
            uuid-types = pkgs.haskell.lib.doJailbreak prev.uuid-types;
            scientific = pkgs.haskell.lib.doJailbreak prev.scientific;
            text-short = pkgs.haskell.lib.doJailbreak prev.text-short;
            time-compat = pkgs.haskell.lib.doJailbreak prev.time-compat;
            lifted-async = pkgs.haskell.lib.doJailbreak prev.lifted-async;
            lifted-base = pkgs.haskell.lib.doJailbreak prev.lifted-base;
            nothunks = pkgs.haskell.lib.doJailbreak prev.nothunks;
            hedgehog = pkgs.haskell.lib.doJailbreak prev.hedgehog;
            barbies = pkgs.haskell.lib.doJailbreak prev.barbies;
            constraints = pkgs.haskell.lib.doJailbreak prev.constraints;
            integer-logarithms = pkgs.haskell.lib.overrideCabal
              (pkgs.haskell.lib.doJailbreak prev.integer-logarithms) (old: {
              postPatch = (old.postPatch or "") + ''
                sed -i 's/ghc-bignum >=1\.0 && <1\.4/ghc-bignum >=1.0/' integer-logarithms.cabal
              '';
            });
            attoparsec = pkgs.haskell.lib.doJailbreak prev.attoparsec;
            binary-orphans = pkgs.haskell.lib.doJailbreak prev.binary-orphans;
            tasty-hspec = pkgs.haskell.lib.doJailbreak prev.tasty-hspec;
            tasty-hedgehog = pkgs.haskell.lib.doJailbreak prev.tasty-hedgehog;
            JuicyPixels = pkgs.haskell.lib.doJailbreak prev.JuicyPixels;
            these-lens = pkgs.haskell.lib.doJailbreak prev.these-lens;

            # HLS and dev tool dependencies - use lib.optionalAttrs to handle missing packages
            colourista = pkgs.haskell.lib.doJailbreak prev.colourista;
            unliftio = pkgs.haskell.lib.doJailbreak prev.unliftio;
            unliftio-core = pkgs.haskell.lib.doJailbreak prev.unliftio-core;
            optparse-applicative = pkgs.haskell.lib.doJailbreak prev.optparse-applicative;
            prettyprinter = pkgs.haskell.lib.doJailbreak prev.prettyprinter;
            prettyprinter-ansi-terminal = pkgs.haskell.lib.doJailbreak prev.prettyprinter-ansi-terminal;
            ansi-terminal = pkgs.haskell.lib.doJailbreak prev.ansi-terminal;
            ansi-terminal-types = pkgs.haskell.lib.doJailbreak prev.ansi-terminal-types;
            regex-base = pkgs.haskell.lib.doJailbreak prev.regex-base;
            regex-tdfa = pkgs.haskell.lib.doJailbreak prev.regex-tdfa;
            QuickCheck = pkgs.haskell.lib.doJailbreak prev.QuickCheck;
            splitmix = pkgs.haskell.lib.doJailbreak prev.splitmix;
            random = pkgs.haskell.lib.doJailbreak prev.random;
            primitive = pkgs.haskell.lib.doJailbreak prev.primitive;
            microlens = pkgs.haskell.lib.doJailbreak prev.microlens;
            microlens-th = pkgs.haskell.lib.doJailbreak prev.microlens-th;
            microlens-mtl = pkgs.haskell.lib.doJailbreak prev.microlens-mtl;
            mono-traversable = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.doJailbreak prev.mono-traversable);
            conduit = pkgs.haskell.lib.doJailbreak prev.conduit;
            resourcet = pkgs.haskell.lib.doJailbreak prev.resourcet;
            typed-process = pkgs.haskell.lib.doJailbreak prev.typed-process;
            extra = pkgs.haskell.lib.doJailbreak prev.extra;
            hiedb = pkgs.haskell.lib.doJailbreak prev.hiedb;
            sqlite-simple = pkgs.haskell.lib.doJailbreak prev.sqlite-simple;
            direct-sqlite = pkgs.haskell.lib.doJailbreak prev.direct-sqlite;
            filepattern = pkgs.haskell.lib.doJailbreak prev.filepattern;
            cryptohash-sha256 = pkgs.haskell.lib.doJailbreak prev.cryptohash-sha256;
            lukko = pkgs.haskell.lib.doJailbreak prev.lukko;
            tar = pkgs.haskell.lib.doJailbreak prev.tar;
            hackage-security = pkgs.haskell.lib.doJailbreak prev.hackage-security;
            edit-distance = pkgs.haskell.lib.doJailbreak prev.edit-distance;
            HTTP = pkgs.haskell.lib.doJailbreak prev.HTTP;
            network = pkgs.haskell.lib.doJailbreak prev.network;
            network-uri = pkgs.haskell.lib.doJailbreak prev.network-uri;
            http-types = pkgs.haskell.lib.doJailbreak prev.http-types;
            colour = pkgs.haskell.lib.doJailbreak prev.colour;
            clock = pkgs.haskell.lib.doJailbreak prev.clock;
            ghcid = pkgs.haskell.lib.doJailbreak prev.ghcid;
            fsnotify = pkgs.haskell.lib.doJailbreak prev.fsnotify;
            hinotify = pkgs.haskell.lib.doJailbreak prev.hinotify;
            unix-compat = pkgs.haskell.lib.doJailbreak prev.unix-compat;
            terminal-size = pkgs.haskell.lib.doJailbreak prev.terminal-size;
            streaming-commons = pkgs.haskell.lib.doJailbreak prev.streaming-commons;
            zlib = pkgs.haskell.lib.doJailbreak prev.zlib;
            digest = pkgs.haskell.lib.doJailbreak prev.digest;
            base16-bytestring = pkgs.haskell.lib.doJailbreak prev.base16-bytestring;
            base64-bytestring = pkgs.haskell.lib.doJailbreak prev.base64-bytestring;
            cryptonite = pkgs.haskell.lib.doJailbreak prev.cryptonite;
            memory = pkgs.haskell.lib.doJailbreak prev.memory;
            basement = pkgs.haskell.lib.doJailbreak prev.basement;
            HUnit = pkgs.haskell.lib.doJailbreak prev.HUnit;
            call-stack = pkgs.haskell.lib.doJailbreak prev.call-stack;
            hspec = pkgs.haskell.lib.doJailbreak prev.hspec;
            hspec-core = pkgs.haskell.lib.doJailbreak prev.hspec-core;
            hspec-discover = pkgs.haskell.lib.doJailbreak prev.hspec-discover;
            hspec-expectations = pkgs.haskell.lib.doJailbreak prev.hspec-expectations;
            lsp = pkgs.haskell.lib.doJailbreak prev.lsp;
            lsp-types = pkgs.haskell.lib.doJailbreak prev.lsp-types;
            sorted-list = pkgs.haskell.lib.doJailbreak prev.sorted-list;
            co-log-core = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.doJailbreak prev.co-log-core);
            parsers = pkgs.haskell.lib.doJailbreak prev.parsers;
            charset = pkgs.haskell.lib.doJailbreak prev.charset;
            trifecta = pkgs.haskell.lib.doJailbreak prev.trifecta;
            fingertree = pkgs.haskell.lib.doJailbreak prev.fingertree;
            reducers = pkgs.haskell.lib.doJailbreak prev.reducers;
            utf8-string = pkgs.haskell.lib.doJailbreak prev.utf8-string;
            syb = pkgs.haskell.lib.doJailbreak prev.syb;
            uniplate = pkgs.haskell.lib.doJailbreak prev.uniplate;
            data-default-instances-containers = pkgs.haskell.lib.doJailbreak prev.data-default-instances-containers;
            data-default-instances-dlist = pkgs.haskell.lib.doJailbreak prev.data-default-instances-dlist;
            data-default-instances-old-locale = pkgs.haskell.lib.doJailbreak prev.data-default-instances-old-locale;
            dlist = pkgs.haskell.lib.doJailbreak prev.dlist;
            old-locale = pkgs.haskell.lib.doJailbreak prev.old-locale;
            old-time = pkgs.haskell.lib.doJailbreak prev.old-time;
            regex-posix = pkgs.haskell.lib.doJailbreak prev.regex-posix;
            safe-exceptions = pkgs.haskell.lib.doJailbreak prev.safe-exceptions;
            happy = pkgs.haskell.lib.doJailbreak prev.happy;
            alex = pkgs.haskell.lib.doJailbreak prev.alex;
            implicit-hie = pkgs.haskell.lib.doJailbreak prev.implicit-hie;
            implicit-hie-cradle = pkgs.haskell.lib.doJailbreak prev.implicit-hie-cradle;
            ghc-exactprint = pkgs.haskell.lib.doJailbreak prev.ghc-exactprint;
            ormolu = pkgs.haskell.lib.doJailbreak prev.ormolu;
            apply-refact = pkgs.haskell.lib.doJailbreak prev.apply-refact;
            retrie = pkgs.haskell.lib.doJailbreak prev.retrie;
            ghc-trace-events = pkgs.haskell.lib.doJailbreak prev.ghc-trace-events;
            dec = pkgs.haskell.lib.doJailbreak prev.dec;
            hie-compat = pkgs.haskell.lib.doJailbreak prev.hie-compat;
            regex-pcre-builtin = pkgs.haskell.lib.doJailbreak prev.regex-pcre-builtin;
            singleton-bool = pkgs.haskell.lib.doJailbreak prev.singleton-bool;
            clay = pkgs.haskell.lib.doJailbreak prev.clay;
            blaze-markup = pkgs.haskell.lib.doJailbreak prev.blaze-markup;
            blaze-html = pkgs.haskell.lib.doJailbreak prev.blaze-html;
          };
        };

        # Runtime library path for OpenGL
        runtimeLibs = with pkgs; [
          libGL
          libGLU
          glfw
          libX11
          libXcursor
          libXi
          libXinerama
          libXrandr
          libXxf86vm
        ];

      in {
        packages = {
          default = haskellPackages.voxel-viewer;
          voxel = haskellPackages.voxel;
          voxel-viewer = haskellPackages.voxel-viewer;
          voxel-render = haskellPackages.voxel-render;
          voxel-app = haskellPackages.voxel-app;
          voxel-ui = haskellPackages.voxel-ui;
          voxel-GPipe = haskellPackages.voxel-GPipe;
          voxel-MagicaVoxel = haskellPackages.voxel-MagicaVoxel;
          MagicaVoxel-vox = haskellPackages.MagicaVoxel-vox;
          GPipe = haskellPackages.GPipe;
          GPipe-GLFW = haskellPackages.GPipe-GLFW;
          gameloop = haskellPackages.voxel-gameloop;
          ui-example = haskellPackages.voxel-example-ui;
          atlas-demo = haskellPackages.voxel-atlas-demo;
          pixel-art-demo = haskellPackages.voxel-pixel-art-demo;
        };

        devShells.default = haskellPackages.shellFor {
          packages = p: [
            p.voxel
            p.voxel-app
            p.voxel-GPipe
            p.voxel-MagicaVoxel
            p.voxel-render
            p.voxel-viewer
            p.voxel-ui
            p.voxel-example-ui
            p.voxel-gameloop
            p.voxel-atlas-demo
            p.voxel-pixel-art-demo
            p.GPipe-GLFW
            p.MagicaVoxel-vox
          ];

          nativeBuildInputs = with pkgs; [
            cabal-install  # Use system cabal-install instead of haskellPackages version
            haskellPackages.ghcid
            # HLS not available for GHC 9.14 yet (ghc-exactprint and other packages need updates)
            pkg-config
            zlib
          ] ++ runtimeLibs;

          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath runtimeLibs;
        };

        apps.default = {
          type = "app";
          program = "${self.packages.${system}.voxel-viewer}/bin/voxel-viewer";
        };
      }
    );
}
