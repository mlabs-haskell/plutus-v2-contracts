{
  description = "plutus-v2-contracts";

  inputs = {
    haskell-nix.url = "github:mlabs-haskell/haskell.nix";

    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";

    iohk-nix.url = "github:input-output-hk/iohk-nix";
    iohk-nix.flake = false; # Bad Nix code

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    # all inputs below here are for pinning with haskell.nix
    cardano-node = {
      url =
        "github:input-output-hk/cardano-node/d0b0fa10ac83dc1d0d40aaf294cfded1b455a0a4";
      flake = false;
    };
    cardano-base = {
      url =
        "github:input-output-hk/cardano-base/631cb6cf1fa01ab346233b610a38b3b4cba6e6ab";
      flake = false;
    };
    cardano-crypto = {
      url =
        "github:input-output-hk/cardano-crypto/f73079303f663e028288f9f4a9e08bcca39a923e";
      flake = false;
    };
    cardano-ledger = {
      url =
        "github:input-output-hk/cardano-ledger/c415253fff9a5ce9d7575230fc9098bcfee97653";
      flake = false;
    };
    cardano-prelude = {
      url =
        "github:input-output-hk/cardano-prelude/bb4ed71ba8e587f672d06edf9d2e376f4b055555";
      flake = false;
    };
    ekg-forward = {
      url = "github:input-output-hk/ekg-forward/297cd9db5074339a2fb2e5ae7d0780debb670c63";
      flake = false;
    };
    ekg-json = {
      url = "github:input-output-hk/ekg-json/00ebe7211c981686e65730b7144fbf5350462608";
      flake = false;
    };
    flat = {
      url =
        "github:input-output-hk/flat/ee59880f47ab835dbd73bea0847dab7869fc20d8";
      flake = false;
    };
    goblins = {
      url =
        "github:input-output-hk/goblins/cde90a2b27f79187ca8310b6549331e59595e7ba";
      flake = false;
    };
    hedgehog-extras = {
      url =
        "github:vshabanov/hedgehog-extras/ee59880f47ab835dbd73bea0847dab7869fc20d8";
      flake = false;
    };
    iohk-monitoring-framework = {
      url =
        "github:input-output-hk/iohk-monitoring-framework/eb7854d1337637b8672af1227b276aa33a658f47";
      flake = false;
    };
    io-sim = {
      url =
        "github:input-output-hk/io-sim/606de33fa2f467d108fb1efb86daeb3348bf34e3";
      flake = false;
    };
    optparse-applicative = {
      url =
        "github:input-output-hk/optparse-applicative/7497a29cb998721a9068d5725d49461f2bba0e7a";
      flake = false;
    };
    ouroboros-network = {
      url =
        "github:input-output-hk/ouroboros-network/ea202b7b21983140d1944ccfde8750b891b59699";
      flake = false;
    };
    typed-protocols = {
      url = "github:input-output-hk/typed-protocols/91c3fba44d68439df207796171cd6f867354b76b";
      flake = false;
    };
    plutus = {
      url =
        "github:input-output-hk/plutus/fec94223a985e34d3b270460c8f150002f41b85b";
      flake = false;
    };
    Win32-network = {
      url =
        "github:input-output-hk/Win32-network/3825d3abf75f83f406c1f7161883c438dac7277d";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, haskell-nix, iohk-nix, ... }@inputs:
    let
      defaultSystems = [ "x86_64-linux" "x86_64-darwin" ];

      perSystem = nixpkgs.lib.genAttrs defaultSystems;

      nixpkgsFor = system:
        import nixpkgs {
          overlays = [ haskell-nix.overlay (import "${iohk-nix}/overlays/crypto") ];
          inherit (haskell-nix) config;
          inherit system;
        };
      nixpkgsFor' = system: import nixpkgs { inherit system; };


      haskellModules = [
        ({ pkgs, ... }:
          {
            packages = {
              marlowe.flags.defer-plugin-errors = true;
              plutus-use-cases.flags.defer-plugin-errors = true;
              plutus-ledger.flags.defer-plugin-errors = true;
              plutus-contract.flags.defer-plugin-errors = true;
              cardano-crypto-praos.components.library.pkgconfig = pkgs.lib.mkForce [ [ pkgs.libsodium-vrf ] ];
              cardano-crypto-class.components.library.pkgconfig = pkgs.lib.mkForce [ [ pkgs.libsodium-vrf ] ];
              cardano-wallet-core.components.library.build-tools = [
                pkgs.buildPackages.buildPackages.gitMinimal
              ];
              cardano-config.components.library.build-tools = [
                pkgs.buildPackages.buildPackages.gitMinimal
              ];
            };
          }
        )
      ];

      extraSources = [
        {
          src = inputs.cardano-base;
          subdirs = [
            "base-deriving-via"
            "binary"
            "binary/test"
            "cardano-crypto-class"
            "cardano-crypto-praos"
            "cardano-crypto-tests"
            "measures"
            "orphans-deriving-via"
            "slotting"
            "strict-containers"
          ];
        }
        {
          src = inputs.cardano-crypto;
          subdirs = [ "." ];
        }
        {
          src = inputs.cardano-ledger;
          subdirs = [
            "eras/alonzo/impl"
            "eras/alonzo/test-suite"
            "eras/babbage/impl"
            "eras/babbage/test-suite"
            "eras/byron/chain/executable-spec"
            "eras/byron/crypto"
            "eras/byron/crypto/test"
            "eras/byron/ledger/executable-spec"
            "eras/byron/ledger/impl"
            "eras/byron/ledger/impl/test"
            "eras/shelley/impl"
            "eras/shelley/test-suite"
            "eras/shelley-ma/impl"
            "eras/shelley-ma/test-suite"
            "libs/cardano-ledger-core"
            "libs/cardano-ledger-pretty"
            "libs/cardano-protocol-tpraos"
            "libs/cardano-data"
            "libs/vector-map"
            "libs/set-algebra"
            "libs/small-steps"
            "libs/small-steps-test"
            "libs/non-integral"
          ];
        }
        {
          src = inputs.cardano-node;
          subdirs = [
            "cardano-api"
            "cardano-cli"
            "cardano-git-rev"
            "cardano-node"
            "cardano-node-chairman"
            "trace-dispatcher"
            "trace-forward"
            "trace-resources"
          ];
        }
        {
          src = inputs.cardano-prelude;
          subdirs = [ "cardano-prelude" "cardano-prelude-test" ];
        }
        {
          src = inputs.ekg-json;
          subdirs = [ "." ];
        }
        {
          src = inputs.ekg-forward;
          subdirs = [ "." ];
        }
        {
          src = inputs.flat;
          subdirs = [ "." ];
        }
        {
          src = inputs.goblins;
          subdirs = [ "." ];
        }
        {
          src = inputs.hedgehog-extras;
          subdirs = [ "." ];
        }
        {
          src = inputs.iohk-monitoring-framework;
          subdirs = [
            "contra-tracer"
            "iohk-monitoring"
            "plugins/backend-aggregation"
            "plugins/backend-ekg"
            "plugins/backend-monitoring"
            "plugins/backend-trace-forwarder"
            "plugins/scribe-systemd"
            "tracer-transformers"
          ];
        }
        {
          src = inputs.io-sim;
          subdirs = [
            "io-classes"
            "io-sim"
            "strict-stm"
          ];
        }
        {
          src = inputs.optparse-applicative;
          subdirs = [ "." ];
        }
        {
          src = inputs.ouroboros-network;
          subdirs = [
            "monoidal-synchronisation"
            "network-mux"
            "ntp-client"
            "ouroboros-consensus"
            "ouroboros-consensus-byron"
            "ouroboros-consensus-cardano"
            "ouroboros-consensus-protocol"
            "ouroboros-consensus-shelley"
            "ouroboros-network"
            "ouroboros-network-framework"
            "ouroboros-network-testing"
          ];
        }
        {
          src = inputs.plutus;
          subdirs = [
            "plutus-core"
            "plutus-ledger-api"
            "plutus-tx"
            "plutus-tx-plugin"
            "prettyprinter-configurable"
            "stubs/plutus-ghc-stub"
            "word-array"
          ];
        }
        {
          src = inputs.typed-protocols;
          subdirs = [
            "typed-protocols"
            "typed-protocols-cborg"
            "typed-protocols-examples"
          ];
        }
        {
          src = inputs.Win32-network;
          subdirs = [ "." ];
        }
      ];

      projectFor = system:
        let
          pkgs = nixpkgsFor system;
          pkgs' = nixpkgsFor' system;
        in
        pkgs.haskell-nix.cabalProject' {
          src = ./.;
          inherit extraSources;
          name = "plutus-v2-contracts";
          compiler-nix-name = "ghc8107";
          shell = {
            withHoogle = true;
            tools.haskell-language-server = { };
            exactDeps = true;
            nativeBuildInputs = with pkgs'; [
              cabal-install
              haskellPackages.cabal-fmt
              haskellPackages.implicit-hie
              haskellPackages.fourmolu
              hlint
              fd
              nixpkgs-fmt
              nodejs
            ];
            additional = ps: [
              ps.cardano-node
              ps.plutus-tx-plugin
            ];
          };
          modules = haskellModules;
        };

      formatCheckFor = system:
        let
          pkgs = nixpkgsFor system;
        in
        pkgs.runCommand "format-check"
          { nativeBuildInputs = [ self.devShell.${system}.nativeBuildInputs ]; } ''
          cd ${self}
          export LC_CTYPE=C.UTF-8
          export LC_ALL=C.UTF-8
          export LANG=C.UTF-8
          export IN_NIX_SHELL='pure'
          make format_check cabalfmt_check nixpkgsfmt_check lint
          mkdir $out
        '';

    in
    {
      inherit extraSources haskellModules;

      project = perSystem projectFor;
      flake = perSystem (system: (projectFor system).flake { });

      defaultPackage = perSystem (system:
        let lib = "plutus-v2-contracts:exe:mk-plutus-v2-contracts";
        in self.flake.${system}.packages.${lib});

      packages = perSystem (system: self.flake.${system}.packages);

      apps = perSystem (system: self.flake.${system}.apps);

      devShell = perSystem (system: self.flake.${system}.devShell);

      # This will build all of the project's executables and the tests
      check = perSystem (system:
        (nixpkgsFor system).runCommand "combined-check"
          {
            nativeBuildInputs = builtins.attrValues self.checks.${system}
              ++ builtins.attrValues self.flake.${system}.packages
              ++ [ self.devShell.${system}.inputDerivation ];
          } "touch $out");
      # NOTE `nix flake check` will not work at the moment due to use of
      # IFD in haskell.nix
      checks = perSystem (system: self.flake.${system}.checks // {
        formatCheck = formatCheckFor system;
      });

      herculesCI.ciSystems = [ "x86_64-linux" ];
    };
}
