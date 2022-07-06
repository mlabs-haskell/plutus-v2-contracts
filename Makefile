.PHONY: build-nix hoogle nix-build-library nix-build-executables \
        nix-cabal-repl requires_nix_shell ci-build-run

# Starts a hoogle Server.
hoogle:
	@ nix develop -c hoogle server --local --port 8008

npm-install:
	npm i

# Run tests
run:
	echo "123456789" | cabal run mk-plutus-v2-contracts
	npm start

# Build the library with nix.
nix-build-library:
	@ nix build .#plutus-v2-contracts:lib:plutus-v2-contracts

current-system := $(shell nix eval --impure --expr builtins.currentSystem)

# Build the executables with nix (also builds the test suite).
nix-build-executables:
	@ nix build .#check.${current-system}

# Starts a ghci repl inside the nix environment.
nix-cabal-repl:
	@ nix develop -c cabal new-repl

# Target to use as dependency to fail if not inside nix-shell.
requires_nix_shell:
	@ [ "$(IN_NIX_SHELL)" ] || echo "The $(MAKECMDGOALS) target must be run from inside a nix shell"
	@ [ "$(IN_NIX_SHELL)" ] || (echo "    run 'nix develop' first" && false)

FOURMOLU_EXTENSIONS := -o -XTypeApplications -o -XTemplateHaskell -o -XImportQualifiedPost -o -XPatternSynonyms -o -fplugin=RecordDotPreprocessor

# Add folder locations to the list to be reformatted.
format:
	@ echo "> Formatting all .hs files"
	fourmolu $(FOURMOLU_EXTENSIONS) --mode inplace --check-idempotence $$(find src/  -iregex ".*.hs") $$(find app/  -iregex ".*.hs") 

format_check:
	@ echo "> Checking format of all .hs files"
	fourmolu $(FOURMOLU_EXTENSIONS) --mode check --check-idempotence $$(find src/  -iregex ".*.hs") $$(find app/  -iregex ".*.hs")

NIX_SOURCES := $(shell fd -enix)

nixpkgsfmt: requires_nix_shell
	nixpkgs-fmt $(NIX_SOURCES)

nixpkgsfmt_check: requires_nix_shell
	nixpkgs-fmt --check $(NIX_SOURCES)

CABAL_SOURCES := $(shell fd -ecabal)

cabalfmt: requires_nix_shell
	cabal-fmt --inplace $(CABAL_SOURCES)

cabalfmt_check: requires_nix_shell
	cabal-fmt --check $(CABAL_SOURCES)

lint: requires_nix_shell
	hlint $$(find src/  -iregex ".*.hs") $$(find test/ -iregex ".*.hs")
