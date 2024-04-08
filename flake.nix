{
  description = "purescript-cardano-types";

  # Allow IFD in `nix flake check`.
  nixConfig.allow-import-from-derivation = "true";

  nixConfig = {
    extra-substituters = [ "https://plutonomicon.cachix.org" ];
    extra-trusted-public-keys = [ "plutonomicon.cachix.org-1:evUxtNULjCjOipxwAnYhNFeF/lyYU1FeNGaVAnm+QQw=" ];
    bash-prompt = "\\[\\e[0m\\][\\[\\e[0;2m\\]nix-develop \\[\\e[0;1m\\]ps-cardano-types@\\[\\033[33m\\]$(git rev-parse --abbrev-ref HEAD) \\[\\e[0;32m\\]\\w\\[\\e[0m\\]]\\[\\e[0m\\]$ \\[\\e[0m\\]";
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url = github:hercules-ci/flake-parts;
    hercules-ci-effects.url = github:hercules-ci/hercules-ci-effects;
    easy-purescript-nix = {
      url = "github:justinwoo/easy-purescript-nix";
      flake = false;
    };
  };

  outputs = inputs @ { self, flake-parts, hercules-ci-effects, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } ({ ... }: {
      imports = [
        # Hercules CI effects module used to deploy to GitHub Pages
        hercules-ci-effects.flakeModule
      ];

      # Systems supported by this flake
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];

      perSystem = { self', pkgs, system, ... }:
        let
          easy-ps = (import inputs.easy-purescript-nix { inherit pkgs; });

          spagoPkgs = import ./spago-packages.nix { inherit pkgs; };

          # building/testing machinery is taken from cardano-transaction-lib.
          # TODO: migrate it to a separate repo

          # Compiles the dependencies of a Purescript project and copies the `output`
          # and `.spago` directories into the Nix store.
          # Intended to be used in `buildPursProject` to not recompile the entire
          # package set every time.
          buildPursDependencies =
            {
              # If warnings generated from project source files will trigger a build error.
              # Controls `--strict` purescript-psa flag
              strictComp ? true
              # Warnings from `purs` to silence during compilation, independent of `strictComp`
              # Controls `--censor-codes` purescript-psa flag
            , censorCodes ? [ "UserDefinedWarning" ]
            , ...
            }:
            pkgs.stdenv.mkDerivation {
              name = "ps-deps";
              buildInputs = [
              ];
              nativeBuildInputs = [
                spagoPkgs.installSpagoStyle
                easy-ps.psa
                easy-ps.purs
                easy-ps.spago
              ];
              # Make the derivation independent of the source files.
              # `src` is not needed
              unpackPhase = "true";
              buildPhase = ''
                install-spago-style
                psa ${pkgs.lib.optionalString strictComp "--strict" } \
                  --censor-lib \
                  --is-lib=.spago ".spago/*/*/src/**/*.purs" \
                  --censor-codes=${builtins.concatStringsSep "," censorCodes} \
                  -gsourcemaps,js
              '';
              installPhase = ''
                mkdir $out
                mv output $out/
                mv .spago $out/
              '';
            };

          nodejs = pkgs.nodejs-18_x;

          src = ./.;

          mkNodeEnv = { withDevDeps ? true }: import
            (pkgs.runCommand "node-packages"
              {
                buildInputs = [ pkgs.nodePackages.node2nix ];
              } ''
              mkdir $out
              cd $out
              cp ${src}/package-lock.json ./package-lock.json
              cp ${src}/package.json ./package.json
              node2nix ${pkgs.lib.optionalString withDevDeps "--development" } \
                --lock ./package-lock.json -i ./package.json
            '')
            { inherit pkgs nodejs system; };

          mkNodeModules = { withDevDeps ? true }:
            let
              nodeEnv = mkNodeEnv { inherit withDevDeps; };
              modules = pkgs.callPackage
                (_:
                  nodeEnv // {
                    shell = nodeEnv.shell.override {
                      # see https://github.com/svanderburg/node2nix/issues/198
                      buildInputs = [ pkgs.nodePackages.node-gyp-build ];
                    };
                  });
            in
            (modules { }).shell.nodeDependencies;

          nodeModules = mkNodeModules { };

          # Compiles your Purescript project and copies the `output` directory into the
          # Nix store. Also copies the local sources to be made available later as `purs`
          # does not include any external files to its `output` (if we attempted to refer
          # to absolute paths from the project-wide `src` argument, they would be wrong)
          buildPursProject =
            {
              # If warnings generated from project source files will trigger a build error.
              # Controls `--strict` purescript-psa flag
              strictComp ? true
              # Warnings from `purs` to silence during compilation, independent of `strictComp`
              # Controls `--censor-codes` purescript-psa flag
            , censorCodes ? [ "UserDefinedWarning" ]
            , pursDependencies ? buildPursDependencies {
                inherit strictComp censorCodes;
              }
            , ...
            }:
            pkgs.stdenv.mkDerivation {
              name = "ps-project";
              src = ./.;
              buildInputs = [
              ];
              nativeBuildInputs = [
                spagoPkgs.installSpagoStyle
                easy-ps.psa
                easy-ps.purs
                easy-ps.spago
              ];
              unpackPhase = ''
                export HOME="$TMP"

                # handle NodeJS deps
                export NODE_PATH="${nodeModules}/lib/node_modules"
                ln -sfn $NODE_PATH node_modules
                export PATH="${nodeModules}/bin:$PATH"

                # copy the dependency build artifacts and sources
                # preserve the modification date so that we don't rebuild them
                mkdir -p output .spago
                cp -rp ${pursDependencies}/.spago/* .spago
                cp -rp ${pursDependencies}/output/* output
                # note that we copy the entire source directory, not just $src/src,
                # because we need sources in ./examples and ./test
                cp -rp $src ./src

                # add write permissions for the PS compiler to use
                # `output/cache-db.json`
                chmod -R +w output/
              '';
              buildPhase = ''
                psa ${pkgs.lib.optionalString strictComp "--strict" } \
                  --censor-lib \
                  --is-lib=.spago ".spago/*/*/src/**/*.purs" \
                  --censor-codes=${builtins.concatStringsSep "," censorCodes} "./src/**/*.purs" \
                  -gsourcemaps,js
              '';
              # We also need to copy all of `src` here, since compiled modules in `output`
              # might refer to paths that will point to nothing if we use `src` directly
              # in other derivations (e.g. when using `fs.readFileSync` inside an FFI
              # module)
              installPhase = ''
                mkdir $out
                cp -r output $out/
              '';
            };

          # Runs a test written in Purescript using NodeJS.
          runPursTest =
            {
              # The main Purescript module
              testMain
              # The entry point function in the main PureScript module
            , psEntryPoint ? "main"
              # Additional variables to pass to the test environment
            , env ? { }
              # Passed through to the `buildInputs` of the derivation. Use this to add
              # additional packages to the test environment
            , buildInputs ? [ ]
            , builtProject ? buildPursProject { main = testMain; }
            , ...
            }: pkgs.runCommand "ps-test"
              (
                {
                  src = ./.;
                  buildInputs = [ pkgs.nodejs ];
                } // env
              )
              ''
                # Copy the purescript project files
                cp -r ${builtProject}/* .

                # Handle nodejs dependencies
                export NODE_PATH="${nodeModules}/lib/node_modules"
                ln -sfn $NODE_PATH node_modules
                export PATH="${nodeModules}/bin:$PATH"

                # The tests may depend on sources
                cp -r $src/* .

                # Call the main module and execute the entry point function
                node --enable-source-maps -e 'import("./output/${testMain}/index.js").then(m => m.${psEntryPoint}())'

                # Create output file to tell Nix we succeeded
                touch $out
              '';

        in
        {
          devShells = {
            default = pkgs.mkShell {
              shellHook = ''
                export NODE_PATH="${nodeModules}/lib/node_modules"
                ln -sfn $NODE_PATH node_modules
                export PATH="${nodeModules}/bin:$PATH"
                export NPM_CONFIG_PACKAGE_LOCK_ONLY=true
              '';
              buildInputs = with pkgs; [
                nixpkgs-fmt
                easy-ps.purs
                easy-ps.purs-tidy
                easy-ps.spago
                easy-ps.pscid
                easy-ps.psa
                easy-ps.spago2nix
                nodePackages.eslint
                nodePackages.prettier
                fd
                git
                nodejs-18_x
              ];
            };
          };

          packages = {
            # Example package. Build with `nix build` or `nix build .#myapp`.
            default = self'.packages.myapp;
            myapp = pkgs.writeShellScriptBin "myapp" ''
              echo "Hello, World!"
            '';
          };

          # Example flake checks. Run with `nix flake check --keep-going`
          checks = {
            tests = runPursTest { testMain = "Test.Main"; psEntryPoint = "main"; };

            formatting-check = pkgs.runCommand "formatting-check"
              {
                nativeBuildInputs = with pkgs; [
                  easy-ps.purs-tidy
                  nixpkgs-fmt
                  nodePackages.prettier
                  nodePackages.eslint
                  fd
                ];
              }
              ''
                cd ${self}
                purs-tidy check './src/**/*.purs' './test/**/*.purs'
                nixpkgs-fmt --check "$(fd --no-ignore-parent -enix --exclude='spago*')"
                prettier --log-level warn -c $(fd --no-ignore-parent -ejs -ecjs)
                eslint --quiet $(fd --no-ignore-parent -ejs -ecjs) --parser-options 'sourceType: module'
                touch $out
              '';
          };
        };

      # On CI, build only on available systems, to avoid errors about systems without agents.
      # Please use aarch64-linux and x86_64-darwin sparingly as they run on smaller hardware.
      herculesCI.ciSystems = [ "x86_64-linux" ];

      # Schedule task to run `nix flake update`, run CI, and open a PR with changes
      hercules-ci.flake-update = {
        enable = true;
        when = {
          dayOfWeek = "Sun";
          hour = 12;
          minute = 45;
        };
      };
    });
}
