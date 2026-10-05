{
  description = "promql";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    validity.url = "github:NorfairKing/validity";
    validity.flake = false;
    safe-coloured-text.url = "github:NorfairKing/safe-coloured-text";
    safe-coloured-text.flake = false;
    fast-myers-diff.url = "github:NorfairKing/fast-myers-diff";
    fast-myers-diff.flake = false;
    sydtest.url = "github:NorfairKing/sydtest";
    sydtest.flake = false;
    opt-env-conf.url = "github:NorfairKing/opt-env-conf";
    opt-env-conf.flake = false;
    dekking.url = "github:NorfairKing/dekking";
    dekking.flake = false;
    weeder-nix.url = "github:NorfairKing/weeder-nix";
    weeder-nix.flake = false;
    hopinion.url = "github:NorfairKing/hopinion";
    hopinion.flake = false;
    release-to-hackage.url = "github:NorfairKing/release-to-hackage";
  };

  outputs =
    { self
    , nixpkgs
    , pre-commit-hooks
    , validity
    , safe-coloured-text
    , fast-myers-diff
    , sydtest
    , opt-env-conf
    , dekking
    , weeder-nix
    , hopinion
    , release-to-hackage
    }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        # These are all rights reserved, which nixpkgs reads as unfree and
        # will not build without being told it may.
        config.allowUnfree = true;
        overlays = [
          self.overlays.default
          (import (validity + "/nix/overlay.nix"))
          (import (safe-coloured-text + "/nix/overlay.nix"))
          (import (fast-myers-diff + "/nix/overlay.nix"))
          (import (sydtest + "/nix/overlay.nix"))
          (import (opt-env-conf + "/nix/overlay.nix"))
          (import (dekking + "/nix/overlay.nix"))
          (import (weeder-nix + "/nix/overlay.nix"))
          (import (hopinion + "/nix/overlay.nix"))
        ];
      };
      # The packages this repository has, which the checks below are made
      # one per.  A list rather than something read out of the overlay,
      # because the overlay is three callPackage lines and nothing else.
      packageNames = [ "promql" "promql-gen" "promql-e2e" ];
      promqlPackages =
        builtins.listToAttrs (map
          (name: { inherit name; value = pkgs.haskellPackages.${name}; })
          packageNames);
    in
    {
      overlays.default = import ./nix/overlay.nix;
      packages.${system} = {
        default = pkgs.haskellPackages.promql;
        # What a release of this is: everything but the harness.
        #
        # Named by what it leaves out rather than by what it takes in, so that
        # a package added later is released unless somebody says otherwise.
        # The other way round, a new one is left out until somebody remembers
        # a list over here, and nothing says they forgot.
        #
        # promql-e2e is what asks a Prometheus whether the other two are
        # right.  It is this repository's own business and nobody would depend
        # on it.
        release-to-hackage = release-to-hackage.lib.${system}.makeHackageRelease {
          packages = removeAttrs promqlPackages [ "promql-e2e" ];
        };
      } // promqlPackages;

      checks.${system} = {
        inherit (pkgs.haskellPackages) promql promql-gen promql-e2e;
        pre-commit = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            # Only formatters here; the checkers are separate checks with
            # properly filtered sources, so that a checker failing does not
            # look like a file needing to be rewritten.
            hpack.enable = true;
            ormolu.enable = true;
            nixpkgs-fmt.enable = true;
            nixpkgs-fmt.excludes = [ ".*/default.nix" ];
            cabal2nix.enable = true;
            tagref.enable = true;
          };
        };
        hlint-check =
          let
            mkHlintCheck = name:
              let
                src = pkgs.lib.cleanSourceWith {
                  src = ./${name};
                  filter = path: type:
                    type == "directory" || pkgs.lib.hasSuffix ".hs" (baseNameOf path);
                };
              in
              pkgs.runCommand "hlint-check-${name}"
                {
                  nativeBuildInputs = [ pkgs.haskellPackages.hlint ];
                } ''
                hlint --hint=${./.hlint.yaml} ${src}
                touch $out
              '';
          in
          pkgs.linkFarm "hlint-check" (builtins.listToAttrs (map
            (name: { name = "hlint-${name}"; value = mkHlintCheck name; })
            packageNames));
        statix-check = pkgs.runCommand "statix-check"
          {
            nativeBuildInputs = [ pkgs.statix ];
          } ''
          statix check ${
            pkgs.lib.cleanSourceWith {
              src = ./.;
              filter = path: type:
                (type == "directory" || pkgs.lib.hasSuffix ".nix" (baseNameOf path))
                && baseNameOf path != "default.nix";
            }
          }
          touch $out
        '';
        deadnix-check = pkgs.runCommand "deadnix-check"
          {
            nativeBuildInputs = [ pkgs.deadnix ];
          } ''
          deadnix --fail ${
            pkgs.lib.cleanSourceWith {
              src = ./.;
              filter = path: type:
                (type == "directory" || pkgs.lib.hasSuffix ".nix" (baseNameOf path))
                && baseNameOf path != "default.nix";
            }
          }
          touch $out
        '';
        # Roots are what the tests reach, which is why includeTests is on: for a
        # library the API is the thing to keep alive, and the suite exercises
        # all of it.  An export no test reaches is reported, which is the
        # report worth having.
        weeder-check = pkgs.weeder-nix.makeWeederCheck {
          weederToml = ./weeder.toml;
          packages = packageNames;
          includeTests = true;
        };
        hopinion = pkgs.hopinion.makeHopinionCheck {
          src = ./.;
          packages = packageNames;
        };
        coverage-report = pkgs.dekking.makeCoverageReport {
          name = "promql-coverage-report";
          packages = [ "promql" ];
          coverage = [ "promql-gen" ];
          # The renderer is what the suite is about, so anything much below
          # this means a case went in without a test.
          threshold = 80; # %
        };
        # Every mutation of the renderer should be caught: it is a pure
        # function over a small type, with a suite that asserts the text.
        mutation = pkgs.haskellPackages.sydtest.mutationCheck {
          name = "mutation-promql";
          configFile = ./mutation.yaml;
          libraries = [ "promql" ];
          tests = [ "promql-gen" ];
        };
        # The one check that asks Prometheus rather than us.  Here rather
        # than in the overlay: it is this repository's own harness, and
        # nothing importing the overlay should be handed it.
        e2e-test = pkgs.callPackage ./nix/e2e-test.nix {
          inherit (pkgs.haskellPackages) promql-e2e;
        };
      };

      devShells.${system}.default =
        let
          shellHaskellPackages = pkgs.haskellPackages;
        in
        shellHaskellPackages.shellFor {
          name = "promql-shell";
          packages = p: map (name: p.${name}) packageNames;
          withHoogle = true;
          buildInputs = with pkgs; [
            cabal-install
            deadnix
            haskellPackages.weeder
            pkgs.hopinion
            prometheus
            prometheus.cli
            statix
            zlib
          ] ++ self.checks.${system}.pre-commit.enabledPackages;
          shellHook = self.checks.${system}.pre-commit.shellHook;
        };
    };
}
