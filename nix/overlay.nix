final: prev:
with final.lib;
with final.haskell.lib;
{
  promqlRelease = final.symlinkJoin {
    name = "promql-release";
    paths = builtins.attrValues final.haskellPackages.promqlPackages;
  };

  # The end-to-end check: a real Prometheus, started here, asked to parse
  # every query the suite can generate.
  #
  # Prometheus itself rather than promtool, because promtool's formatter keeps
  # whatever brackets it was given and so cannot say whether two spellings
  # mean the same thing.  The parse endpoint answers with the tree.
  promqlE2ETest = final.callPackage ./e2e-test.nix {
    inherit (final.haskellPackages.promqlPackages) promql-e2e;
  };

  haskellPackages = prev.haskellPackages.override (old: {
    overrides = final.lib.composeExtensions (old.overrides or (_: _: { })) (self: _:
      let
        promqlPackages = {
          promql = promqlPkg "promql";
          promql-gen = promqlPkg "promql-gen";
          promql-e2e = promqlPkg "promql-e2e";
        };
        # buildStrictly is buildFromSdist plus failOnAllWarnings, so the warning
        # set each package states is backed by a floor here that one of them
        # losing -Werror cannot quietly remove.
        promqlPkg = name:
          buildStrictly (overrideCabal
            (self.callPackage (../${name}) { })
            (_: {
              doBenchmark = false;
              doHaddock = false;
              doCoverage = false;
              doHoogle = false;
              doCheck = false;
              hyperlinkSource = false;
              enableLibraryProfiling = false;
              enableExecutableProfiling = false;
            }));
      in
      {
        inherit promqlPackages;
      } // promqlPackages);
  });
}
