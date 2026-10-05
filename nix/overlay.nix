final: prev:
{
  # Composed with override rather than extend, because extend hands back a
  # package set without an override of its own and every overlay after this
  # one asks for it.
  haskellPackages = prev.haskellPackages.override (old: {
    overrides = final.lib.composeExtensions (old.overrides or (_: _: { })) (self: _: {
      promql = self.callPackage ../promql { };
      promql-gen = self.callPackage ../promql-gen { };
      # The only override of a package, and only because the suite in this one
      # asks a Prometheus that nothing outside this repository's own check
      # starts.  Everything else is a default build: how a package is built is
      # the business of whoever is building it.
      promql-e2e = final.haskell.lib.dontCheck (self.callPackage ../promql-e2e { });
    });
  });
}
