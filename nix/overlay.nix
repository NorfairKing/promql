final: prev:
{
  # Composed with override rather than extend, because extend hands back a
  # package set without an override of its own and every overlay after this
  # one asks for it.
  haskellPackages = prev.haskellPackages.override (old: {
    overrides = final.lib.composeExtensions (old.overrides or (_: _: { })) (self: _:
      let
        # buildFromSdist plus failOnAllWarnings.  The sdist is what a release
        # of this is, so building from one is how we find out it is complete,
        # and the warning floor is here rather than only in each package's
        # ghc-options so that one of them losing -Werror cannot quietly take
        # it away.
        strictly = final.haskell.lib.buildStrictly;
      in
      {
        promql = strictly (self.callPackage ../promql { });
        promql-gen = strictly (self.callPackage ../promql-gen { });
        # The one package built differently, and only because its suite asks a
        # Prometheus that nothing outside this repository's own check starts.
        promql-e2e = final.haskell.lib.dontCheck (strictly (self.callPackage ../promql-e2e { }));
      });
  });
}
