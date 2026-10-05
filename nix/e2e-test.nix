# A real Prometheus, started beside the suite, asked to parse every query the
# generators can write.
#
# Started in the build rather than in a virtual machine: nothing is scraped
# and nothing is stored, so what the suite needs is a parser on a socket, and
# a machine to boot would be most of the time this check takes.
{ prometheus
, promql-e2e
, haskell
}:
let
  # Prometheus will not start without a configuration file, even where nothing
  # is ever scraped.
  configFile = builtins.toFile "prometheus.yml" ''
    global:
      scrape_interval: 1h
    scrape_configs: []
  '';
in
haskell.lib.overrideCabal (haskell.lib.doCheck promql-e2e) (old: {
  testToolDepends = (old.testToolDepends or [ ]) ++ [ prometheus ];
  preCheck = (old.preCheck or "") + ''
    prometheusPort=9090
    prometheusStorage="$TMPDIR/prometheus"
    mkdir -p "$prometheusStorage"

    prometheus \
      --config.file=${configFile} \
      --storage.tsdb.path="$prometheusStorage" \
      --web.listen-address="127.0.0.1:$prometheusPort" \
      > "$TMPDIR/prometheus.log" 2>&1 &
    prometheusPid=$!

    # Waiting for it to answer rather than for a fixed time, which is either
    # longer than it needs to be or shorter than it sometimes needs.
    for _ in $(seq 1 60); do
      if promtool check healthy --url="http://127.0.0.1:$prometheusPort" 2>/dev/null; then
        break
      fi
      sleep 1
    done

    export PROMETHEUS_URL="http://127.0.0.1:$prometheusPort"
  '';
  postCheck = (old.postCheck or "") + ''
    kill "$prometheusPid" 2>/dev/null || true
  '';
})
