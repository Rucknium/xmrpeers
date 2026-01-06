# Ping peer nodes for latency measurement

Ping peer nodes for latency measurement

## Usage

``` r
ping.peers(
  bitmonero.dir = "~/.bitmonero",
  output.file = "monero_peer_pings.csv",
  sleep = 10,
  ping.count = 5,
  threads = NULL
)
```

## Arguments

- bitmonero.dir:

  .bitmonero directory where the monero.log file is.

- output.file:

  Name of the output file. The file will be created in `bitmonero.dir`.

- sleep:

  Number of seconds to sleep between each round of collecting new peer
  IPs.

- ping.count:

  Number of times to ping each peer.

- threads:

  Override default number of threads for sending pings.

## Value

No return value. Executes in a loop until interrupted.

## Examples

``` r
if (FALSE) { # \dontrun{
ping.peers()
} # }
```
