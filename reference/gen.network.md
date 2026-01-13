# Generate a simulated Monero network

Generates a simulated Monero network based on user-provided IP addresses
of reachable nodes. The assumed number of unreachable nodes can be
provided. A list of malicious node IP addresses can specify nodes that
do not establish outbound connections. The user can specify whether the
peer selection algorithm should perform subnet deduplication, and at
what subnet mask level. Network summary statistics are optionally
computed.

## Usage

``` r
gen.network(
  outbound.ips,
  malicious.ips = NULL,
  n.unreachable = 0,
  already.connected.subnet.level = 16,
  deduplication.subnet.level = 24,
  do.deduplication = TRUE,
  default.outbound.connections = 12,
  dropped.connection.churns = 12,
  compute.network.stats = TRUE
)
```

## Arguments

- outbound.ips:

  A character vector of IP addresses that host reachable Monero nodes.
  Required.

- malicious.ips:

  Optional character vector of IP addresses that are suspected to be
  malicious. Can contain IP address ranges with subnet notation.

- n.unreachable:

  Number of unreachable nodes assumed to be in the network.

- already.connected.subnet.level:

  The subnet mask level of the "already-connected-subnet" disqualifying
  condition. Set to 32 to disable this disqualifying condition.

- deduplication.subnet.level:

  The subnet mask level at which to perform subnet deduplication.

- do.deduplication:

  If TRUE, perform subnet deduplication. If FALSE, do not.

- default.outbound.connections:

  The number of outbound connections of each node.

- dropped.connection.churns:

  After `default.outbound.connections` has been reached, the number of
  times to drop one connection and add another one. The
  "already-connected-subnet" behavior makes some peer churning necessary
  to get the correct probability distribution.

- compute.network.stats:

  If TRUE, compute network summary statistics of the simulated network.

## Value

A list with three elements:

- nodes:

  A `data.table` with seven columns: `index`, an index of the node.
  `ip`, the IP address of the node, if it is reachable (NA if it is
  not). `already.connected.subnet`, the subnet that the node belongs to,
  based on the `already.connected.subnet.level` argument specified by
  the user. `deduplication.subnet`, the subnet that the node belongs to,
  based on the `deduplication.subnet.level` argument specified by the
  user. `reachable`, TRUE if reachable and FALSE if not. `malicious`,
  TRUE if node is on the `malicious.ips` list supplied by the user.
  `n.inbound`, number of inbound connections of the node in the
  simulated network. `n.malicious.outbound`, the number of outbound
  connections to nodes on the `malicious.ips` list.

- edgelist:

  A `data.table`. The network edge list of the directed graph. The
  `origin` column is the node establishing the connection. The
  `destination` column is the node accepting the connection. The `index`
  column of the `nodes` `data.table` is used as the identifier.

- network.stats:

  A list of four network summary statistics, computed by the `igraph`
  package: `centr_betw`, `centr_clo`, `centr_degree`, and `centr_eigen`.
  See their documentation in the `igraph` package for interpretation.

## Examples

``` r
if (FALSE) { # \dontrun{
data(good_peers)

good_peers <- stringr::str_extract(good_peers,
  "[0-9]{1,3}[.][0-9]{1,3}[.][0-9]{1,3}[.][0-9]{1,3}")
good_peers <- unique(good_peers)
good_peers <- na.omit(good_peers)
# Clean IP addresses

data(ban_list_v2)

future::plan(future::multisession,
  workers = max(c(1, floor(parallelly::availableCores()/6))))
# Multi-threaded is recommended

share.reachable <- 0.2
# Set share of nodes that are reachable to 20 percent

n.assumed.unreachable <- floor(length(good_peers) *
    ((1 - share.reachable) / share.reachable))

set.seed(314)
# This is a random simulation

generated.network <- gen.network(outbound.ips = good_peers,
  malicious.ips = ban_list_v2,
  n.unreachable = n.assumed.unreachable)

hist(generated.network$nodes[
  reachable == TRUE & malicious == FALSE, n.inbound], breaks = 50)

# Network stats
sapply(names(generated.network$network.stats), function(x) {
  generated.network$network.stats[[x]]$centralization
})
} # }
```
