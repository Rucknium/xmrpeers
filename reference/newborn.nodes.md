# Print likely newborn nodes to the console

When a node connects to our own node, it could be a newborn node that is
syncing from the genesis block. `newborn.nodes` queries the local Monero
node about node connections with a `get_peers` call and prints
information about likely newborn nodes to the console. The unrestricted
RPC port must be reachable by R. This function is an infinite loop.
`ctrl + c` to interrupt the function and print all IP addresses of
likely newborn nodes recorded so far. Each peer will only be printed
once during a specific run of `newborn.nodes`, even if the node
disconnects and reconnects later.

## Usage

``` r
newborn.nodes(
  unrestricted.rpc.url = "http://127.0.0.1:18081",
  poll.time = 30,
  sync.height.lag = 30 * 24 * 90,
  avg_upload.limit = 10,
  current_upload.limit = 10
)
```

## Arguments

- unrestricted.rpc.url:

  URL and port of the `monerod` unrestricted RPC. Default is
  `http://127.0.0.1:18081`

- poll.time:

  How often, in seconds, to check for a newborn node connection. Default
  is 30 seconds.

- sync.height.lag:

  Criteria to consider a node as "newborn". Nodes that have a height
  greater than current network height minus `sync.height.lag` will not
  be considered a newborn node. Default is 3 months.

- avg_upload.limit:

  Criteria to consider a node as "newborn". The `avg_upload` from the
  `get_peers` call must be greater than or equal to `avg_upload.limit`
  OR `current_upload` must be greater than or equal to
  `current_upload.limit` to consider the node as "newborn". Default is
  10 for both limits. Sometimes a node gets stuck at a low height, so it
  isn't actually new or syncing. This criteria makes sure that the peer
  is actually actively syncing data.

- current_upload.limit:

  Criteria to consider a node as "newborn".

## Value

NULL (invisible)

## Examples

``` r
if (FALSE) { # \dontrun{
newborn.nodes()
} # }
```
