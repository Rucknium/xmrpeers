# Collect connected peers' IP addresses

Collects IP addresses of peers that the local node has established
outbound connections to. The time and set of IP addresses are saved to a
CSV file. These IP addresses are checked against an optional set of
suspected malicious IP addresses. Information about the share of
outbound connections to suspected malicious IP addresses is printed. IP
addresses are grouped by subnet and information is printed to check for
possible "subnet saturation" by malicious entities. This function is an
infinite loop. `ctrl + c` to interrupt the function.

## Usage

``` r
peers.ip.collect(
  csv.file = "xmr-peers-ip.csv",
  unrestricted.rpc.url = "http://127.0.0.1:18081",
  malicious.ips = NULL,
  top.subnet.mask = 24,
  n.top.subnets = 10,
  poll.time = 30
)
```

## Arguments

- csv.file:

  The name of the CSV file to write to and read from. If it already
  exists, data will be appended to it and the whole file will be used to
  compute top subnet information.

- unrestricted.rpc.url:

  URL and port of the `monerod` unrestricted RPC. Default is
  `http://127.0.0.1:18081`

- malicious.ips:

  A character vector of IP addresses that are suspected to be malicious.

- top.subnet.mask:

  Numeric value. The IP address subnet mask to print summary information
  about.

- n.top.subnets:

  Number of subnets to print summary information about.

- poll.time:

  How often, in seconds, to collect data from the local Monero node.
  Default is 30 seconds.

## Value

NULL (invisible)

## Examples

``` r
if (FALSE) { # \dontrun{
data(ban_list_v2)
peers.ip.collect(malicious.ips = ban_list_v2)
} # }
```
