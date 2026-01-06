# Reachable nodes' IP addresses

The result of a one-hour run of the Rust Monero network crawler on
2025-05-25. See `Source` for the network crawler code. Only reachable
nodes are included. The version of the network crawler used to collect
this data did not distinguish between good_peers and bad_peers. All
reachable peers were included.

## Usage

``` r
good_peers
```

## Format

### `good_peers`

A character vector with 10,653 elements. Each element has this format:

Note there are three duplicate elements. A single IP address may appear
more than once with different ports. There are 17 ipv6 IP addresses.

## Source

<https://github.com/Rucknium/misc-research/tree/main/Monero-Peer-Subnet-Deduplication/code/Rust>
