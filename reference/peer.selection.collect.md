# Collect peer selection draws

TODO

## Usage

``` r
peer.selection.collect(
  fifo.file = "monerod-log.fifo",
  csv.file.suffix = "peer-selection.csv",
  unrestricted.rpc.url = "http://127.0.0.1:18081",
  verbose = 2,
  read.log.wait.time = 0.01,
  white_list.max.size = 1000,
  gray_list.max.size = 5000
)
```

## Arguments

- fifo.file:

  File name of FIFO ("named pipe") file that `monerod` output is
  directed to.

- csv.file.suffix:

  Suffix for csv files of input data. It should be the same as used in
  the `peer.selection.collect()` function that collected the data. The
  suffix is applied to `white_list-`, `gray_list-`, and `connections-`.

- unrestricted.rpc.url:

  URL and port of the `monerod` unrestricted RPC. Default is
  `http://127.0.0.1:18081`

- verbose:

  If 2, print info about every new connection draw. If 1, print info
  only about cumulative number of `white_list` and `gray_list` draws. If
  0, print nothing.

- read.log.wait.time:

  Time interval, in seconds, between when the peer list state is queried
  through RPC and when the FIFO log file is read.

- white_list.max.size:

  Maximum size of the node's peer `white_list`. This should not be
  changed under normal circumstances because it is set in the `monerod`
  code.

- gray_list.max.size:

  Maximum size of the node's peer `gray_list`. This should not be
  changed under normal circumstances because it is set in the `monerod`
  code.

## Value

NULL (invisible)

## Examples

``` r
if (FALSE) { # \dontrun{
peer.selection.collect()
} # }
```
