# Parse p2p log files

Parse p2p log files

## Usage

``` r
get.p2p.log(bitmonero.dir = "~/.bitmonero", output.file = NULL)
```

## Arguments

- bitmonero.dir:

  Directory location of the log files

- output.file:

  Optional file to output compressed data

## Value

data.frame of each transaction gossip message for each peer.

## Examples

``` r
if (FALSE) { # \dontrun{
get.p2p.log()
} # }
```
