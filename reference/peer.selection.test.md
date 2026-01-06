# Test of whether peer selection deduplicates subnets

TODO

## Usage

``` r
peer.selection.test(
  deduplicated.subnet.level = 24,
  do.list = c("white_list", "gray_list"),
  csv.file.suffix = "peer-selection.csv",
  stat.tests = list(rms_gof = rms_gof),
  already.connected.exclusion.subnet.level = 24,
  skip.warmup = 10 * 60,
  only.first.draw.in.batch = TRUE,
  white_list.monte.carlo.iters = 10000,
  white_list.max.size = 1000,
  gray_list.max.size = 5000
)
```

## Arguments

- deduplicated.subnet.level:

  The subnet level at which `monerod` is performing the deduplication.

- do.list:

  Which peer lists to analyze? By default, will do both white_list and
  gray_list.

- csv.file.suffix:

  Suffix for csv files of input data. It should be the same as used in
  the [`peer.selection.collect()`](peer.selection.collect.md) function
  that collected the data. The suffix is applied to `white_list-`,
  `gray_list-`, and `connections-`.

- stat.tests:

  A named list of one or more functions that performs a goodness-of-fit
  test for discrete distributions. The list elements must be named. Each
  function should take as its first argument the observed counts for
  each category and its second argument should be a vector of
  probabilities of the reference distribution. The default, `rms_gof()`,
  is the root-mean-square goodness-of-fit test in the `discretefit`
  package. The `rms_gof()` test appears to have size closer to the
  correct size, compared to other tests, when there are many zeros in
  observed counts and the reference distribution is non-uniform.

- already.connected.exclusion.subnet.level:

  The subnet level that is used to exclude subnets that the node is
  already connected to. Set to 24 by default.

- skip.warmup:

  Number of seconds to skip in the beginning of the dataset. When Monero
  nodes are frisrt booted, they have a period of establishing
  connectivity to the network when they may behave different from normal
  operation. If zero, no data is skipped.

- only.first.draw.in.batch:

  The Monero node will often make multiple draws of candidate
  connections in a short period because the first draw(s) fail to
  connect. The draws in these "batches" are done without replacement,
  but the statistical test assumes that draws are done with replacement.
  This argument is TRUE by default.

- white_list.monte.carlo.iters:

  Number of iterations for simulating the reference probability
  distribution that draws from the `white_list` should have. If the
  process is unacceptably slow, this number can be reduced to 1000 at
  the cost of a less accurate test.

- white_list.max.size:

  Maximum size of the node's peer `white_list`. This should not be
  changed under normal circumstances because it is set in the `monerod`
  code.

- gray_list.max.size:

  Maximum size of the node's peer `gray_list`. This should not be
  changed under normal circumstances because it is set in the `monerod`
  code.

## Value

A list. One element for each of the `do.list` specified. Each of the
elements is a list with a `tests` component that contains the results of
the statistical tests specified in `stat.tests`. The second element of
the list is a `data.frame`/`data.table` that contains the observed
counts of new connection IP addresses drawn by `monerod` and the
expected reference probabilities.

## Examples

``` r
if (FALSE) { # \dontrun{
future::plan(future::multisession)
# Multi-threaded is recommended
peer.selection.results <- peer.selection.test()
} # }
```
