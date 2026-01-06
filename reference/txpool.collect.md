# Collect txpool archive data

Queries the local Monero node for its txpool once per second. The time
of arrival of each transaction is saved to a database file. The database
file must first be created by [txpool.init](txpool.init.md). This
function executes an infinite loop. Input `ctrl + c` to interrupt the
function.

## Usage

``` r
txpool.collect(
  db.file = "xmr-txpool-archive.db",
  unrestricted.rpc.url = "http://127.0.0.1:18081"
)
```

## Arguments

- db.file:

  Name and path of database file created by
  [txpool.init](txpool.init.md).

- unrestricted.rpc.url:

  URL and port of the `monerod` unrestricted RPC. Default is
  `http://127.0.0.1:18081`

## Value

NULL (invisible)

## See also

[txpool.init](txpool.init.md), which create the database file and
[txpool.export](txpool.export.md), which exports the database contents
to a CSV file.

## Examples

``` r
if (FALSE) { # \dontrun{
txpool.collect()
} # }
```
