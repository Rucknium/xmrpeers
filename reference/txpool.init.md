# Initialize txpool archive database

Initializes a database file to record the arrival time of transactions
to the local Monero node.

## Usage

``` r
txpool.init(db.file = "xmr-txpool-archive.db")
```

## Arguments

- db.file:

  Name and path of database file to be created.

## Value

NULL (invisible)

## See also

[txpool.collect](txpool.collect.md), which collects txpool data and
saves it to the database file and [txpool.export](txpool.export.md),
which exports the database contents to a CSV file.

## Examples

``` r
if (FALSE) { # \dontrun{
txpool.init()
} # }
```
