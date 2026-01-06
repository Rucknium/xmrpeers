# Export txpool archive to CSV

Exports transaction and block arrival times from a database file to a
CSV file. Must use [txpool.init](txpool.init.md) and
[txpool.collect](txpool.collect.md) first. txpool.export can be used
while [txpool.collect](txpool.collect.md) is still running in a separate
R session.

## Usage

``` r
txpool.export(
  db.file = "xmr-txpool-archive.db",
  csv.filepath = "",
  begin.date = "1970-01-01",
  end.date = "2035-01-01"
)
```

## Arguments

- db.file:

  File name/path of the txpool archive database.

- csv.filepath:

  File path of CSV file that will be created. Leave default to save in
  current working directory.

- begin.date:

  Optional argument to restrict data export date range. Use "YYYY-MM-DD"
  format.

- end.date:

  Optional argument to restrict data export date range. Use "YYYY-MM-DD"
  format.

## Value

NULL (invisible)

## See also

[txpool.init](txpool.init.md), which create the database file and
[txpool.collect](txpool.collect.md), which collects txpool data and
saves it to the database file.

## Examples

``` r
if (FALSE) { # \dontrun{
txpool.export()
} # }
```
