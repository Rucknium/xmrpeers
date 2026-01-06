# Convert IPv4 address to its subnet

Convert IPv4 address to its subnet

## Usage

``` r
as.subnet(x, mask, suffix = FALSE)
```

## Arguments

- x:

  Character vector of IPv4 addresses.

- mask:

  Integer between 0 and 32, inclusive. The subnet mask.

- suffix:

  Logical. If TRUE, return value should have "/" + mask concatenated to
  the end of the string. This produces Classless Inter-Domain Routing
  (CIDR) notation for the subnet.

## Value

Character vector.

## Examples

``` r
as.subnet("192.168.1.1", 16)
#> [1] "192.168.0.0"

as.subnet("192.168.1.1", 24, suffix = TRUE)
#> [1] "192.168.1.0/24"
```
