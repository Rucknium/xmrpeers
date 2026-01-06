# Check for IP inclusion in set of IP singletons and ranges

Check for IP inclusion in set of IP singletons and ranges

## Usage

``` r
in.ip.set(x, ip.set)
```

## Arguments

- x:

  Character vector of singleton IP addresses. Can contain duplicate
  elements.

- ip.set:

  Character vector of IP address singletons and/or ranges. Ranges should
  be in Classless Inter-Domain Routing (CIDR) notation, e.g.
  "169.254.0.0/24". No duplicate elements allowed. Ranges should not
  overlap (see documentation in
  [ip.match](https://rdrr.io/pkg/IP/man/varia.html)).

## Value

Logical vector , with length same as `x`.

## Examples

``` r
in.ip.set(c("192.168.5.1", "192.168.4.1", "169.254.0.5"),
  c("169.254.0.0/24", "192.168.1.1", "192.168.5.1"))
#> [1]  TRUE FALSE  TRUE
```
