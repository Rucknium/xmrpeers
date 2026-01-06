# IP ban list of suspected spy nodes

A set of suspected spy nodes. Ban list version 1 was released in
December 2024. Ban list version 2 was released in Janraury 2026. See
this GitHub issue for more information:
<https://github.com/monero-project/meta/issues/1124>

## Usage

``` r
ban_list_v1

ban_list_v2
```

## Format

### `ban_list_v1`

A character vector with 423 elements. Most are singleton IP addresses.
Six of them are IP addresses ranges in Classless Inter-Domain Routing
(CIDR) notation.

### `ban_list_v2`

A character vector with 431 elements.

An object of class `character` of length 431.

## Source

<https://github.com/Boog900/monero-ban-list>
