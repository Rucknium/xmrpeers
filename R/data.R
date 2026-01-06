#' Reachable nodes' IP addresses
#'
#' The result of a one-hour run of the Rust Monero network crawler
#' on 2025-05-25. See `Source` for the network crawler code. Only reachable
#' nodes are included. The version of the network crawler used to collect
#' this data did not distinguish between good_peers and bad_peers. All
#' reachable peers were included.
#'
#' @format ## `good_peers`
#' A character vector with 10,653 elements. Each element has this format:
#' \describe{
#'   `peer: <ip>:<port>, `
#' }
#'
#' Note there are three duplicate elements. A single IP address may appear
#' more than once with different ports. There are 17 ipv6 IP addresses.
#' @source <https://github.com/Rucknium/misc-research/tree/main/Monero-Peer-Subnet-Deduplication/code/Rust>
"good_peers"


#' IP ban list of suspected spy nodes
#'
#' A set of suspected spy nodes. Ban list version 1 was released in December
#' 2024. Ban list version 2 was released in Janraury 2026. See this GitHub
#' issue for more information:
#' \url{https://github.com/monero-project/meta/issues/1124}
#'
#' @format ## `ban_list_v1`
#' A character vector with 423 elements. Most are singleton IP addresses. Six
#' of them are IP addresses ranges in Classless Inter-Domain Routing (CIDR)
#' notation.
#' @format ## `ban_list_v2`
#' A character vector with 431 elements.
#'
#' @source <https://github.com/Boog900/monero-ban-list>
"ban_list_v1"

#' @rdname ban_list_v1
"ban_list_v2"

