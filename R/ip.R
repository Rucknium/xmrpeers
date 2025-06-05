

#' Convert IPv4 address to its subnet
#'
#' @param x Character vector of IPv4 addresses.
#' @param mask Integer between 0 and 32, inclusive. The subnet mask.
#' @param suffix Logical. If TRUE, return value should have
#' "/" + mask concatenated to the end of the string. This produces
#' Classless Inter-Domain Routing (CIDR) notation for the subnet.
#'
#' @return
#' Character vector.
#' @export
#'
#' @examples
#' as.subnet("192.168.1.1", 16)
#'
#' as.subnet("192.168.1.1", 24, suffix = TRUE)
as.subnet <- function(x, mask, suffix = FALSE) {
  stopifnot(mask %in% 0:32)
  suppressWarnings(result <- as.character(IP::ipv4(x) & IP::ipv4.netmask(mask)))
  # Will get a warning about NAs being produced if all x are not IPv4
  if (suffix) {
    result <- paste0(result, "/", mask)
  }
  result
}



#' Check for IP inclusion in set of IP singletons and ranges
#'
#' @param x Character vector of singleton IP addresses. Can contain
#' duplicate elements.
#' @param ip.set Character vector of IP address singletons and/or ranges.
#' Ranges should be in Classless Inter-Domain Routing (CIDR) notation, e.g.
#' "169.254.0.0/24". No duplicate elements allowed. Ranges should not overlap
#' (see documentation in \link[IP]{ip.match}).
#'
#' @return
#' Logical vector , with length same as `x`.
#' @export
#'
#' @examples
#' in.ip.set(c("192.168.5.1", "192.168.4.1", "169.254.0.5"),
#'   c("169.254.0.0/24", "192.168.1.1", "192.168.5.1"))
#'
in.ip.set <- function(x, ip.set) {
  # x can have duplicated elements
  if (any(duplicated(ip.set))) {
    stop("ip.set cannot have any duplicate elements.")
  }
  ip.set.singletons <- ip.set[ ! grepl("/", ip.set)]
  ip.set.ranges <- ip.set[grepl("/", ip.set)]
  result <- x %in% ip.set.singletons
  result <- result | (! is.na(IP::ip.match(IP::ipv4(x), IP::ipv4r(ip.set.ranges))))
  result
}


#' Collect connected peers' IP addresses
#'
#' @description Collects IP addresses of peers that the local node has
#' established outbound connections to. The time and set of IP addresses are
#' saved to a CSV file. These IP addresses are checked against an optional
#' set of suspected malicious IP addresses. Information about the share of
#' outbound connections to suspected malicious IP addresses is printed.
#' IP addresses are grouped by subnet and information is printed to check for
#' possible "subnet saturation" by malicious entities. This function is an
#' infinite loop. `ctrl + c` to interrupt the function.
#'
#' @param csv.file The name of the CSV file to write to and read from. If it
#' already exists, data will be appended to it and the whole file will be
#' used to compute top subnet information.
#' @param unrestricted.rpc.url URL and port of the `monerod` unrestricted RPC.
#' Default is `http://127.0.0.1:18081`
#' @param malicious.ips A character vector of IP addresses that are suspected
#' to be malicious.
#' @param top.subnet.mask Numeric value. The IP address subnet mask to print
#' summary information about.
#' @param n.top.subnets Number of subnets to print summary information about.
#' @param poll.time How often, in seconds, to collect data from the local
#' Monero node. Default is 30 seconds.
#'
#' @return
#' NULL (invisible)
#' @export
#'
#' @examples
#' \dontrun{
#' data(ban_list)
#' peers.ip.collect(malicious.ips = ban_list)
#' }

peers.ip.collect <- function(csv.file = "xmr-peers-ip.csv",
  unrestricted.rpc.url = "http://127.0.0.1:18081", malicious.ips = NULL,
  top.subnet.mask = 24, n.top.subnets = 10, poll.time = 30) {

  if ( ! (is.null(malicious.ips) | is.vector(malicious.ips)) ) {
    stop("malicious.ips must be an atomic vector, i.e. not a data.frame or matrix.")
  }

  if (file.exists(csv.file)) {
    peer.ip.data <- as.matrix(read.csv(csv.file, header = FALSE))
    peer.ip.data <- peer.ip.data[, -1] # Remove the time column
  } else {
    file.create(csv.file)
    peer.ip.data <- matrix(NA_character_, ncol = 24, nrow = 0)
  }

  json.post <- RJSONIO::toJSON(
    list(
      jsonrpc = "2.0",
      id = "0",
      method = "get_connections",
      params = ""
    )
  )

  session.start.time <- Sys.time()

  cat(paste0(format(session.start.time, "%Y-%m-%d %T"),
    ifelse(length(malicious.ips) > 0, " Start collecting peer data...\n",
      " Start collecting peer data (no malicious peer list)...\n")))

  # peer.malicious.ips.data <- matrix(NA_real_, ncol = 24, nrow = 0)

  handle <- RCurl::getCurlHandle()

  while (TRUE) {

    RPC.time <- as.numeric(Sys.time())

    peers <- RJSONIO::fromJSON(
      RCurl::postForm(paste0(unrestricted.rpc.url, "/json_rpc"),
        .opts = list(
          userpwd = "",
          postfields = json.post,
          httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')
        ),
        curl = handle
      ), asText = TRUE
    )

    if(length(peers$result$connections) == 0 ) {
      cat("Unexpected results from node's RPC response. Check that monerod's unrestricted RPC port is at ",
        unrestricted.rpc.url, "\n")
      Sys.sleep(poll.time)
      next
    }

    peers <- peers$result$connections

    peer.address <- sapply(peers, FUN = function(x) {gsub("[:][0-9]*", "", x$address)})
    peer.incoming <- sapply(peers, FUN = function(x) {x$incoming})

    peer.address <- peer.address[ ! peer.incoming]

    peer.address.ipv4 <- IP::ipv4(peer.address)

    peer.address <- peer.address[order(peer.address.ipv4)]

    peer.ip.data <- rbind(peer.ip.data,
      matrix(c(peer.address, rep(NA_character_, 24 - length(peer.address))),
        ncol = 24, nrow = 1))

    write.table(cbind(RPC.time, peer.ip.data[nrow(peer.ip.data), , drop = FALSE]), file = csv.file,
      sep = ",", append = TRUE, col.names = FALSE, row.names = FALSE)

    if (length(malicious.ips) > 0) {

      peer.address.malicious.ips <- as.numeric(in.ip.set(peer.address, malicious.ips))

      # peer.malicious.ips.data <- rbind(peer.malicious.ips.data,
      #   matrix(c(peer.address.malicious.ips, rep(NA_real_, 24 - length(peer.address.malicious.ips))),
      #     ncol = 24, nrow = 1))

      cat(format(Sys.time(), "%Y-%m-%d %T"), " Outbound peers on malicious IPs list: ",
        sum(peer.address.malicious.ips), "/", length(peer.address.malicious.ips),
        " (", round(100 * mean(peer.address.malicious.ips)), "%)", "\n", sep = "")

    }

    peer.ip.data.unique <- na.omit(unique(c(peer.ip.data)))

    peer.ip.data.unique.subnet <- as.subnet(peer.ip.data.unique, top.subnet.mask)

    if (length(peer.ip.data.unique.subnet) < n.top.subnets) { Sys.sleep(poll.time); next }

    subnets.message <- sort(table(peer.ip.data.unique.subnet), decreasing = TRUE)[1:n.top.subnets]

    subnets.message <- paste0(names(subnets.message), "/", top.subnet.mask,
      ": ", subnets.message, collapse = ", ")

    cat("Top subnets (", 2^(32 - top.subnet.mask) - 2,
      " possible /", top.subnet.mask, " IPs each): ", subnets.message,
      "\n", sep = "")

    Sys.sleep(poll.time)

  }

  return(invisible(NULL))

}

