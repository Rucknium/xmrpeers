

#' Collect peer selection draws
#'
#' @description TODO
#'
#' @param fifo.file File name of FIFO ("named pipe") file that `monerod`
#' output is directed to.
#' @param csv.file.suffix Suffix for csv files of input data. It should
#' be the same as used in the `peer.selection.collect()` function that
#' collected the data. The suffix is applied to `white_list-`, `gray_list-`,
#' and `connections-`.
#' @param unrestricted.rpc.url URL and port of the `monerod` unrestricted RPC.
#' Default is `http://127.0.0.1:18081`
#' @param verbose If 2, print info about every new connection draw. If 1,
#' print info only about cumulative number of `white_list` and `gray_list`
#' draws. If 0, print nothing.
#' @param read.log.wait.time Time interval, in seconds, between when the
#' peer list state is queried through RPC and when the FIFO log file
#' is read.
#' @param white_list.max.size Maximum size of the node's peer `white_list`.
#' This should not be changed under normal circumstances because it is
#' set in the `monerod` code.
#' @param gray_list.max.size Maximum size of the node's peer `gray_list`.
#' This should not be changed under normal circumstances because it is
#' set in the `monerod` code.
#'
#' @return
#' NULL (invisible)
#' @export
#'
#' @examples
#' \dontrun{
#' peer.selection.collect()
#' }

peer.selection.collect <- function(
  fifo.file = "monerod-log.fifo",
  csv.file.suffix = "peer-selection.csv",
  unrestricted.rpc.url = "http://127.0.0.1:18081",
  verbose = 2,
  read.log.wait.time = 0.01,
  white_list.max.size = 1000,
  gray_list.max.size = 5000) {

  stopifnot(length(verbose) == 1 && verbose %in% 0:2)

  handle <- RCurl::getCurlHandle()

  json.post.get_peer_list <- RJSONIO::toJSON(
    list(
      jsonrpc = "2.0",
      id = "0",
      method = "",
      params = ""
    )
  )

  get_peer_list <- function() {
    RJSONIO::fromJSON(
      RCurl::postForm(paste0(unrestricted.rpc.url, "/get_peer_list"),
        .opts = list(
          userpwd = "",
          postfields = json.post.get_peer_list,
          httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')
        ),
        curl = handle
      ), asText = TRUE
    )
  }


  json.post.get_connections <- RJSONIO::toJSON(
    list(
      jsonrpc = "2.0",
      id = "0",
      method = "get_connections",
      params = ""
    )
  )

  get_connections <- function() {
    RJSONIO::fromJSON(
      RCurl::postForm(paste0(unrestricted.rpc.url, "/json_rpc"),
        .opts = list(
          userpwd = "",
          postfields = json.post.get_connections,
          httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')
        ),
        curl = handle
      ), asText = TRUE
    )
  }

  get.connection.addresses <- function(connections) {
    connections <- connections$result$connections
    connections.address <- sapply(connections, FUN = function(x) {gsub("[:][0-9]*", "", x$address)})
    connections.incoming <- sapply(connections, FUN = function(x) {x$incoming})
    connections.address <- connections.address[ ! connections.incoming]
    connections.address
  }

  total.draws.from.white_list <- 0
  total.draws.from.gray_list <- 0

  fifo.file.connection <- fifo(fifo.file, 'r')

  on.exit({
    message("Total number of draws from white_list: ", total.draws.from.white_list)
    message("Total number of draws from gray_list: ", total.draws.from.gray_list)
    close(fifo.file.connection)
  })

  white_list.csv.file <- paste0("white_list-", csv.file.suffix)
  gray_list.csv.file <- paste0("gray_list-", csv.file.suffix)
  connections.csv.file <- paste0("connections-", csv.file.suffix)

  # Flush the FIFO file so we have fresh data
  message("Flushing FIFO file", appendLF = FALSE)
  flushed.lines <- paste0(readLines(fifo.file.connection), 1)
  while (length(flushed.lines) > 0) {
    flushed.lines <- readLines(fifo.file.connection)
    Sys.sleep(1)
    cat(".")
  }


  while (TRUE) {

    RPC.time <- as.numeric(Sys.time())
    peer_list <- get_peer_list()
    current.connections <- get_connections()

    Sys.sleep(read.log.wait.time)

    log.output <- readLines(fifo.file.connection)

    considering.connecting <- grep(" needed., in loop pass", log.output)

    if (length(considering.connecting) > 0) {

      draws.from.white_list <- grep("white list.* needed., in loop pass 1", log.output)
      draws.from.gray_list  <- grep("gray list.* needed., in loop pass 1", log.output)

      total.draws.from.white_list <- total.draws.from.white_list + length(draws.from.white_list)
      total.draws.from.gray_list <- total.draws.from.gray_list + length(draws.from.gray_list)

      if (length(draws.from.white_list) > 0) {

        white_list <- data.table::rbindlist(peer_list$white_list, fill = TRUE)
        setorder(white_list, -last_seen)
        # Seems to originally be sorted by the "ip" column, which is IP address
        # in integer form

        hosts <- white_list$host
        hosts <- c(hosts, rep(NA, white_list.max.size - length(hosts)))

        new.connections <- stringr::str_extract(log.output[draws.from.white_list],
          "[0-9]{1,3}[.][0-9]{1,3}[.][0-9]{1,3}[.][0-9]{1,3}")

        if (verbose == 2) {
          message(paste0("white_list draw: ", new.connections, " Position(s) in white_list: ",
            paste0(which( gsub("::ffff:", "", hosts, fixed = TRUE) %in% new.connections ),
              collapse = "-")), collapse = "\n")
        }

        hosts <- cbind(new.connections,
          matrix(rep(c(RPC.time, hosts), length(new.connections)),
            nrow = length(new.connections), byrow = TRUE)
        )

        write.table(hosts, file = white_list.csv.file,
          sep = ",", na = "",
          append = TRUE, row.names = FALSE, col.names = FALSE)
      }

      if (length(draws.from.gray_list) > 0) {

        gray_list <- data.table::rbindlist(peer_list$gray_list, fill = TRUE)
        # Don't need to set order on the gray_list because the node's draw
        # from the gray_list is uniform probability

        hosts <- gray_list$host
        hosts <- c(hosts, rep(NA, gray_list.max.size - length(hosts)))

        new.connections <- stringr::str_extract(log.output[draws.from.gray_list],
          "[0-9]{1,3}[.][0-9]{1,3}[.][0-9]{1,3}[.][0-9]{1,3}")

        if (verbose == 2) {
          message(paste0("gray_list draw: ", new.connections, " Position(s) in gray_list: ",
            paste0(which( gsub("::ffff:", "", hosts, fixed = TRUE) %in% new.connections ),
              collapse = "-")), collapse = "\n")
        }

        hosts <- cbind(new.connections,
          matrix(rep(c(RPC.time, hosts), length(new.connections)),
            nrow = length(new.connections), byrow = TRUE)
        )

        write.table(hosts, file = gray_list.csv.file,
          sep = ",", na = "",
          append = TRUE, row.names = FALSE, col.names = FALSE)

      }

      current.connections <- get.connection.addresses(current.connections)

      if (FALSE && verbose == 2) {
        message(paste0(log.output[grep("Random connection index", log.output)], collapse = "\n"))
        message(paste0(log.output[considering.connecting], collapse = "\n"))
        message("Current connections: ", length(current.connections))
      }

      if (verbose >= 1) {
        message("Total number of draws from white_list: ", total.draws.from.white_list)
        message("Total number of draws from gray_list: ", total.draws.from.gray_list)
      }

      write.table(matrix(c(RPC.time, current.connections), nrow = 1),
        file = connections.csv.file,
        sep = ",", na = "",
        append = TRUE, row.names = FALSE, col.names = FALSE)

    }

  }

  return(invisible(NULL))

}














#' Test of whether peer selection deduplicates subnets
#'
#' @description TODO
#'
#' @param deduplicated.subnet.level The subnet level at which `monerod` is
#' performing the deduplication.
#' @param do.list Which peer lists to analyze? By default, will do both
#' white_list and gray_list.
#' @param csv.file.suffix Suffix for csv files of input data. It should
#' be the same as used in the `peer.selection.collect()` function that
#' collected the data. The suffix is applied to `white_list-`, `gray_list-`,
#' and `connections-`.
#' @param stat.tests A named list of one or more functions that performs
#' a goodness-of-fit test for discrete distributions. The list elements must
#' be named. Each function should take as its first argument the observed
#' counts for each category and its second argument should be a vector of
#' probabilities of the reference distribution. The default, `rms_gof()`,
#' is the root-mean-square goodness-of-fit test in the `discretefit`
#' package. The `rms_gof()` test appears to have size closer to the correct
#' size, compared to other tests,  when there are many zeros in observed counts
#' and the reference distribution is non-uniform.
#' @param already.connected.exclusion.subnet.level The subnet level that is
#' used to exclude subnets that the node is already connected to. Set to
#' 24 by default.
#' @param skip.warmup Number of seconds to skip in the beginning of the
#' dataset. When Monero nodes are frisrt booted, they have a period of
#' establishing connectivity to the network when they may behave different
#' from normal operation. If zero, no data is skipped.
#' @param only.first.draw.in.batch The Monero node will often make
#' multiple draws of candidate connections in a short period because the
#' first draw(s) fail to connect. The draws in these "batches" are done
#' without replacement, but the statistical test assumes that draws are done
#' with replacement. This argument is TRUE by default.
#' @param white_list.monte.carlo.iters Number of iterations for simulating
#' the reference probability distribution that draws from the `white_list`
#' should have. If the process is unacceptably slow, this number can be
#' reduced to 1000 at the cost of a less accurate test.
#' @param white_list.max.size Maximum size of the node's peer `white_list`.
#' This should not be changed under normal circumstances because it is
#' set in the `monerod` code.
#' @param gray_list.max.size Maximum size of the node's peer `gray_list`.
#' This should not be changed under normal circumstances because it is
#' set in the `monerod` code.
#'
#' @return
#' A list. One element for each of the `do.list` specified. Each of the
#' elements is a list with a `tests` component that contains the results of
#' the statistical tests specified in `stat.tests`. The second element of
#' the list is a `data.frame`/`data.table` that contains the observed counts
#' of new connection IP addresses drawn by `monerod` and the expected
#' reference probabilities.
#' @export
#'
#' @examples
#' \dontrun{
#' future::plan(future::multisession)
#' # Multi-threaded is recommended
#' peer.selection.results <- peer.selection.test()
#' }

peer.selection.test <- function(
  deduplicated.subnet.level = 24,
  do.list = c("white_list", "gray_list"),
  csv.file.suffix = "peer-selection.csv",
  stat.tests = list(rms_gof = rms_gof),
  already.connected.exclusion.subnet.level = 24,
  skip.warmup = 10 * 60,
  only.first.draw.in.batch = TRUE,
  white_list.monte.carlo.iters = 10000,
  white_list.max.size = 1000,
  gray_list.max.size = 5000) {

  stopifnot(length(do.list) %between% 1:2)

  if (length(names(stat.tests)) == 0 ||
      any(nchar(names(stat.tests)) == 0)) {
    stop("stat.tests list elements do not all have names.")
  }

  white_list.csv.file <- paste0("white_list-", csv.file.suffix)
  gray_list.csv.file <- paste0("gray_list-", csv.file.suffix)
  connections.csv.file <- paste0("connections-", csv.file.suffix)

  connections <- read.csv(connections.csv.file, header = FALSE,
    col.names = c("time",
      paste0("connection.", formatC(seq_len(white_list.max.size), width = 4, flag = "0") )))

  connections <- connections[, apply(connections, 2, FUN = function(x) ! all(is.na(x)))]
  # Remove columns with all missings
  setDT(connections)

  if (skip.warmup > 0) {
    start.time <- connections[1, time]
    connections <- connections[time >= (start.time + skip.warmup), ]
  }

  if ("gray_list" %in% do.list) {
    # Do this now so there would not be much time between reading of all data files.
    # simulated.probability for white_list takes a long time to run.
    gray_list <- read.csv(gray_list.csv.file, header = FALSE,
      col.names = c("new.connection", "time",
        paste0("peer.", formatC(seq_len(gray_list.max.size), width = 4, flag = "0") )))
    setDT(gray_list)

    if (only.first.draw.in.batch) {
      gray_list <- unique(gray_list, by = "time")
    }

    gray_list <- merge(gray_list, connections, by = "time")

  }

  if ("white_list" %in% do.list) {

    white_list <- read.csv(white_list.csv.file, header = FALSE,
      col.names = c("new.connection", "time",
        paste0("peer.", formatC(seq_len(white_list.max.size), width = 4, flag = "0") )))
    setDT(white_list)

    if (only.first.draw.in.batch) {
      white_list <- unique(white_list, by = "time")
    }

    white_list <- merge(white_list, connections, by = "time")

    white_list[, already.connected := grepl(new.connection, apply(.SD, 1,
      FUN = function(x) {paste(x, collapse = "  ")} )),
      .SDcols = patterns("^connection[.][0-9]+$"), by = seq_len(nrow(white_list))]

    white_list <- white_list[ already.connected == FALSE, ]
    white_list[, already.connected := NULL]
    # If RPC results say we are already connected to the new.connection,
    # which could be caused by a race condition, then exclude the draw


    max_index <- 19
    x <- 0:(16 * max_index)
    parabolic.probability <- c(unclass(prop.table(table(floor((x*x*x)/(max_index*max_index*16*16*16))))))
    # From get_random_index_with_fixed_probability() function:
    # https://github.com/monero-project/monero/blob/master/src/p2p/net_node.inl#L1302-L1312

    message(
      "Hypothesis test on white_list. Null hypothesis: Node behavior fits subnet deduplication specification\n",
      "Number of observations: ", nrow(white_list), "\n",
      "-----------------------------------------------------------------------------------------------------")

    simulated.probability <- future.apply::future_apply(white_list, 1, FUN = function(x) {

      peers <- data.table(host = na.omit(x[grepl("peer", names(x))]))
      peers[, host := gsub("::ffff:", "", host, fixed = TRUE)]
      # Delete the leading ffff
      # https://www.ibm.com/docs/en/i/7.6.0?topic=concepts-ipv6-address-formats
      # IPv4-mapped IPv6 address uses this alternative format. This type of
      # address is used to represent IPv4 nodes as IPv6 addresses. It allows
      # IPv6 applications to communicate directly with IPv4 applications. For
      # example, 0:0:0:0:0:ffff:192.1.56.10 and ::ffff:192.1.56.10/96 (shortened format).
      connections <- na.omit(x[grepl("^connection[.][0-9]+$", names(x))])
      peers <- peers[ ! host %chin% connections, ]
      # Remove peers that node is already connected to
      peers[, subnet.AC := as.subnet(host, already.connected.exclusion.subnet.level)]
      peers <- peers[! subnet.AC %chin%
          as.subnet(connections, already.connected.exclusion.subnet.level), ]
      # Remove peers in subnets that the node is already connected to
      peers[, subnet.deduplicated := as.subnet(host, deduplicated.subnet.level)]
      peers[, orig.order := seq_len(.N)]

      result <- replicate(white_list.monte.carlo.iters, {
        peers <- peers[sample(.N), ]
        peers <- unique(peers, by = "host")
        # Deduplicate ports
        peers <- unique(peers, by = "subnet.deduplicated")
        setorder(peers, orig.order)
        data.length <- min(length(parabolic.probability), nrow(peers))
        peers <- peers[seq_len(data.length), ]
        data.table(host = peers$host,
          probability = parabolic.probability[seq_len(data.length)]
        )
      }, simplify = FALSE)

      result <- rbindlist(result)
      result <- result[, .(probability = sum(probability)), by = "host"]
      result[, probability := probability / sum(probability)]
      result

    }, simplify = FALSE, future.seed = TRUE)


    simulated.probability <- rbindlist(simulated.probability)
    simulated.probability <- simulated.probability[, .(probability = sum(probability)), by = "host"]
    simulated.probability[, probability := probability / sum(probability)]


    # TODO: Need to convert it into subnet?


    new.connections <- white_list$new.connection
    rm(white_list)
    new.connections <- new.connections[new.connections %in% simulated.probability$host]

    new.connections <- as.data.frame(table(new.connections), stringsAsFactors = FALSE)

    new.connections <- merge(new.connections, simulated.probability,
      by.x = "new.connections", by.y = "host", all.y = TRUE)

    setDT(new.connections)
    new.connections[is.na(Freq), Freq := 0]

    tests <- list()

    for (i in seq_along(stat.tests)) {
      tests[[ names(stat.tests)[i] ]] <-
        stat.tests[[i]](new.connections$Freq, new.connections$probability)
      print(tests[[ names(stat.tests)[i] ]])
    }


    results.white_list <- list(tests = tests, selection.frequency = new.connections)


  } else {
    results.white_list <- NULL
  }



  if ("gray_list" %in% do.list) {

    gray_list[, already.connected := grepl(new.connection, apply(.SD, 1,
      FUN = function(x) {paste(x, collapse = "  ")} )),
      .SDcols = patterns("^connection[.][0-9]+$"), by = seq_len(nrow(gray_list))]

    gray_list <- gray_list[ already.connected == FALSE, ]
    gray_list[, already.connected := NULL]
    # If RPC results say we are already connected to the new.connection,
    # which could be caused by a race condition, then exclude the draw

    message(
      "Hypothesis test on gray_list. Null hypothesis: Node behavior fits subnet deduplication specification\n",
      "Number of observations: ", nrow(gray_list), "\n",
      "-----------------------------------------------------------------------------------------------------")

    simulated.probability <- future.apply::future_apply(gray_list, 1, FUN = function(x) {

      peers <- data.table(host = na.omit(x[grepl("peer", names(x))]))
      peers[, host := gsub("::ffff:", "", host, fixed = TRUE)]
      # Delete the leading ffff
      # https://www.ibm.com/docs/en/i/7.6.0?topic=concepts-ipv6-address-formats
      # IPv4-mapped IPv6 address uses this alternative format. This type of
      # address is used to represent IPv4 nodes as IPv6 addresses. It allows
      # IPv6 applications to communicate directly with IPv4 applications. For
      # example, 0:0:0:0:0:ffff:192.1.56.10 and ::ffff:192.1.56.10/96 (shortened format).
      connections <- na.omit(x[grepl("^connection[.][0-9]+$", names(x))])
      peers <- peers[ ! host %chin% connections, ]
      # Remove peers that node is already connected to
      peers[, subnet.AC := as.subnet(host, already.connected.exclusion.subnet.level)]
      peers <- peers[! subnet.AC %chin%
          as.subnet(connections, already.connected.exclusion.subnet.level), ]
      # Remove peers in subnets that the node is already connected to
      peers <- unique(peers, by = "host")
      # Deduplicate ports
      peers[, subnet.deduplicated := as.subnet(host, deduplicated.subnet.level)]
      peers[, probability := 1/.N, by = "subnet.deduplicated"]
      result <- peers[, .(host = host, probability = probability)]
      result[, probability := probability / sum(probability)]
      result

    }, simplify = FALSE)

    simulated.probability <- rbindlist(simulated.probability)
    simulated.probability <- simulated.probability[, .(probability = sum(probability)), by = "host"]
    simulated.probability[, probability := probability / sum(probability)]

    # new.connections <- unique(
    #   as.subnet(gray_list$new.connection, deduplicated.subnet.level))
    new.connections <- gray_list$new.connection
    rm(gray_list)
    new.connections <- new.connections[new.connections %in% simulated.probability$host]

    new.connections <- as.data.frame(table(new.connections), stringsAsFactors = FALSE)

    new.connections <- merge(new.connections, simulated.probability,
      by.x = "new.connections", by.y = "host", all.y = TRUE)

    setDT(new.connections)
    new.connections[is.na(Freq), Freq := 0]

    tests <- list()

    for (i in seq_along(stat.tests)) {
      tests[[ names(stat.tests)[i] ]] <-
        stat.tests[[i]](new.connections$Freq, new.connections$probability)
      print(tests[[ names(stat.tests)[i] ]])
    }

    results.gray_list <- list(tests = tests, selection.frequency = new.connections)

    # TODO: Fix "blank" IP addresses

  } else {
    results.gray_list <- NULL
  }


  list(white_list = results.white_list, gray_list = results.gray_list)

}









#' Generate a simulated Monero network
#'
#' @description Generates a simulated Monero network based on user-provided
#' IP addresses of reachable nodes. The assumed number of unreachable nodes
#' can be provided. A list of malicious node IP addresses can specify nodes
#' that do not establish outbound connections. The user can specify whether
#' the peer selection algorithm should perform subnet deduplication, and
#' at what subnet mask level. Network summary statistics are optionally
#' computed.
#'
#' @param outbound.ips A character vector of IP addresses that host reachable
#' Monero nodes. Required.
#' @param malicious.ips Optional character vector of IP addresses that are suspected
#' to be malicious. Can contain IP address ranges with subnet notation.
#' @param n.unreachable Number of unreachable nodes assumed to be in the
#' network.
#' @param already.connected.subnet.level The subnet mask level of the
#' "already-connected-subnet" disqualifying condition. Set to 32 to disable
#' this disqualifying condition.
#' @param deduplication.subnet.level The subnet mask level at which to perform
#' subnet deduplication.
#' @param do.deduplication If TRUE, perform subnet deduplication. If FALSE,
#' do not.
#' @param default.outbound.connections The number of outbound connections
#' of each node.
#' @param dropped.connection.churns After `default.outbound.connections`
#' has been reached, the number of times to drop one connection and add
#' another one. The "already-connected-subnet" behavior makes some peer
#' churning necessary to get the correct probability distribution.
#' @param compute.network.stats If TRUE, compute network summary statistics
#' of the simulated network.
#'
#' @return
#' A list with three elements:
#' \describe{
#'   \item{nodes}{A `data.table` with seven columns: `index`, an index of the
#'   node. `ip`, the IP address of the node, if it is reachable (NA if it
#'   is not). `already.connected.subnet`, the subnet that the node belongs to,
#'   based on the `already.connected.subnet.level` argument specified by the
#'   user. `deduplication.subnet`, the subnet that the node belongs to,
#'   based on the `deduplication.subnet.level` argument specified by the user.
#'   `reachable`, TRUE if reachable and FALSE if not. `malicious`, TRUE if
#'   node is on the `malicious.ips` list supplied by the user. `n.inbound`,
#'   number of inbound connections of the node in the simulated network.
#'   `n.malicious.outbound`, the number of outbound connections to nodes
#'   on the `malicious.ips` list.}
#'   \item{edgelist}{A `data.table`. The network edge list of the directed
#'   graph. The `origin` column is the node establishing the connection.
#'   The `destination` column is the node accepting the connection. The
#'   `index` column of the `nodes` `data.table` is used as the identifier.}
#'   \item{network.stats}{A list of four network summary statistics, computed
#'   by the `igraph` package: `centr_betw`, `centr_clo`, `centr_degree`,
#'   and `centr_eigen`. See their documentation in the `igraph` package
#'   for interpretation.}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' data(good_peers)
#'
#' good_peers <- stringr::str_extract(good_peers,
#'   "[0-9]{1,3}[.][0-9]{1,3}[.][0-9]{1,3}[.][0-9]{1,3}")
#' good_peers <- unique(good_peers)
#' good_peers <- na.omit(good_peers)
#' # Clean IP addresses
#'
#' data(ban_list_v2)
#'
#' future::plan(future::multisession,
#'   workers = max(c(1, floor(parallelly::availableCores()/6))))
#' # Multi-threaded is recommended
#'
#' share.reachable <- 0.2
#' # Set share of nodes that are reachable to 20 percent
#'
#' n.assumed.unreachable <- floor(length(good_peers) *
#'     ((1 - share.reachable) / share.reachable))
#'
#' set.seed(314)
#' # This is a random simulation
#'
#' generated.network <- gen.network(outbound.ips = good_peers,
#'   malicious.ips = ban_list_v2,
#'   n.unreachable = n.assumed.unreachable)
#'
#' hist(generated.network$nodes[
#'   reachable == TRUE & malicious == FALSE, n.inbound], breaks = 50)
#'
#' # Network stats
#' sapply(names(generated.network$network.stats), function(x) {
#'   generated.network$network.stats[[x]]$centralization
#' })
#' }
gen.network <- function(
  outbound.ips,
  malicious.ips = NULL,
  n.unreachable = 0,
  already.connected.subnet.level = 16,
  deduplication.subnet.level = 24,
  do.deduplication = TRUE,
  default.outbound.connections = 12,
  dropped.connection.churns = 12,
  compute.network.stats = TRUE) {

  if (any(duplicated(outbound.ips))) {
    stop("Duplicate IP addresses present in outbound.ips")
  }

  if (any(duplicated(malicious.ips))) {
    stop("Duplicate IP addresses present in malicious.ips")
  }

  malicious.ips <- outbound.ips[in.ip.set(outbound.ips, malicious.ips)]

  simulated.nodes <- data.table(ip = outbound.ips,
    already.connected.subnet =
      paste0(as.subnet(outbound.ips, already.connected.subnet.level),
        "/", already.connected.subnet.level),
    deduplication.subnet =
      paste0(as.subnet(outbound.ips, deduplication.subnet.level),
        "/", deduplication.subnet.level),
    reachable = TRUE)


  simulated.nodes <- rbind(simulated.nodes,
    data.table(
      ip = rep(NA, n.unreachable),
      already.connected.subnet = rep(NA, n.unreachable),
      deduplication.subnet = rep(NA, n.unreachable),
      reachable = rep(FALSE, n.unreachable))
    # Explicitly rep() means that when n.unreachable = 0, an empty
    # table is produced
  )

  simulated.nodes[, malicious := ip %in% malicious.ips]

  simulated.nodes[, index := seq_len(.N)]

  simulated.nodes.reachable <- simulated.nodes[reachable == TRUE, ]
  non.malicious.indices <- simulated.nodes[malicious == FALSE, index]
  # The malicious IPs do not establish outbound connections

  message("Simulating node connection algorithm...")

  simulated.nodes.connections <- future.apply::future_lapply(non.malicious.indices, FUN = function(i) {

    result <- integer(default.outbound.connections)

    choose.peer <- function(result) {
      possible.connections <- simulated.nodes.reachable[
        ! already.connected.subnet %in% already.connected.subnet[result],
      ]
      # This ^ will also prevent connecting to a specific IP address that we
      # are already connected to, so there are no duplicate connections.
      if (do.deduplication) {
        possible.connections <- possible.connections[sample(.N), ]
        possible.connections <- unique(possible.connections, by = "deduplication.subnet")
      }
      sample(possible.connections$index, 1)
    }

    for (j in seq_len(default.outbound.connections)) {
      result[j] <- choose.peer(result)
    }

    for (churn.iter in seq_len(dropped.connection.churns)) {
      dropped.connection  <- sample(default.outbound.connections, 1)
      result[dropped.connection] <- NA
      result[dropped.connection] <- choose.peer(result)

    }

    if (i %% 100 == 0 || i %in% c(1, max(non.malicious.indices))) {
      # Sometimes this doesn't hit because i is a malicious node
      message("\r", "Choosing connections for node ", i, "/", max(non.malicious.indices), "", appendLF = FALSE)
    }

    result
  }, future.seed = TRUE,
    future.chunk.size = floor(nrow(simulated.nodes)/(10 * future::nbrOfWorkers())),
    future.packages = "data.table",
    future.globals = c("simulated.nodes.reachable", "dropped.connection.churns",
      "default.outbound.connections", "malicious.ips", "do.deduplication",
      "non.malicious.indices"))


  origin.indices <- lengths(simulated.nodes.connections)

  simulated.nodes.edgelist <- data.table(
    origin = rep(non.malicious.indices, origin.indices),
    destination = unlist(simulated.nodes.connections)
  )


  simulated.nodes.connection.count <- as.data.table(table(simulated.nodes.edgelist$destination))

  setnames(simulated.nodes.connection.count, c("index", "n.inbound"))
  simulated.nodes.connection.count[, index := as.integer(index)]

  simulated.nodes <- merge(simulated.nodes, simulated.nodes.connection.count,
    all.x = TRUE)

  simulated.nodes[is.na(n.inbound), n.inbound := 0]


  simulated.nodes.edgelist[, malicious.connection :=
      destination %in% simulated.nodes[malicious == TRUE, index] ]

  malicious.connection.count <- simulated.nodes.edgelist[,
    .(n.malicious.outbound = sum(malicious.connection)), by = "origin"]

  simulated.nodes.edgelist[, malicious.connection := NULL]

  setnames(malicious.connection.count, c("index", "n.malicious.outbound"))

  simulated.nodes <- merge(simulated.nodes, malicious.connection.count,
    all.x = TRUE)

  simulated.nodes[is.na(n.malicious.outbound), n.malicious.outbound := 0]


  result <- list(nodes = simulated.nodes, edgelist = simulated.nodes.edgelist)


  if (compute.network.stats) {

    message("\nComputing network stats...")

    network.stats <- list()

    simulated.nodes.igraph <- igraph::graph_from_edgelist(as.matrix(simulated.nodes.edgelist), directed = TRUE)
    network.stats$centr_betw <- igraph::centr_betw(simulated.nodes.igraph, directed = FALSE)
    network.stats$centr_clo <- igraph::centr_clo(simulated.nodes.igraph, mode = "all")

    if (any(! is.finite(network.stats$centr_clo$res))) {
      # Sometimes there are NaNs in the individual "res" node stats
      network.stats$centr_clo$centralization <- igraph::centralize(na.omit(network.stats$centr_clo$res),
        theoretical.max = network.stats$centr_clo$theoretical_max)
    }

    network.stats$centr_degree <- igraph::centr_degree(simulated.nodes.igraph, mode = "all")
    network.stats$centr_eigen <- igraph::centr_eigen(simulated.nodes.igraph, directed = FALSE)

    result$network.stats <- network.stats

  }

  result

}









