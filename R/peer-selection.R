

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

    considering.connecting <- grep("Considering connecting", log.output)

    if (length(considering.connecting) > 0) {

      draws.from.white_list <- grep("white list peer", log.output)
      draws.from.gray_list <- grep("gray list peer", log.output)

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

      if (verbose == 2) {
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


    max_index <- 19 # TODO: Or 20?
    x <- 0:(16 * max_index)
    parabolic.probability <- c(unclass(prop.table(table(floor((x*x*x)/(max_index*max_index*16*16*16))))))
    # From get_random_index_with_fixed_probability() function:
    # https://github.com/monero-project/monero/blob/master/src/p2p/net_node.inl#L1302-L1312


    simulated.probability <- future.apply::future_apply(white_list, 1, FUN = function(x) {

      peers <- data.table(host = na.omit(x[grepl("peer", names(x))]))
      peers[, host := gsub("::ffff:", "", host, fixed = TRUE)]
      # Delete the leading ffff
      # https://www.ibm.com/docs/en/i/7.6.0?topic=concepts-ipv6-address-formats
      # IPv4-mapped IPv6 address uses this alternative format. This type of
      # address is used to represent IPv4 nodes as IPv6 addresses. It allows
      # IPv6 applications to communicate directly with IPv4 applications. For
      # example, 0:0:0:0:0:ffff:192.1.56.10 and ::ffff:192.1.56.10/96 (shortened format).
      connections <- na.omit(x[grepl("connection", names(x))])
      peers <- peers[ ! host %chin% connections, ]
      # Remove peers that node is already connected to
      peers[, subnet.AC := convert.to.subnet(host, already.connected.exclusion.subnet.level)]
      peers <- peers[! subnet.AC %chin%
          convert.to.subnet(connections, already.connected.exclusion.subnet.level), ]
      # Remove peers in subnets that the node is already connected to
      peers[, subnet.deduplicated := convert.to.subnet(host, deduplicated.subnet.level)]
      peers[, orig.order := seq_len(.N)]

      result <- replicate(white_list.monte.carlo.iters, {
        peers <- peers[sample(.N), ]
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

    simulated.probability <- future.apply::future_apply(gray_list, 1, FUN = function(x) {

      peers <- data.table(host = na.omit(x[grepl("peer", names(x))]))
      peers[, host := gsub("::ffff:", "", host, fixed = TRUE)]
      # Delete the leading ffff
      # https://www.ibm.com/docs/en/i/7.6.0?topic=concepts-ipv6-address-formats
      # IPv4-mapped IPv6 address uses this alternative format. This type of
      # address is used to represent IPv4 nodes as IPv6 addresses. It allows
      # IPv6 applications to communicate directly with IPv4 applications. For
      # example, 0:0:0:0:0:ffff:192.1.56.10 and ::ffff:192.1.56.10/96 (shortened format).
      connections <- na.omit(x[grepl("connection", names(x))])
      peers <- peers[ ! host %chin% connections, ]
      # Remove peers that node is already connected to
      peers[, subnet.AC := convert.to.subnet(host, already.connected.exclusion.subnet.level)]
      peers <- peers[! subnet.AC %chin%
          convert.to.subnet(connections, already.connected.exclusion.subnet.level), ]
      # Remove peers in subnets that the node is already connected to
      peers[, subnet.deduplicated := convert.to.subnet(host, deduplicated.subnet.level)]
      peers[, probability := 1/.N, by = "subnet.deduplicated"]
      result <- peers[, .(host = host, probability = probability)]
      result[, probability := probability / sum(probability)]
      result

    }, simplify = FALSE)

    simulated.probability <- rbindlist(simulated.probability)
    simulated.probability <- simulated.probability[, .(probability = sum(probability)), by = "host"]
    simulated.probability[, probability := probability / sum(probability)]

    # new.connections <- unique(
    #   convert.to.subnet(gray_list$new.connection, deduplicated.subnet.level))
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











