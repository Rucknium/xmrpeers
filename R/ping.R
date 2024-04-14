


#' Ping peer nodes for latency measurement
#'
#' @param bitmonero.dir .bitmonero directory where the monero.log file is.
#' @param output.file Name of the output file. The file will be created in `bitmonero.dir`.
#' @param sleep Number of seconds to sleep between each round of collecting new peer IPs.
#' @param ping.count Number of times to ping each peer.
#'
#' @return No return value. Executes in a loop until interrupted.
#' @export
#'
#' @examples
#' ping.peers()
ping.peers <- function(bitmonero.dir = "~/.bitmonero", output.file = "/monero_peer_pings.csv", sleep = 10, ping.count = 5) {

  bitmonero.dir <- path.expand(bitmonero.dir)
  bitmonero.dir <- gsub("/+$", "", bitmonero.dir)
  # Remove trailing slash(es) if they exist

  output.file <- paste0(bitmonero.dir, output.file)

  log.file <- paste0(bitmonero.dir, "/bitmonero.log")

  first.file.line <- readr::read_lines(log.file, n_max = 1)
  # Get the first file line so we know when the log file rolls over
  # to the next file.
  # Better to do this than file.info() because file.info() is OS-dependent:
  # > What is meant by the three file times depends on the OS and file system.
  # > On Windows native file systems ctime is the file creation time
  # > (something which is not recorded on most Unix-alike file systems).
  # > What is meant by ‘file access’ and hence the ‘last access time’ is system-dependent.



  lines.already.read <- 0

  while (TRUE) {

    check.first.file.line <- readr::read_lines(log.file, n_max = 1)

    if (first.file.line != check.first.file.line) {
      first.file.line <- check.first.file.line
      lines.already.read <- 0
    }

    if (file.exists(output.file)) {
      old.ping.data <- read.csv(output.file, header = FALSE)
      old.ip.ports <- paste0(old.ping.data[, 1], ":", old.ping.data[, 2])
    } else {
      old.ip.ports <- ""
    }


    tail.file <- readr::read_lines(log.file, skip = lines.already.read)

    ip.lines <- grep("Received NOTIFY_NEW_TRANSACTIONS", tail.file, fixed = TRUE)

    if (length(ip.lines) == 0) {
      Sys.sleep(sleep.time)
      cat(base::date(), " Peers pinged: 0\n", sep = "")
      next
    }

    get.peer.ip.port.direction <- function(x) {
      x <- stringr::str_extract(x, "\\[[0-9]{1,3}[.][0-9]{1,3}[.][0-9]{1,3}[.][0-9]{1,3}:[0-9]+\\s[INCOUT]{3}\\]")
      x <- stringr::str_extract(x, "[0-9]{1,3}[.][0-9]{1,3}[.][0-9]{1,3}[.][0-9]{1,3}:[0-9]+\\s[INCOUT]{3}")
      x <- strsplit(x, "(:)|(\\s)")
      x <- as.data.frame(matrix(unlist(x), ncol = 3, byrow = TRUE), stringsAsFactors = FALSE)
      colnames(x) <- c("ip", "port", "direction")
      x
    }


    peers <- get.peer.ip.port.direction(tail.file[ip.lines])
    peers <- unique(peers)
    peers <- peers[! paste0(peers$ip, ":", peers$port) %in% old.ip.ports, , drop = FALSE]

    if (nrow(peers) == 0) {
      Sys.sleep(sleep.time)
      cat(base::date(), " Peers pinged: 0\n", sep = "")
      next
    }

    get.ping.data <- function(x) {
      ip <- x[[1]]
      port <- x[[2]]
      direction <- x[[3]]
      if (paste0(ip, ":", port) %in% old.ip.ports) {
        return("")
      }
      pings <- pingr::ping_port(ip, port = port, count = ping.count)
      if (all(is.na(pings))) {
        # This may happen if it is an incoming connection and the peer's port is closed
        pings <- pingr::ping(ip, count = ping.count)
      }
      paste(ip, port, direction, paste(pings, collapse = ","), sep = ",")
    }


    if (nrow(peers) * ping.count > 5) {

      n.workers <- min(c(floor(nrow(peers) * ping.count / 5), parallelly::availableCores()*4))
      options(parallelly.maxWorkers.localhost = 4) # This means number of CPU cores times 4
      # Most time in thread is waiting for ping to return, so can have
      # high number of workers

      future::plan(future::multisession(workers = n.workers))

      ping.data <- future.apply::future_apply(peers, MARGIN = 1, get.ping.data, future.seed = TRUE)

      future::plan(future::sequential)
      # Shut down workers

    } else {
      ping.data <- apply(peers, MARGIN = 1, get.ping.data)
    }

    ping.data <- unname(ping.data)

    ping.data <- ping.data[ping.data != ""]

    if (length(ping.data) > 0) {
      cat(ping.data, file = output.file, sep = "\n", append = TRUE)
    }

    lines.already.read <- n.lines.file

    Sys.sleep(sleep.time)

    cat(base::date(), " Peers pinged: ", length(ping.data), "\n", sep = "")

  }


}



