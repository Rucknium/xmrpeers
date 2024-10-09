




#' Parse p2p log files
#'
#' @param bitmonero.dir Directory location of the log files
#' @param output.file Optional file to output compressed data
#'
#' @return
#' data.frame of each transaction gossip message for each peer.
#' @export
#'
#' @examples
#' \dontrun{
#' get.p2p.log()
#' }
get.p2p.log <- function(bitmonero.dir = "~/.bitmonero", output.file = NULL) {

  # Sys.setenv(VROOM_CONNECTION_SIZE = formatC(131072 * 10000, format = "fg"))
  # Fixes occasional issues with https://github.com/tidyverse/vroom/issues/364
  # formatC() is used since VROOM_CONNECTION_SIZE cannot interpret scientific notation

  bitmonero.dir <- path.expand(bitmonero.dir)
  bitmonero.dir <- gsub("/+$", "", bitmonero.dir)
  # Remove trailing slash(es) if they exist

  files.in.dir <- list.files(bitmonero.dir)
  bitmonero.files <- files.in.dir[grepl("(^bitmonero[.]log)|(^monero[.]log)", files.in.dir, ignore.case = TRUE)]
  # This one does not have "$" at the end of each string like ping.peers()
  # since we want all log files

  get.time <- function(x) {
    x <- stringr::str_extract(x, "^[0-9]{4}-[0-9]{2}-[0-9]{2}\\s[0-9]{2}:[0-9]{2}:[0-9]{2}\\S*")
    as.POSIXct(strptime(x, format = "%Y-%m-%d %H:%M:%OS"))
  }

  cat(base::date(), " Reading ", length(bitmonero.files), " log files...\n", sep = "")

  monero.log <- lapply(bitmonero.files, function(x) {
    x <- data.table::fread(paste0(bitmonero.dir, "/", x),
      header = FALSE, sep = NULL, blank.lines.skip = FALSE)[[1]]
    # Do [[1]] since data.table::fread() produces a data.frame. We want a vector.
    times <- get.time(x)
    x[!is.na(times)]
  })
  # Doing get.time() and excluding invalid lines prevents problems when
  # re-reading the data to monero.log.timing
  monero.log.timing <- lapply(seq_along(monero.log), function(x) {
    suppressWarnings(readr::read_tsv(I(monero.log[[x]]), col_names = c("time", "thread"),
      col_types = c("T", "c"), col_select = 1:2, skip_empty_rows = FALSE))
    # skip_empty_rows = FALSE is very important for this will not align with monero.log
    # Do not re-read the file from storage. Must wrap in I()
    # suppresswarnings() because we get
    # # One or more parsing issues, call `problems()` on your data frame for details, e.g.:
    # # dat <- vroom(...)
    # # problems(dat)
    # but we cannot view the problem beacuse: https://github.com/tidyverse/readr/issues/1477
    # It is probably because a few lines have more than the normal number of tab characters.
    # It likely has no effect since we only get the first two columns.
  })

  beginning.log.times <- vector("numeric", length(monero.log))

  for (i in seq_along(beginning.log.times)) {
    if (grepl("^[0-9]{4}-", monero.log[[i]][1])) {
      first.date.line <- 1
    } else {
      first.date.line <- grep("^[0-9]{4}-", monero.log[[i]])[1]
      # Get first appearance of a date in the log file
    }
    beginning.log.times[i] <- get.time(monero.log[[i]][first.date.line])
  }

  monero.log <- unlist(monero.log[order(beginning.log.times)])
  # Put monero logs in correct order and concatenate them

  monero.log.timing <- as.data.frame(do.call(rbind, monero.log.timing[order(beginning.log.times)]))

  n.out.of.order <- sum(monero.log.timing$time[-nrow(monero.log.timing)] <= monero.log.timing$time[-1])

  if (! n.out.of.order > 0) {
    warning(paste0(n.out.of.order, " log lines were not originally in the correct time order. Probably this\n is fixed automatically."))
  }

  keep.rows <- (! duplicated(monero.log)) & complete.cases(monero.log.timing)
  monero.log <- monero.log[keep.rows]
  monero.log.timing <- monero.log.timing[keep.rows, ]
  monero.log <- monero.log[order(monero.log.timing$thread, monero.log.timing$time)]
  rm(monero.log.timing, keep.rows)
  # Log messages from different peers can sometimes print at roughly the same time.
  # We need to link the tx_hash of each log line to its NOTIFY_NEW_TRANSACTIONS
  # line that has the IP address of the peer. So we will order the log lines
  # first by thread number and then by time so that each set of tx_hashes
  # is kept together.
  # moneromooo: "Use the thread column to disentangle."
  # https://libera.monerologs.net/monero-dev/20240317#c348611

  cat(base::date(), " Finished reading ", length(bitmonero.files), " log files.\n", sep = "")

  monero.log <- monero.log[grepl("net.p2p.msg", monero.log, fixed = TRUE)]

  # TODO: Must do the fluffy blocks before this

  monero.log <- monero.log[
    grepl("Received NOTIFY_NEW_TRANSACTIONS", monero.log, fixed = TRUE) |
      grepl("Including transaction", monero.log, fixed = TRUE)
  ]

  notify.lines <- grep("Received NOTIFY_NEW_TRANSACTIONS", monero.log, fixed = TRUE)

  get.number.of.txs <- function(x) {
    x <- stringr::str_extract(x, "([0-9]+) txes[)]", group = 1)
    as.numeric(x)
  }

  number.of.txs <- get.number.of.txs(monero.log[notify.lines])

  get.tx.hash <- function(x) {
    stringr::str_extract(x, "Including transaction <([:xdigit:]{64})>", group = 1)
  }

  tx.hashes <- get.tx.hash(monero.log)

  rle.result <- rle(!is.na(tx.hashes))
  # The "Including transaction <tx_hash>" will not be printed if the tx fails
  # the cryptonote::parse_and_validate_tx_from_blob() check or if the peer sends
  # duplicate txs in the message:
  # https://github.com/monero-project/monero/blob/c8214782fb2a769c57382a999eaf099691c836e7/src/cryptonote_protocol/cryptonote_protocol_handler.inl#L990
  # https://github.com/monero-project/monero/blob/c8214782fb2a769c57382a999eaf099691c836e7/src/cryptonote_basic/cryptonote_format_utils.cpp#L224
  # Therefore, we compute the "run length" of whether there are tx_hashes in the
  # log line. (If it does not have the tx_hash, then it must be the
  # "NOTIFY_NEW_TRANSACTIONS" message because all other lines were exlcuded above.)
  # The run length is used to create the final output data.

  for (i in which( (! rle.result$values) & rle.result$lengths > 1)) {
    # This fixes the rare case that the number of txs received in a message
    # is zero, or if all the txs in a message did not pass validation
    # so none of them were printed to the log.
    rle.result$lengths <- append(rle.result$lengths, 0, after = i)
    rle.result$values <- append(rle.result$values, TRUE, after = i)
  }

  cat(base::date(), " ", sum(rle.result$lengths[rle.result$values] != number.of.txs),
    " log messages not aligned. Repairing...\n", sep = "")

  run.lengths <- rle.result$lengths[rle.result$values]
  # When "values" is TRUE

  number.of.txs.corrected<- run.lengths

  peer_ip_port <- get.peer.ip.port.direction(monero.log[notify.lines], "Received NOTIFY_NEW_TRANSACTIONS")

  tx.data <- data.frame(
    time = rep(get.time(monero.log[notify.lines]), times = number.of.txs.corrected + 1),
    # Do not get the actual time of the log message. Get the time that the
    # notify message was received.
    gossip.msg.id = rep(1:length(notify.lines), times = number.of.txs.corrected + 1),
    tx.hash = get.tx.hash(monero.log),
    stringsAsFactors = FALSE)

  stopifnot( all(diff(tx.data[tx.data$number.of.txs.corrected > 0, "gossip.msg.id"])[is.na(tx.data[tx.data$number.of.txs.corrected > 0, "tx.hash"])[-1]] == 1) )
  # TODO: Make this syntax clearer
  # Checks to make sure each transmission of txs from each node has its
  # own gossip.msg.id, i.e. that there is no misalignment in the data.frame

  tx.data <- cbind(tx.data, peer_ip_port[tx.data$gossip.msg.id, ])

  tx.data <- tx.data[!is.na(tx.data$tx.hash), ]
  # Remove rows that were the NOTIFY_NEW_TRANSACTIONS message
  rownames(tx.data) <- NULL

  if (!is.null(output.file)) {
    cat(base::date(), " Writing data to ", output.file, " with highest compression settings...\n", sep = "")
    qs::qsave(tx.data, output.file, preset = "custom", algorithm = "zstd_stream", compress_level = 22, shuffle_control = 15)
  }

  tx.data

}












#' Extract and compress selected log lines
#'
#' @description
#' With default arguments, this function accomplishes the same thing as this
#' Linux shell call:
#' ```
#' grep -I "net.p2p.msg" bitmonero.log* > extracted-xmr-log
#' for file in bitmonero.log*.tar.gz; do tar xf "$file" --to-command 'egrep -Ha --label="$TAR_FILENAME" "net.p2p.msg" || true' >> extracted-xmr-log; done
#' xz extracted-xmr-log
#' ```
#'
#'
#' @param bitmonero.dir Directory location of the log files
#' @param output.file Name of the file that will be created. ".tar.xz" will
#' be appended to this name
#' @param log.filter Regular expression that selects log lines.
#' @param rm.uncompressed.file Remove the uncompressed version of the
#' extracted log file after compression? Does not affect the original log files.
#'
#' @return
#' NULL (invisible)
#' @export
#'
#' @examples
#' \dontrun{
#' compress.log()
#' }
compress.log <- function(bitmonero.dir = c("~/.bitmonero", "C:\\ProgramData\\bitmonero"),
  output.file = "extracted-xmr-log", log.filter = "net.p2p.msg", rm.uncompressed.file = TRUE) {

  # https://monero.stackexchange.com/questions/1146/where-is-the-blockchain-saved-in-windows
  if (identical(bitmonero.dir, c("~/.bitmonero", "C:\\ProgramData\\bitmonero"))) {
    if ( ! (.Platform$OS.type %in% c("unix", "windows"))) {
      stop("Operating system is neither Unix nor Windows. Please manually specify Monero log directory with bitmonero.dir argument.")
    }
    if ( .Platform$OS.type == "unix" ) {
      bitmonero.dir <- "~/.bitmonero"
    }
    if ( .Platform$OS.type == "windows" ) {
      bitmonero.dir <- "C:\\ProgramData\\bitmonero"
    }
  }


  if (file.exists(output.file)) {
    stop(paste0("Uncompressed ", output.file, " file already exists. Delete or move file and retry."))
  }

  if (rm.uncompressed.file) {
    on.exit(unlink(output.file))
  }

  bitmonero.dir <- path.expand(bitmonero.dir)
  bitmonero.dir <- gsub("/+$", "", bitmonero.dir)
  # Remove trailing slash(es) if they exist

  files.in.dir <- list.files(bitmonero.dir)
  bitmonero.files <- files.in.dir[grepl("(^bitmonero[.]log)|(^monero[.]log)", files.in.dir, ignore.case = TRUE)]

  if (length(bitmonero.files) == 0) {
    stop(paste0("No Monero log files (bitmonero.log* or monero.log*) found in ", bitmonero.dir))
  }

  bitmonero.files.tar <- bitmonero.files[grepl("[.]tar[.]gz$", bitmonero.files)]
  bitmonero.files <- bitmonero.files[!grepl("[.]tar[.]gz$", bitmonero.files)]
  cat("Processing ", length(bitmonero.files), " uncompressed log file(s) and ",
    length(bitmonero.files.tar), " compressed archive(s).\n", sep = "")

  for (tar.file in bitmonero.files.tar) {
    # This loop will be skipped if there are zero tar files
    gz.files <- untar(paste0(bitmonero.dir, "/", tar.file), list = TRUE)
    cat("Processing ", length(gz.files), " log files in ", tar.file, "...\n", sep = "")
    for (gz.file in gz.files) {
      cat("Processing log file ", gz.file, " from ", tar.file, "...\n", sep = "")
      if ("archive" %in% installed.packages()[, 1]) {
        gz.connection <- archive::archive_read(paste0(bitmonero.dir, "/", tar.file), file = gz.file)
        gz.output <- readLines(gz.connection, skipNul = TRUE)
        # Cannot use data.table::fread() with a connection: https://github.com/Rdatatable/data.table/issues/561
        data.table::fwrite(list(paste0(gz.file, ":", gz.output[grep(log.filter, gz.output)])),
          file = output.file, append = TRUE, quote = FALSE, col.name = FALSE, showProgress = FALSE)
        rm(gz.output)
        close(gz.connection)
        # Close connection
      } else {
        tmp <- tempdir()
        untar(paste0(bitmonero.dir, "/", tar.file), files = gz.file, exdir = tmp)
        gz.output <- data.table::fread(paste0(bitmonero.dir, "/", gz.file), colClasses = "character",
          header = FALSE, sep = NULL, blank.lines.skip = FALSE, showProgress = FALSE)[[1]]
        data.table::fwrite(list(paste0(gz.file, ":", gz.output[grep(log.filter, gz.output)])),
          file = output.file, append = TRUE, quote = FALSE, col.name = FALSE, showProgress = FALSE)
        rm(gz.output)
        unlink(paste0(tmp, "/", gz.file))
      }
    }
  }
  gc()

  for (bitmonero.file in bitmonero.files) {
    cat("Processing log file ", bitmonero.file, "...\n", sep = "")
    txt.output <- data.table::fread(paste0(bitmonero.dir, "/", bitmonero.file), colClasses = "character",
      header = FALSE, sep = NULL, blank.lines.skip = FALSE, showProgress = FALSE)[[1]]
    data.table::fwrite(list(paste0(bitmonero.file, ":", txt.output[grep(log.filter, txt.output)])),
      file = output.file, append = TRUE, quote = FALSE, col.name = FALSE, showProgress = FALSE)
    rm(txt.output)
  }
  gc()
  output.file.size <- file.size(output.file)

  cat("Compressing ", output.file, " (", round(output.file.size / 10^9, 2),
    " GB)...\n", sep = "")
  tic <- Sys.time()

  xzcompress(output.file, overwrite = TRUE, remove = rm.uncompressed.file, BFR.SIZE = 1e+08)
  # archive::archive_write_files() is multi-threaded, but it requires the .xz
  # to be put in an archive that can save file metadata like creation time and
  # user account
  time.diff <- Sys.time() - tic
  cat("Compression from ", round(output.file.size / 10^9, 2), " GB to ",
    round(file.size(paste0(output.file, ".xz")) / 10^6, 2), " MB completed in ",
    round(time.diff, 2), " ", attr(time.diff, "units"),
    ".\nFile is ", output.file, ".xz in ", getwd(), "\n", sep = "")

  return(invisible(NULL))
}







# xzcompress function copied from micropan R package. Modified this line:
# nbytes <- 0L
# to
# nbytes <- 0
# and added this for a progress indicator:
# cat( formatC(round(nbytes / 10^9, 1), digits = 1, flag = "#"), " GB compressed...\r", sep = "")
# GPL-2 license, Copyright 2022 Lars Snipen and Kristian Hovde Liland

xzcompress <- function (filename, destname = sprintf("%s.xz", filename), temporary = FALSE,
  skip = FALSE, overwrite = FALSE, remove = TRUE, BFR.SIZE = 1e+07,
  compression = 6, ...)
{
  if (!file.exists(filename)) {
    stop("No such file: ", filename)
  }
  if (temporary) {
    destname <- file.path(tempdir(), basename(destname))
  }
  attr(destname, "temporary") <- temporary
  if (filename == destname)
    stop(sprintf("Argument 'filename' and 'destname' are identical: %s",
      filename))
  if (file.exists(destname)) {
    if (skip) {
      return(destname)
    }
    else if (!overwrite) {
      stop(sprintf("File already exists: %s", destname))
    }
  }
  destpath <- dirname(destname)
  if (!file.info(destpath)$isdir)
    dir.create(destpath)
  inn <- file(filename, open = "rb")
  on.exit(if (!is.null(inn)) close(inn))
  outComplete <- FALSE
  out <- xzfile(destname, open = "wb", compression = compression,
    ...)
  on.exit({
    close(out)
    if (!outComplete) {
      file.remove(destname)
    }
  }, add = TRUE)
  nbytes <- 0 # Make nbytes a numeric (float), not integer, to avoid overflow
  repeat {
    bfr <- readBin(inn, what = raw(0L), size = 1L, n = BFR.SIZE)
    n <- length(bfr)
    if (n == 0L)
      break
    nbytes <- nbytes + n
    writeBin(bfr, con = out, size = 1L)
    bfr <- NULL
    cat( formatC(round(nbytes / 10^9, 1), width = 2, digits = 1, flag = "#",
      format = "f"), " GB compressed...\r", sep = "")
    # Added this. \r is to delete previous line.
  }
  outComplete <- TRUE
  if (remove) {
    close(inn)
    inn <- NULL
    file.remove(filename)
  }
  attr(destname, "nbrOfBytes") <- nbytes
  invisible(destname)
}






# Suggested by
# https://r-pkgs.org/dependencies-in-practice.html#how-to-not-use-a-package-in-imports
# We need the R.utils package to enable reading compressed log file with
# data.table::fread(), but we do not explicitly load it anywhere. R CMD check
# creates a NOTE if it is not loaded anywhere
ignore_unused_imports <- function() {
  R.utils::decompressFile
}




