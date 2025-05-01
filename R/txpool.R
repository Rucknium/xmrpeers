


#' Initialize txpool archive database
#'
#' @description Initializes a database file to record the arrival time of
#' transactions to the local Monero node.
#'
#' @param db.file Name and path of database file to be created.
#'
#' @return
#' NULL (invisible)
#' @seealso [txpool.collect], which collects txpool data and saves it to the
#' database file and [txpool.export], which exports the database contents to a
#' CSV file.
#' @export
#'
#' @examples
#' \dontrun{
#' txpool.init()
#' }

txpool.init <- function(db.file = "xmr-txpool-archive.db") {

  if (file.exists(db.file)) {
    stop(paste0("File already exists at ", db.file, "\n Aborting new database file creation."))
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), db.file)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "PRAGMA journal_mode=WAL;")
  # txpool.export() can read while txpool.collect() writes
  # https://stackoverflow.com/questions/15143871/simplest-way-to-retry-sqlite-query-if-db-is-locked

  DBI::dbExecute(con,
"CREATE TABLE txs (
id_hash TEXT,
fee TEXT,
weight TEXT,
receive_time TEXT,
key_images TEXT,
unique(id_hash)
)")
  # unique(id_hash) prevents the same txs being inserted more than once

  DBI::dbExecute(con,
"CREATE TABLE blocks (
block_hash TEXT,
prev_block_hash TEXT,
block_height TEXT,
block_timestamp TEXT,
block_receive_time TEXT,
unique(block_hash)
)")

  if (file.exists(paste0(getwd(), "/", db.file))) {
    db.filepath <- paste0(getwd(), "/", db.file)
  }

  if (file.exists(db.file)) {
    # This would happen if the user gave a full filepath
    db.filepath <- db.file
  }

  if (! exists("db.filepath")) {
    stop("Database file failed to be created or it cannot be found in the file system.")
  }

  message(paste0("Database file successfully created at ", db.filepath))

  return(invisible(NULL))

}



#' Collect txpool archive data
#'
#' @description Queries the local Monero node for its txpool once per second.
#' The time of arrival of each transaction is saved to a database file. The
#' database file must first be created by [txpool.init]. This function executes
#' an infinite loop. Input `ctrl + c` to interrupt the function.
#'
#' @param db.file Name and path of database file created by [txpool.init].
#' @param unrestricted.rpc.url URL and port of the `monerod` unrestricted RPC.
#' Default is `http://127.0.0.1:18081`
#'
#' @return
#' NULL (invisible)
#' @seealso [txpool.init], which create the database file and [txpool.export],
#' which exports the database contents to a CSV file.
#' @export
#'
#' @examples
#' \dontrun{
#' txpool.collect()
#' }

txpool.collect <- function(db.file = "xmr-txpool-archive.db",
  unrestricted.rpc.url = "http://127.0.0.1:18081") {

  if (! file.exists(db.file)) {
    stop(paste0("Database file ", db.file, " does not exist. Please run txpool.init() first."))
  }

  # Modified from TownforgeR::tf_rpc_curl function
  xmr.rpc <- function(
    url.rpc = "http://127.0.0.1:18081/json_rpc",
    method = "",
    params = list(),
    userpwd = "",
    num.as.string = TRUE,
    nonce.as.string = FALSE,
    keep.trying.rpc = FALSE,
    ...
  ){

    json.ret <- RJSONIO::toJSON(
      list(
        jsonrpc = "2.0",
        id = "0",
        method = method,
        params = params
      ), digits = 50
    )

    rcp.ret <- 	tryCatch(RCurl::postForm(url.rpc,
      .opts = list(
        userpwd = userpwd,
        postfields = json.ret,
        httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')
        # https://stackoverflow.com/questions/19267261/timeout-while-reading-csv-file-from-url-in-r
      )
    ), error = function(e) {NULL})

    if (keep.trying.rpc && length(rcp.ret) == 0) {
      while (length(rcp.ret) == 0) {
        rcp.ret <- 	tryCatch(RCurl::postForm(url.rpc,
          .opts = list(
            userpwd = userpwd,
            postfields = json.ret,
            httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')
            # https://stackoverflow.com/questions/19267261/timeout-while-reading-csv-file-from-url-in-r
          )
        ), error = function(e) {NULL})
      }
    }

    if (is.null(rcp.ret)) {
      stop("Cannot connect to monerod. Is monerod running?")
    }

    if (num.as.string) {
      rcp.ret <- gsub("(: )([-0123456789.]+)([,\n\r])", "\\1\"\\2\"\\3", rcp.ret )
    }

    if (nonce.as.string & ! num.as.string) {
      rcp.ret <- gsub("(\"nonce\": )([-0123456789.]+)([,\n\r])", "\\1\"\\2\"\\3", rcp.ret )
    }

    RJSONIO::fromJSON(rcp.ret) # , simplify = FALSE
  }

  tx.pool <- c()

  message("Checking that Monero node has acceptable configuration. Please wait for confirmation...")

  # Check that node is responding
  while(length(tx.pool) == 0) {
    tx.pool <- xmr.rpc(paste0(unrestricted.rpc.url, "/get_transaction_pool"), num.as.string = FALSE)$transactions

    if (length(tx.pool) > 0 && tx.pool[[1]]$receive_time == 0) {
      stop("Transaction receive_time is missing. Possible solution: remove '--restricted-rpc' monerod flag.")
    }
    Sys.sleep(1)
  }

  message("Monero node seems to have acceptable configuration. Initiating data collection!")

  con <- DBI::dbConnect(RSQLite::SQLite(), db.file)
  DBI::dbExecute(con, "PRAGMA journal_mode=WAL;")
  # txpool.export() can read while txpool.collect() writes
  # https://stackoverflow.com/questions/15143871/simplest-way-to-retry-sqlite-query-if-db-is-locked

  try(DBI::dbExecute(con, "ALTER TABLE txs ADD COLUMN key_images TEXT"), silent = TRUE)
  # This is in case there is an older version of the database without key_images

  on.exit({
    DBI::dbDisconnect(con)
    message(paste0("txpool data collection stopped at ", base::date()))
  })

  while (TRUE) {

    compute.time <- system.time({

      tx.pool <- xmr.rpc(paste0(unrestricted.rpc.url, "/get_transaction_pool"), num.as.string = FALSE, keep.trying.rpc = TRUE)$transactions

      block.header <- xmr.rpc(paste0(unrestricted.rpc.url, "/json_rpc"), method = "get_last_block_header", num.as.string = FALSE)$result$block_header

      block_receive_time <- round(Sys.time())
      # One-second time resolution

      if (length(tx.pool) > 0) {

        txs <- vector(mode = "list", length = length(tx.pool))

        for (i in seq_along(tx.pool)) {

          tx_json <- RJSONIO::fromJSON(tx.pool[[i]]$tx_json)
          key.images <- sapply(tx_json$vin, FUN = function(x) {x$key$k_image})
          key.images <- paste0(key.images, collapse = ";")

          txs[[i]] <- data.table::data.table(
            id_hash = tx.pool[[i]]$id_hash,
            fee = tx.pool[[i]]$fee,
            weight = tx.pool[[i]]$weight,
            receive_time = tx.pool[[i]]$receive_time,
            key_images = key.images)
        }

        txs <- data.table::rbindlist(txs)

        tx.statement <- DBI::dbSendQuery(con,
          "INSERT OR IGNORE INTO txs VALUES (:id_hash,:fee,:weight,:receive_time,:key_images)")
        # "IGNORE" prevents the same txs from being inserted more than once
        DBI::dbBind(tx.statement, params = txs)
        DBI::dbClearResult(tx.statement)

        blocks <- data.table::data.table(
          block_hash = block.header$hash,
          prev_block_hash = block.header$prev_hash,
          block_height = block.header$height,
          block_timestamp = block.header$timestamp,
          block_receive_time = as.character(as.numeric(block_receive_time))
        )

        block.statement <- DBI::dbSendQuery(con,
          "INSERT OR IGNORE INTO blocks VALUES (:block_hash,:prev_block_hash,:block_height,:block_timestamp,:block_receive_time)")
        # "IGNORE" prevents the same blocks from being inserted more than once
        DBI::dbBind(block.statement, params = blocks)
        DBI::dbClearResult(block.statement)

      }
    })

    loop.message <- paste0(format(Sys.time(), "%Y-%m-%d %T"), " ",
      formatC(nrow(txs), width = 4), " txs in txpool. Took ",
      sprintf("%.3f", round(compute.time["elapsed"], 3)),
      " seconds to execute txpool query.\n")
    cat(loop.message)
    Sys.sleep(max(c(0, 1 - compute.time["elapsed"])))
    # Should poll once per second unless data processing takes more than one second. In
    # that case, polls as frequently as possible.
  }

  return(invisible(NULL))

}




#' Export txpool archive to CSV
#'
#' @description Exports transaction and block arrival times from a database file
#' to a CSV file. Must use [txpool.init] and [txpool.collect] first.
#' [txpool.export] can be used while [txpool.collect] is still running in a
#' separate R session.
#'
#'
#' @param db.file File name/path of the txpool archive database.
#' @param csv.filepath File path of CSV file that will be created. Leave
#' default to save in current working directory.
#' @param begin.date Optional argument to restrict data export date range.
#' Use "YYYY-MM-DD" format.
#' @param end.date Optional argument to restrict data export date range.
#' Use "YYYY-MM-DD" format.
#'
#' @return
#' NULL (invisible)
#' @seealso [txpool.init], which create the database file and [txpool.collect],
#' which collects txpool data and saves it to the database file.
#' @export
#'
#' @examples
#' \dontrun{
#' txpool.export()
#' }

txpool.export <- function(db.file = "xmr-txpool-archive.db", csv.filepath = "",
  begin.date = "1970-01-01", end.date = "2035-01-01") {

  if (! file.exists(db.file)) {
    stop(paste0("Database file ", db.file, " does not exist."))
  }

  begin.date.filename <- ifelse(begin.date == "1970-01-01", "", paste0("begin-", begin.date, "-"))
  end.date.filename <- ifelse(end.date == "2035-01-01", "", paste0("end-" , end.date, "-"))

  con <- DBI::dbConnect(RSQLite::SQLite(), db.file)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "PRAGMA journal_mode=WAL;")
  # txpool.export() can read while txpool.collect() writes
  # https://stackoverflow.com/questions/15143871/simplest-way-to-retry-sqlite-query-if-db-is-locked

  file.time <- round(Sys.time())

  txs <- DBI::dbGetQuery(con, paste0(
    "SELECT * FROM txs WHERE receive_time >= ", as.integer(as.POSIXct(begin.date)),
    " AND receive_time <= ", as.integer(as.POSIXct(end.date)) + 24L * 60L * 60L
    # 24L * 60L * 60L to get the end of the day
  ))

  txs$receive_time_UTC <- as.POSIXct(as.integer(txs$receive_time), origin = "1970-01-01")

  txpool.filename <- paste0(csv.filepath, "xmr-txpool-archive-", begin.date.filename,
    end.date.filename, "exporttime-", gsub("( )|([:])", "-", file.time), ".csv")

  write.csv(txs, txpool.filename, row.names = FALSE)

  message("Successfully wrote txpool CSV to ", txpool.filename)

  blocks <- DBI::dbGetQuery(con, paste0(
    "SELECT * FROM blocks WHERE block_receive_time >= ", as.integer(as.POSIXct(begin.date)),
    " AND block_receive_time <= ", as.integer(as.POSIXct(end.date)) + 24L * 60L * 60L
    # 24L * 60L * 60L to get the end of the day
  ))

  blocks$block_receive_time_UTC <- as.POSIXct(as.integer(blocks$block_receive_time), origin = "1970-01-01")

  blocks.filename <- paste0(csv.filepath, "xmr-block-archive-", begin.date.filename,
    end.date.filename, "exporttime-", gsub("( )|([:])", "-", file.time), ".csv")

  write.csv(blocks, blocks.filename, row.names = FALSE)

  message("Successfully wrote block CSV to ", blocks.filename)

  return(invisible(NULL))

}



