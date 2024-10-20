

#' Print likely newborn nodes to the console
#'
#' @description When a node connects to our own node, it could be a newborn
#' node that is syncing from the genesis block. `newborn.nodes` queries the
#' local Monero node about node connections with a `get_peers` call and prints
#' information about likely newborn nodes to the console. The unrestricted RPC
#' port must be reachable by R. This function is an infinite loop. `ctrl + c`
#' to interrupt the function and print all IP addresses of likely newborn nodes
#' recorded so far. Each peer will only be printed once during a specific run of
#' `newborn.nodes`, even if the node disconnects and reconnects later.
#'
#' @param unrestricted.rpc.url URL and port of the `monerod` unrestricted RPC.
#' Default is `http://127.0.0.1:18081`
#' @param poll.time How often, in seconds, to check for a newborn node
#' connection. Default is 30 seconds.
#' @param sync.height.lag Criteria to consider a node as "newborn". Nodes that
#' have a height greater than current network height minus `sync.height.lag`
#' will not be considered a newborn node. Default is 3 months.
#' @param avg_upload.limit Criteria to consider a node as "newborn". The
#' `avg_upload` from the `get_peers` call must be greater than or equal to
#' `avg_upload.limit` OR `current_upload` must be greater than or equal to
#' `current_upload.limit` to consider the node as "newborn". Default is 10 for
#' both limits. Sometimes a node gets stuck at a low height, so it isn't
#' actually new or syncing. This criteria makes sure that the peer is actually
#' actively syncing data.
#' @param current_upload.limit Criteria to consider a node as "newborn".
#'
#' @return
#' NULL (invisible)
#' @export
#'
#' @examples
#' \dontrun{
#' newborn.nodes()
#' }

newborn.nodes <- function(unrestricted.rpc.url = "http://127.0.0.1:18081",
  poll.time = 30, sync.height.lag = 30 * 24 * 90, avg_upload.limit = 10,
  current_upload.limit = 10) {

  json.post <- RJSONIO::toJSON(
    list(
      jsonrpc = "2.0",
      id = "0",
      method = "get_connections",
      params = ""
    )
  )

  already.seen.newborn.address <- c()
  already.seen.newborn.peer_id <- c()

  on.exit({
    session.duration <- Sys.time() - session.start.time
    cat(paste0(format(Sys.time(), "%Y-%m-%d %T"),
      " ", length(already.seen.newborn.address),
      " likely newborn nodes seen during this session that lasted ",
      round(session.duration, 1), " ", units(session.duration),
      ": \n",
      paste0(already.seen.newborn.address, collapse = " "), "\n"))
  })

  session.start.time <- Sys.time()

  cat(paste0(format(session.start.time, "%Y-%m-%d %T"), " Start polling monerod for newborn nodes...\n"))

  while (TRUE) {

    peers <- RJSONIO::fromJSON(
      RCurl::postForm(paste0(unrestricted.rpc.url, "/json_rpc"),
        .opts = list(
          userpwd = "",
          postfields = json.post,
          httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')
        )
      ), asText = TRUE
    )

    if(length(peers$result$connections) == 0 ) {
      cat("Unexpected results from node's RPC response. Check that monerod's unrestricted RPC port is at ",
        unrestricted.rpc.url, "\n")
      Sys.sleep(poll.time)
      next
    }

    peers <- peers$result$connections

    peer.heights <- sapply(peers, FUN = function(x) {x$height})
    peer.address <- sapply(peers, FUN = function(x) {x$address})
    peer.peer_id <- sapply(peers, FUN = function(x) {x$peer_id})
    peer.avg_upload <- sapply(peers, FUN = function(x) {x$avg_upload})
    peer.current_upload <- sapply(peers, FUN = function(x) {x$current_upload})

    consensus.height <- floor(quantile(peer.heights, prob = 0.9))

    new.nodes <- which(
      peer.heights > 1 &
        (peer.heights <= consensus.height - sync.height.lag) &
        (! peer.address %in% already.seen.newborn.address) &
        (! peer.peer_id %in% already.seen.newborn.peer_id) &
        (peer.avg_upload > avg_upload.limit | peer.current_upload > current_upload.limit)
    )

    for (i in new.nodes) {

      cat(
        paste0(format(Sys.time(), "%Y-%m-%d %T"), " ",
          "Likely newborn node: ",
          formatC(peers[[i]]$address, width = 21), ", ",
          # "Peer ID: ", peers[[i]]$peer_id, ", ",
          # "Connection ID: ",  peers[[i]]$connection_id, ", ",
          "Height: ",
          formatC(peers[[i]]$height, width = 7, format = "d"), "/", consensus.height, ", ",
          ifelse(peers[[i]]$pruning_seed > 0, "  Pruned", "Unpruned"), ", ",
          ifelse(peers[[i]]$incoming, "Incoming connection", "Outgoing connection"), ", ",
          "avg_upload: ", formatC(peers[[i]]$avg_upload, width = 4),
          # ", ", "current_upload: ", formatC(peers[[i]]$current_upload, width = 4),
          # ", ", "send_count: ", formatC(peers[[i]]$send_count, width = 8, format = "d"),
          "\n"
        )
      )

      already.seen.newborn.address <- c(already.seen.newborn.address, peers[[i]]$address)
      already.seen.newborn.peer_id <- c(already.seen.newborn.peer_id, peers[[i]]$peer_id)

    }

    Sys.sleep(poll.time)

  }

  return(invisible(NULL))

}
