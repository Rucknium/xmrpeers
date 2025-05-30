% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/newborn.R
\name{newborn.nodes}
\alias{newborn.nodes}
\title{Print likely newborn nodes to the console}
\usage{
newborn.nodes(
  unrestricted.rpc.url = "http://127.0.0.1:18081",
  poll.time = 30,
  sync.height.lag = 30 * 24 * 90,
  avg_upload.limit = 10,
  current_upload.limit = 10
)
}
\arguments{
\item{unrestricted.rpc.url}{URL and port of the \code{monerod} unrestricted RPC.
Default is \verb{http://127.0.0.1:18081}}

\item{poll.time}{How often, in seconds, to check for a newborn node
connection. Default is 30 seconds.}

\item{sync.height.lag}{Criteria to consider a node as "newborn". Nodes that
have a height greater than current network height minus \code{sync.height.lag}
will not be considered a newborn node. Default is 3 months.}

\item{avg_upload.limit}{Criteria to consider a node as "newborn". The
\code{avg_upload} from the \code{get_peers} call must be greater than or equal to
\code{avg_upload.limit} OR \code{current_upload} must be greater than or equal to
\code{current_upload.limit} to consider the node as "newborn". Default is 10 for
both limits. Sometimes a node gets stuck at a low height, so it isn't
actually new or syncing. This criteria makes sure that the peer is actually
actively syncing data.}

\item{current_upload.limit}{Criteria to consider a node as "newborn".}
}
\value{
NULL (invisible)
}
\description{
When a node connects to our own node, it could be a newborn
node that is syncing from the genesis block. \code{newborn.nodes} queries the
local Monero node about node connections with a \code{get_peers} call and prints
information about likely newborn nodes to the console. The unrestricted RPC
port must be reachable by R. This function is an infinite loop. \code{ctrl + c}
to interrupt the function and print all IP addresses of likely newborn nodes
recorded so far. Each peer will only be printed once during a specific run of
\code{newborn.nodes}, even if the node disconnects and reconnects later.
}
\examples{
\dontrun{
newborn.nodes()
}
}
