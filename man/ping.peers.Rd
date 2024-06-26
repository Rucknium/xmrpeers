% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ping.R
\name{ping.peers}
\alias{ping.peers}
\title{Ping peer nodes for latency measurement}
\usage{
ping.peers(
  bitmonero.dir = "~/.bitmonero",
  output.file = "monero_peer_pings.csv",
  sleep = 10,
  ping.count = 5,
  threads = NULL
)
}
\arguments{
\item{bitmonero.dir}{.bitmonero directory where the monero.log file is.}

\item{output.file}{Name of the output file. The file will be created in \code{bitmonero.dir}.}

\item{sleep}{Number of seconds to sleep between each round of collecting new peer IPs.}

\item{ping.count}{Number of times to ping each peer.}

\item{threads}{Override default number of threads for sending pings.}
}
\value{
No return value. Executes in a loop until interrupted.
}
\description{
Ping peer nodes for latency measurement
}
\examples{
\dontrun{
ping.peers()
}
}
