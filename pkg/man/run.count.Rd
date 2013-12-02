\name{run.count}
\alias{run.count}
\title{Running Count over given window}
\usage{
  run.count(x, window.len)
}
\arguments{
  \item{x}{data matrix}

  \item{window.len}{window length}
}
\value{
  counts
}
\description{
  This function will count number of non NA elements over
  given window
}
\examples{
\dontrun{
run.count(matrix(1:9,3,3),2)
}
}

