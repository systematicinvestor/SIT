\name{count}
\alias{count}
\title{Count number of non NA elements}
\usage{
  count(x, side = 2)
}
\arguments{
  \item{x}{data matrix}

  \item{side}{margin along which to count}
}
\value{
  counts
}
\description{
  This function will count number of non NA elements in the
  given matrix
}
\examples{
\dontrun{
count(matrix(c(1,NA,2,3),2,2))
}
}

