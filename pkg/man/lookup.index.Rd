\name{lookup.index}
\alias{lookup.index}
\title{Find location: row, col in the matrix, given index of of observation}
\usage{
  lookup.index(data, i, details = F)
}
\arguments{
  \item{data}{matrix}

  \item{i}{index of observations}

  \item{details}{flag to provide details, \strong{defaults
  to FALSE}}
}
\value{
  new matrix
}
\description{
  Find location: row, col in the matrix, given index of of
  observation
}
\examples{
\dontrun{
data = matrix(1:16,4,4)
lookup.index(data, which(data > 4))
}
}

