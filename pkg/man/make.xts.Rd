\name{make.xts}
\alias{make.xts}
\title{Create \code{\link{xts}} object, faster version of \code{\link{xts}} fucntion}
\usage{
  make.xts(x, order.by)
}
\arguments{
  \item{x}{vector / matrix / data frame}

  \item{order.by}{dates that correspond to rows of x}
}
\value{
  \code{\link{xts}} object
}
\description{
  Create \code{\link{xts}} object, faster version of
  \code{\link{xts}} fucntion
}
\examples{
\dontrun{
make.xts(1:101,seq(Sys.Date()-100, Sys.Date(), 1))
}
}

