\name{write.xts}
\alias{write.xts}
\title{Write \code{\link{xts}} object to file}
\usage{
  write.xts(x, filename, append = FALSE, ...)
}
\arguments{
  \item{x}{\code{\link{xts}} object}

  \item{filename}{file name}

  \item{append}{flag to inidicate if file is overwritten or
  appended, \strong{defaults to FALSE}}

  \item{...}{additional paramaeters to the
  \code{\link{format}} function}
}
\value{
  nothing
}
\description{
  Write \code{\link{xts}} object to file
}
\examples{
\dontrun{
write.xts(make.xts(1:101,seq(Sys.Date()-100, Sys.Date(), 1)), 'temp.csv')
}
}

