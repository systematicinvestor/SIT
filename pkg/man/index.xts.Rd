\name{index.xts}
\alias{index.xts}
\title{Fast alternative to index() function for \code{\link{xts}} object}
\usage{
  index.xts(x)
}
\arguments{
  \item{x}{\code{\link{xts}} object}
}
\value{
  dates
}
\description{
  NOTE index.xts is the same name as the index function in
  the XTS package
}
\examples{
\dontrun{
index.xts(make.xts(1:101,seq(Sys.Date()-100, Sys.Date(), 1)))
}
}

