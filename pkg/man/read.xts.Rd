\name{read.xts}
\alias{read.xts}
\title{Read \code{\link{xts}} object from file}
\usage{
  read.xts(x, date.fn = paste, index.class = "Date",
    decreasing = FALSE, sep = ",", date.column = 1,
    skip = 0L, ...)
}
\arguments{
  \item{filename}{file name}

  \item{date.fn}{function to preprocess string dates,
  \strong{defaults to \code{\link{paste}} - i.e. no
  preprocessing}}

  \item{index.class}{class of the date object,
  \strong{defaults to 'Date'}}

  \item{...}{additional paramaeters to the
  \code{\link{as.POSIXct}} function}
}
\value{
  \code{\link{xts}} object
}
\description{
  Read \code{\link{xts}} object from file
}
\examples{
\dontrun{
write.xts(make.xts(1:101,seq(Sys.Date()-100, Sys.Date(), 1)), 'temp.csv')
read.xts('temp.csv')
}
}

