\name{dates2index}
\alias{dates2index}
\title{Determine the index of subset of dates in the time series}
\usage{
  dates2index(x, dates = 1:nrow(x))
}
\arguments{
  \item{x}{xts time series}

  \item{dates}{string represnting subset of dates i.e.
  '2010::2012'}
}
\value{
  index of subset of dates in the time series
}
\description{
  Determine the index of subset of dates in the time series
}
\examples{
\dontrun{
dates2index(data$prices, '2010::2012')

data = textConnection('
date,Close
2013-03-18,    154.97
2013-03-19,    154.61
2013-03-20,    155.69
2013-03-21,    154.36
2013-03-22,    155.60
2013-03-25,    154.95')

x = read.xts(data)
dates2index(x, '2013-03-19')

}
}

