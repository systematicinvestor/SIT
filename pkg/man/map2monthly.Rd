\name{map2monthly}
\alias{map2monthly}
\title{Map given time series to monthly}
\usage{
  map2monthly(equity)
}
\arguments{
  \item{equity}{time series}
}
\value{
  xts object
}
\description{
  If frequency of observations in the given time series is
  less than monthly, i.e. quaterly or annually, properly
  align this time series to monthly
}
\examples{
\dontrun{
map2monthly(equity)
}
}

