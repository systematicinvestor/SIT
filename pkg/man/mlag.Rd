\name{mlag}
\alias{mlag}
\title{Lag matrix or vector}
\usage{
  mlag(m, nlag = 1)
}
\arguments{
  \item{x}{vector / matrix}

  \item{nlag}{number of lags, \strong{defaults to 1}}
}
\value{
  modified object
}
\description{
  This function shifts elemnts in a vector or a mtrix by a
  given lag. For example: mlag(x,1) - use yesterday's
  values and mlag(x,-1) - use tomorrow's values
}
\examples{
\dontrun{
mlag(1:10)
}
}

