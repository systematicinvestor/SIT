\name{plota.matplot}
\alias{plota.matplot}
\title{\code{\link{matplot}} version for \code{\link{xts}} object}
\usage{
  plota.matplot(y, dates = NULL, ylim = NULL, type = "l",
    ...)
}
\arguments{
  \item{y}{\code{\link{xts}} object}

  \item{dates}{subset of dates\strong{defaults to NULL}}

  \item{ylim}{range on Y values, \strong{defaults to NULL}}

  \item{type}{plot type, \strong{defaults to 'l'}, see
  \code{\link{plot}} for details}

  \item{...}{additional parameters to the
  \code{\link{matplot}}}
}
\value{
  nothing
}
\description{
  \code{\link{matplot}} version for \code{\link{xts}}
  object
}
\examples{
\dontrun{
# plot all prices
plota.matplot(data$prices)
}
}

