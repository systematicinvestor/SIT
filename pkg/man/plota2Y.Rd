\name{plota2Y}
\alias{plota2Y}
\title{Plot time series with second Y axis}
\usage{
  plota2Y(y, las = 1, type = "n", ...)
}
\arguments{
  \item{y}{\code{\link{xts}} object}

  \item{las}{rotation of Y axis labels, \strong{defaults to
  1}, for more info see \code{\link{par}}}

  \item{type}{plot type, \strong{defaults to 'n'}, for more
  info see \code{\link{plot}} also support 'ohlc', 'hl',
  'candle', 'volume' types}

  \item{...}{additional parameters to the
  \code{\link{plot}}}
}
\value{
  nothing
}
\description{
  Plot time series with second Y axis
}
\examples{
\dontrun{
# download data
data.spy = getSymbols('SPY', auto.assign = FALSE)
data.ibm = getSymbols('IBM', auto.assign = FALSE)

# two Y axis example
y = data.spy['2010:01:01::2011:02:01']

# to plot second Y axis, free some space on left side, set LeftMargin=3
plota(y, type = 'ohlc', LeftMargin=3)

y0 = y
y = data.ibm['2010:10:15::2011:02:01']
plota2Y(y, ylim = range(OHLC(y)),las=1, col='red', col.axis = 'red')
  plota.ohlc(y, col = 'red')
plota.legend('SPY(rhs),IBM(lhs)', 'blue,red', list(y0,y))
}
}

