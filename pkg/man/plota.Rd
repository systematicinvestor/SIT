\name{plota}
\alias{plota}
\title{Plot function for time series}
\usage{
  plota(y, main = NULL, plotX = TRUE, LeftMargin = 0,
    x.highlight = NULL, y.highlight = NULL, las = 1,
    type = "n", xlab = "", ylab = "", ylim = NULL,
    log = "", ...)
}
\arguments{
  \item{y}{\code{\link{xts}} object}

  \item{main}{plot title}

  \item{plotX}{flag to display X axis}

  \item{LeftMargin}{to plot second Y axis, set
  LeftMargin=3, \strong{defaults to 0}}

  \item{x.highlight}{segments to highlight along X axis,
  \strong{defaults to NULL}}

  \item{y.highlight}{segments to highlight along Y axis,
  \strong{defaults to NULL}}

  \item{las}{rotation of Y axis labels, \strong{defaults to
  1}, for more info see \code{\link{par}}}

  \item{type}{plot type, \strong{defaults to 'n'}, for more
  info see \code{\link{plot}} also support 'ohlc', 'hl',
  'candle', 'volume' types}

  \item{xlab}{X label, \strong{defaults to ''}, for more
  info see \code{\link{plot}}}

  \item{ylab}{Y label, \strong{defaults to ''}, for more
  info see \code{\link{plot}}}

  \item{ylim}{range on Y values, \strong{defaults to NULL}}

  \item{log}{log scale x, y, xy axes, \strong{defaults to
  ''}}

  \item{...}{additional parameters to the
  \code{\link{plot}}}
}
\value{
  nothing
}
\description{
  Plot function for time series
}
\examples{
\dontrun{
# download data
data.spy = getSymbols('SPY', auto.assign = FALSE)

# simple example candles and volume
y = data.spy['2011:01:01::2011:02:01']
highlight = which(Cl(y) < 127)

# plot
layout(c(1,1,2))
plota(y, type = 'candle', main = 'SPY', plotX = F, x.highlight = highlight)
  y = plota.scale.volume(y)
plota(y, type = 'volume', x.highlight = highlight)
}
}

