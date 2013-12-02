\name{plota.y.highlight}
\alias{plota.y.highlight}
\title{Highlight horizontal segments}
\usage{
  plota.y.highlight(y, highlight,
    col = plota.control$col.y.highlight)
}
\arguments{
  \item{y}{\code{\link{xts}} object}

  \item{highlight}{segments to highlight along Y axis}

  \item{col}{highlight color, \strong{defaults to
  plota.control$col.y.highlight}}
}
\value{
  nothing
}
\description{
  Highlight horizontal segments
}
\examples{
\dontrun{
# download data
data.spy = getSymbols('SPY', auto.assign = FALSE)
rsi = RSI(Cl(data.spy), 20)

#set up two regions for graphs candlestick price data on top 2/3 of the plot
#and rsi on the bottom 1/3 of the plot
layout(c(1,1,2))

plota(data.spy, type = 'candle', plotX = F)
  plota.legend('SPY', 'grey70', data.spy)
plota(rsi, type = 'l')

col = col.add.alpha(spl('green,red'),80)
plota.y.highlight(col=col[1], highlight=c(50,100))
plota.y.highlight(col=col[2], highlight=c(0,50))

abline(h = 50, col = 'gray20')

  col = iif(last(rsi)>50,'black','red')
plota.legend('RSI(20)', col, rsi, text.col=col)
}
}

