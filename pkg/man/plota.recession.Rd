\name{plota.recession}
\alias{plota.recession}
\title{Add recession bars to the existing plot}
\usage{
  plota.recession(col = col.add.alpha("gray", 50),
    highlight = NULL)
}
\arguments{
  \item{col}{highlight color for recession periods,
  \strong{defaults to \code{\link{col.add.alpha}}('gray',
  50) - alpha = 50 so it's relatively transparent }}

  \item{ylim}{y-range to highlight recession periods,
  \strong{defaults to \code{\link{par}}('usr')[3:4] -
  extremes y coordinates of the plotting region }}

  \item{...}{additional parameters for the
  \code{\link{rect}} fucntion}
}
\value{
  nothing
}
\description{
  This function will add recession bars to the existing
  plot
}
\examples{
\dontrun{
SPY = getSymbols('SPY', auto.assign = F)
plota(SPY, type='l')
plota.recession()
plota.legend('SPY','black',SPY)
}
}
\author{
  Judson Bishop
}

