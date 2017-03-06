\name{proxy.prices}
\alias{proxy.prices}
\title{Plot complete history for each index for Close and Adjusted, and create summary table}
\usage{
  proxy.prices(data, names = ls(data))
}
\arguments{
  \item{data}{list or enviroment that holds proxy time
  series}

  \item{names}{names or indexs of time series,
  \strong{defaults to all time series}}
}
\value{
  nothing
}
\description{
  Plot complete history for each index for Close and
  Adjusted, and create summary table
}
\examples{
\dontrun{
tickers = spl('HYG,VWEHX')
data = new.env()
  getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)

proxy.prices(data)
}
}

