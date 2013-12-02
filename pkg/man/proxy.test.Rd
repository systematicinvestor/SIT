\name{proxy.test}
\alias{proxy.test}
\title{Plot proxies and create summary table over common period}
\usage{
  proxy.test(data.all, names = ls(data.all), price.fn = Ad)
}
\arguments{
  \item{data.all}{list or enviroment that holds proxy time
  series}

  \item{names}{names or indexs of time series,
  \strong{defaults to all time series}}

  \item{price.fn}{name of price function, \strong{defaults
  to Ad}}
}
\value{
  nothing
}
\description{
  Plot proxies and create summary table over common period
}
\examples{
\dontrun{
tickers = spl('HYG,VWEHX')
data = new.env()
  getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)

proxy.test(data)
}
}

