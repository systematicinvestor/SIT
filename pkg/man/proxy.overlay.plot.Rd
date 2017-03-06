\name{proxy.overlay.plot}
\alias{proxy.overlay.plot}
\title{Plot all proxies overlaying the longest one}
\usage{
  proxy.overlay.plot(data.all, names = ls(data.all),
    price.fn = Ad)
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
  Plot all proxies overlaying the longest one
}
\examples{
\dontrun{
tickers = spl('HYG,VWEHX')
data = new.env()
  getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)

proxy.overlay.plot(data)
}
}

