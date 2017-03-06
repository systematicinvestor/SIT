\name{getSymbols.sit}
\alias{getSymbols.sit}
\title{Helper function to read historical stock prices saved by Seasonality tool}
\usage{
  getSymbols.sit(Symbols, env = .GlobalEnv,
    auto.assign = TRUE,
    stock.folder = "c:/temp/Seasonality/stocks",
    stock.date.format = "\%Y-\%m-\%d", ...)
}
\arguments{
  \item{Symbols}{vector of symbols}

  \item{env}{enviroment to store prices, \strong{defaults
  to .GlobalEnv}}

  \item{auto.assign}{flag to auto assign symbols,
  \strong{defaults to TRUE}}

  \item{stock.folder}{stock folder, \strong{defaults to
  'c:/temp/Seasonality/stocks'}}

  \item{stock.date.format}{stock date format,
  \strong{defaults to '\%Y-\%m-\%d'}}

  \item{...}{other parameters for getSymbols function}
}
\value{
  nothing is auto.assign = TRUE, prices are stored in the
  env enviroment if auto.assign = FALSE, returns first
  symbol
}
\description{
  Helper function to read historical stock prices saved by
  Seasonality tool
}
\examples{
\dontrun{
data <- new.env()
getSymbols.sit(spl('SPY,IBM'), env = data, auto.assign = T)
}
}
\references{
  \url{http://stackoverflow.com/questions/8970823/how-to-load-csv-data-file-into-r-for-use-with-quantmod}
}

