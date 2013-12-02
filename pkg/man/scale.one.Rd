\name{scale.one}
\alias{scale.one}
\title{Normilize all timeseries to start at one}
\usage{
  scale.one(x, overlay = F,
    main.index = which(!is.na(x[1, ]))[1])
}
\arguments{
  \item{x}{\code{\link{xts}} time series}
}
\value{
  scaled \code{\link{xts}} time series, so that each
  timeseries starts at one
}
\description{
  Normilize all timeseries to start at one
}
\examples{
\dontrun{
plota.matplot(scale.one(data$prices))
}
}

