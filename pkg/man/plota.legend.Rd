\name{plota.legend}
\alias{plota.legend}
\title{Plot legend - shortcut to the \code{\link{legend}} function}
\usage{
  plota.legend(labels, fill = NULL, lastobs = NULL,
    x = "topleft", merge = F, bty = "n",
    yformat = plota.format, ...)
}
\arguments{
  \item{labels}{legend labels}

  \item{fill}{fill colors, \strong{defaults to NULL}}

  \item{lastobs}{list of last observations,
  \strong{defaults to NULL}}

  \item{x}{location of legend, \strong{defaults to
  'topleft'}}

  \item{merge}{merge, \strong{defaults to FALSE}, see
  \code{\link{legend}} function for more info}

  \item{bty}{box, \strong{defaults to 'n'}, see
  \code{\link{legend}} function for more info}

  \item{yformat}{format Y values function, \strong{defaults
  to \code{\link{plota.format}}}}

  \item{...}{other parameters to legend, see
  \code{\link{legend}} function for more info}
}
\value{
  nothing
}
\description{
  Plot legend - shortcut to the \code{\link{legend}}
  function
}

