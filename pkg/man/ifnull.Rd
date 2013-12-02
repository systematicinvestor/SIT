\name{ifnull}
\alias{ifnull}
\title{Replace NULL values}
\usage{
  ifnull(x, y)
}
\arguments{
  \item{x}{data to check for NULL}

  \item{y}{values to be used in place of NULL}
}
\value{
  updated data
}
\description{
  This function will replace all NULL with given value
}
\examples{
\dontrun{
temp = list()
temp$val1 = ifnull(temp$val1, 4)
}
}

