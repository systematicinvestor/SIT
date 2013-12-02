\name{ifna}
\alias{ifna}
\title{Replace NA, NaN, Inf values}
\usage{
  ifna(x, y)
}
\arguments{
  \item{x}{data to check for NA, NaN, Inf}

  \item{y}{values(s) to be used in place of NA, NaN, Inf}
}
\value{
  updated data
}
\description{
  This function will replace all NA, NaN, Inf with given
  values
}
\examples{
\dontrun{
ifna(c(1,NA,2,Inf,3), 4)
}
}

