\name{iif}
\alias{iif}
\title{Faster version of ifelse function}
\usage{
  iif(cond, truepart, falsepart)
}
\arguments{
  \item{cond}{true / false condition}

  \item{truepart}{resulting value(s) if condition is true}

  \item{falsepart}{resulting value(s) if condition is
  false}
}
\value{
  number of elements in x
}
\description{
  This function is a faster version of ifelse function
}
\examples{
\dontrun{
iif(1:10 > 5, 1, 1:10)
}
}

