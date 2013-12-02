\name{join}
\alias{join}
\title{Join vector of strings into one string using delim}
\usage{
  join(v, delim = "")
}
\arguments{
  \item{v}{vector of strings}

  \item{delim}{delimiter, \strong{defaults to ","}}
}
\value{
  resulting string
}
\description{
  This function will join vector of strings into one string
  using delim
}
\examples{
\dontrun{
join(c('a','b','c'), ',')
}
}

