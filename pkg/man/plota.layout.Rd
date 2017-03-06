\name{plota.layout}
\alias{plota.layout}
\title{Create layout}
\usage{
  plota.layout(ilayout, delim = ",")
}
\arguments{
  \item{ilayout}{matrix stored as a string}

  \item{delim}{delimiter, \strong{defaults to ','}}
}
\value{
  nothing
}
\description{
  Create layout
}
\examples{
\dontrun{
# create layout
ilayout =
'1,1
2,2
2,2
3,4
3,4
3,4'
plota.layout(ilayout)
}
}

