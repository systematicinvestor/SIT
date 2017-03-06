\name{bt.apply.min.weight}
\alias{bt.apply.min.weight}
\title{Remove small weights}
\usage{
  bt.apply.min.weight(weight, long.min.weight = 0.1,
    short.min.weight = long.min.weight)
}
\arguments{
  \item{weight}{weight matrix}

  \item{long.min.weight}{minimum weight for long positions,
  \strong{defaults to 0.1 }}

  \item{short.min.weight}{minimum weight for short
  positions, \strong{defaults to long.min.weight }}
}
\value{
  updated weight matrix
}
\description{
  This function will remove weights that are smaller than
  given threshold
}
\examples{
\dontrun{
weight = c(0.1, 0.6, 0.2, 0.1, 0, -0.1, -0.6, -0.2, -0.1, 0)
weight = matrix(weight, nrow=2, byrow=TRUE)
print(bt.apply.min.weight(weight, 0.1))
}
}
\author{
  Ivan Popivanov and Michael Kapler
}

