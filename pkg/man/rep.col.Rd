\name{rep.col}
\alias{rep.col}
\title{Repeat Columns}
\usage{
  rep.col(m, nc)
}
\arguments{
  \item{m}{vector (column)}

  \item{nc}{number of copies along columns}
}
\value{
  new matrix
}
\description{
  Repeat Columns
}
\examples{
\dontrun{
matrix(1:5, nr=5, nc=3, byrow=F)
rep.col(1:5, 3)
}
}

