\name{rep.row}
\alias{rep.row}
\title{Repeat Rows}
\usage{
  rep.row(m, nr)
}
\arguments{
  \item{m}{vector (row)}

  \item{nr}{number of copies along rows}
}
\value{
  new matrix
}
\description{
  Repeat Rows
}
\examples{
\dontrun{
matrix(1:3, nr=5, nc=3, byrow=T)
rep.row(1:3, 5)
}
}

