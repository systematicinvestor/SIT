\name{asc}
\alias{asc}
\alias{chr}
\alias{make.random.string}
\title{Working with characters}
\usage{
  asc(x)

  chr(n)

  make.random.string(nbits = 256)
}
\description{
  http://datadebrief.blogspot.ca/2011/03/ascii-code-table-in-r.html
}
\details{
  char is 8 bits, so to generate 512 bits random string
  rawToChar(as.raw(runif(512/8, 1, 255)))
}

