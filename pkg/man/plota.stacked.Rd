\name{plota.stacked}
\alias{plota.stacked}
\title{Create Staccked plot}
\usage{
  plota.stacked(x, y, xlab = "",
    col = plota.colors(ncol(y)), type = c("l", "s"),
    flip.legend = F, ...)
}
\arguments{
  \item{x}{dates object}

  \item{y}{matrix with weights}

  \item{xlab}{X label, \strong{defaults to ''}, for more
  info see \code{\link{plot}}}

  \item{col}{colors, \strong{defaults to
  \code{\link{plota.colors(ncol(y))}}}}

  \item{type}{plot type: lines, step stairs c('l','s')}

  \item{...}{additional parameters to the
  \code{\link{plot}}}
}
\value{
  nothing
}
\description{
  Create Staccked plot
}
\examples{
\dontrun{
# plot allocation weights
plota.stacked(index.xts(weight), weight)
}
}

