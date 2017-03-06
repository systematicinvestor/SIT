\name{load.packages}
\alias{load.packages}
\title{Load Packages that are available and install ones that are not available}
\usage{
  load.packages(packages,
    repos = "http://cran.r-project.org",
    dependencies = c("Depends", "Imports"), ...)
}
\arguments{
  \item{packages}{names of the packages separated by comma}

  \item{repos}{default repository}

  \item{dependencies}{type of dependencies to install}

  \item{...}{additional parameters for the
  \code{\link{install.packages}} function}
}
\value{
  nothing
}
\description{
  This function a convience wrapper for install.packages()
  function
}
\examples{
\dontrun{
load.packages('quantmod')
}
}

