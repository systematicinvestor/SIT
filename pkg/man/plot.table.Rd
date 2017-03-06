\name{plot.table}
\alias{plot.table}
\title{Plot Table}
\usage{
  plot.table(plot.matrix, smain = NULL, text.cex = 1,
    frame.cell = T, highlight = F, colorbar = FALSE,
    keep_all.same.cex = FALSE)
}
\arguments{
  \item{plot.matrix}{matrix to plot}

  \item{smain}{text to draw in top,left cell}

  \item{text.cex}{text size, \strong{defaults to 1}}

  \item{frame.cell}{flag to draw border, \strong{defaults
  to TRUE}}

  \item{highlight}{flag to highlight data, \strong{defaults
  to FALSE}}

  \item{colorbar}{flag to draw colorbar, \strong{defaults
  to FALSE}}

  \item{keep_all.same.cex}{flag to auto-adjust text size,
  \strong{defaults to FALSE}}
}
\value{
  nothing
}
\description{
  Create Plot of the given matrix
}
\examples{
\dontrun{
# generate 1,000 random numbers from Normal(0,1) distribution
data =  matrix(rnorm(1000), nc=10)
  colnames(data) = paste('data', 1:10, sep='')

# compute Pearson correlation of data and format it nicely
temp = cor(data, use='complete.obs', method='pearson')
  temp[] = plota.format(100 * temp, 0, '', '\%')

# plot temp with colorbar, display Correlation in (top, left) cell
plot.table(temp, smain='Correlation', highlight = TRUE, colorbar = TRUE)
}
}

