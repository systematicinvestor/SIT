library(shiny)
library(xtable)
library(SIT)

load.packages('quantmod')
if (!require(quantmod)) {
	stop("This app requires the quantmod package. To install it, run 'install.packages(\"quantmod\")'.\n")
}
