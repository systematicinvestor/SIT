library(shiny)
library(xtable)


###############################################################################
# Load Systematic Investor Toolbox (SIT)
# http://systematicinvestor.wordpress.com/systematic-investor-toolbox/
###############################################################################
if(!file.exists('../sit'))
	shiny:::download('https://github.com/systematicinvestor/SIT/raw/master/sit.lite.gz', '../sit', mode = 'wb', quiet = TRUE)
con = gzcon(file('../sit', 'rb'))
	source(con)
close(con)






load.packages('quantmod')
if (!require(quantmod)) {
	stop("This app requires the quantmod package. To install it, run 'install.packages(\"quantmod\")'.\n")
}

