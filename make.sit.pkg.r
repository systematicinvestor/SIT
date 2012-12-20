###############################################################################
# Setup
###############################################################################
library(devtools)
library(roxygen)
library(roxygen2)

package.name = 'sit'

###############################################################################
# Create folders, copy files
###############################################################################
shell('rmdir /S /Q SIT', wait = TRUE)
shell('mkdir SIT', wait = TRUE)
shell('copy Readme.txt SIT\\*.*', wait = TRUE)
shell('copy Readme.pkg.txt SIT\\*.*', wait = TRUE)

shell('mkdir SIT\\R', wait = TRUE)
shell('copy R\\*.* SIT\\R\\*.*', wait = TRUE)

###############################################################################
# Create DESCRIPTION files
###############################################################################
write.dcf(list(
	Package = toupper(package.name), 
	Type = 'Package',
	Title = 'Systematic Investor Toolbox', 
	Description = 'Systematic Investor Toolbox is a collection of tools that\n I use in my investment research.', 
    Version = format(Sys.Date(),'%Y.%m.%d'), 
    Date = Sys.Date(),
    License = 'GPL-3', 
    LazyLoad = 'yes',
    Author = 'Michael Kapler <TheSystematicInvestor@gmail.com>', 
    Maintainer = 'Michael Kapler <TheSystematicInvestor@gmail.com>'
    ), 
    file = file.path(package.name, "DESCRIPTION")
)

cat("
#' Systematic Investor Toolbox.
#' 
#' Systematic Investor Toolbox is a collection of tools that 
#' I use in my investment research.
#' 
#' 
#' 
#' @name SIT
#' @aliases SIT SIT-package
#' @docType package
#' @title Systematic Investor Toolbox.
#' @author Michael Kapler \\email{TheSystematicInvestor@@gmail.com}
#' @keywords package Asset Allocation Backtesting Trading
#' @examples
#' \\dontrun{
#' ##*****************************************************************
#' ## Permanent Portfolio Example
#' ##*****************************************************************
#' ## Load historical data 
#' ##*****************************************************************
#' load.packages('quantmod')
#' tickers = spl('SPY,TLT,GLD,SHY')
#' 
#' data <- new.env()
#' getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T) 
#'     # adjust for dividends
#'     for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T) 
#' bt.prep(data, align='remove.na') 
#' 
#' ##*****************************************************************
#' ## Setup
#' ##****************************************************************** 		
#' prices = data$prices   
#'     n = ncol(prices)
#' 
#' ## rebalance quarterly
#' period.ends = endpoints(prices, 'quarters')
#'     period.ends = period.ends[period.ends > 0]		
#'     period.ends = c(1, period.ends) 
#' 
#' models = list()
#' 	
#' ##*****************************************************************
#' ## Code Strategies
#' ##****************************************************************** 	
#' target.allocation = matrix(rep(1/n,n), nrow=1)
#' 	
#' ## Buy & Hold	
#' data$weight[] = NA	
#'     data$weight[1,] = target.allocation
#' models$buy.hold = bt.run.share(data, clean.signal=F)
#' 
#' ## Equal Weight
#' data$weight[] = NA
#'     data$weight[period.ends,] = ntop(prices[period.ends,], n)
#' models$equal.weight = bt.run.share(data, clean.signal=F)
#'  
#' ##*****************************************************************
#' ## Create Report
#' ##******************************************************************        
#' strategy.performance.snapshoot(models,T)	
#'    
#' plotbt.custom.report.part2(models$buy.hold)
#'    
#' plotbt.custom.report.part2(models$equal.weight)
#' }
NULL
", file = file.path(package.name, 'R', paste(package.name,'package.R',sep='-')))

###############################################################################
# Create documentaion and build package
###############################################################################
roxygenize(package.name, copy.package = F, unlink.target = F, overwrite = T)

pkg <- as.package(package.name)
name = devtools:::build(pkg, package.name)
shell(paste('copy /Y /B', gsub('/','\\\\',name), 'SIT.tar.gz'), wait = TRUE)


###############################################################################
# Usage
###############################################################################
# install.packages('SIT.tar.gz', repos = NULL, type='source')
# library(SIT)
# ?spl


