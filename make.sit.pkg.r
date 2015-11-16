###############################################################################
# Setup
###############################################################################
library(devtools)
library(roxygen)
library(roxygen2)

package.name = 'SIT'
os = Sys.info()[["sysname"]]

###############################################################################
# Create folders, copy files
###############################################################################
unlink('pkg', recursive = T, force = T)
dir.create('pkg')
#file.copy('Readme.txt', 'pkg/.')
#file.copy('Readme.pkg.txt', 'pkg/.')

dir.create('pkg/R', recursive = T)

if (os == "Windows") {
    #shell('rmdir /S /Q pkg', wait = TRUE)
    #shell('mkdir pkg', wait = TRUE)
    #shell('copy Readme.txt pkg\\*.*', wait = TRUE)
    #shell('copy Readme.pkg.txt pkg\\*.*', wait = TRUE)
    #shell('mkdir pkg\\R', wait = TRUE)
    
    shell('copy R\\*.* pkg\\R\\*.*', wait = TRUE)
} else {
    system('cp R/* pkg/R/.', wait = T)        
}

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
    Maintainer = 'Michael Kapler <TheSystematicInvestor@gmail.com>',
    Depends = 'SIT.date'
    ), 
    file = file.path('pkg', "DESCRIPTION")
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
", file = file.path('pkg', 'R', paste(package.name,'package.R',sep='-')))

###############################################################################
# Create documentaion and build package
###############################################################################
roxygenize('pkg', copy.package = F, unlink.target = F, overwrite = T)

pkg <- as.package('pkg')
name = devtools:::build(pkg, 'pkg')

file.rename(name, paste(package.name, '.tar.gz', sep=''))
file.remove(name)

###############################################################################
# Usage
###############################################################################
# install.packages('SIT.tar.gz', repos = NULL, type='source')
# library(SIT)
# ?spl


