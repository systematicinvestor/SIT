
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
#' @author Michael Kapler \email{TheSystematicInvestor@@gmail.com}
#' @keywords package Asset Allocation Backtesting Trading
#' @examples
#' \dontrun{
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
