###############################################################################
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
###############################################################################
# Collection of routines to examine and compare proxies and data
# Copyright (C) 2013  Michael Kapler
#
# For more information please visit my blog at www.SystematicInvestor.wordpress.com
# or drop me a line at TheSystematicInvestor at gmail
###############################################################################



###############################################################################
#' Compute correlations
#'
#' @param data matrix with data
#' @param method method used to compute correlations, please see \code{\link{cor}} for more details
#'
#' @return correlation matrix
#'
#' @export 
############################################################################### 
compute.cor <- function
(
	data, 		# matrix with data
	method = c("pearson", "kendall", "spearman")
)
{
	cor(data, use='complete.obs',method=method[1])
}

###############################################################################
#' Plot proxies and create summary table over common period
#'
#' @param data.all list or enviroment that holds proxy time series 
#' @param names names or indexs of time series, \strong{defaults to all time series} 
#' @param price.fn name of price function, \strong{defaults to Ad} 
#'
#' @return nothing
#'
#' @examples
#' \dontrun{ 
#' tickers = spl('HYG,VWEHX')
#' data = new.env()
#'   getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)   
#'
#' proxy.test(data)
#' }
#' @export 
############################################################################### 
proxy.test <- function(data.all, names = ls(data.all), price.fn=Ad)
{
	#*****************************************************************
	# Prepare data
	#****************************************************************** 	
	data = new.env()
		data$symbolnames = names
	for(n in data$symbolnames)
		data[[n]] = make.stock.xts( price.fn( data.all[[n]] ) )	
	bt.prep(data, align='remove.na', fill.gaps=T)

	#*****************************************************************
	# Prepare data
	#****************************************************************** 	
	prices = data$prices
	
	# Plot side by side
#	layout(1:2, heights=c(4,1))
layout(1)
		plota.matplot(scale.one(prices))

	rets = (prices/mlag(prices)-1)[-1,]
				
	# compute correlations
	temp = cor(rets, use='complete.obs', method='pearson')
		diag(temp) = NA
		temp[lower.tri(temp)] = NA		
		#temp = temp[-nrow(temp),-1,drop=F]
		temp = temp[-nrow(temp),,drop=F]
		temp[] = plota.format(100 * temp, 0, '', '%')
	out = temp
	
	# compute stats
	temp = compute.stats( as.list(rets),
			list(
				Mean=function(x) 252*mean(x,na.rm=T),
				StDev=function(x) sqrt(252)*sd(x,na.rm=T)
				)
			)
	temp[] = plota.format(100 * temp, 1, '', '%')

	# plot	
	out = rbind(out,NA,temp)
#	plot.table(out)	
  print(out)
}

###############################################################################
#' Plot 12 Month Spread for 2 symbols over common period
#'
#' @param data.all list or enviroment that holds proxy time series 
#' @param names names or indexs of time series, \strong{defaults to all time series} 
#' @param price.fn name of price function, \strong{defaults to Ad} 
#'
#' @return nothing
#'
#' @examples
#' \dontrun{ 
#' tickers = spl('HYG,VWEHX')
#' data = new.env()
#'   getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)   
#'
#' proxy.spread(data)
#' }
#' @export 
############################################################################### 
plot12month.rolling.spread <- function(data.all, names = ls(data.all), price.fn=Ad)
{
	#*****************************************************************
	# Prepare data
	#****************************************************************** 	
	data = new.env()
		data$symbolnames = names[1:2]
	for(n in data$symbolnames)
		data[[n]] = make.stock.xts( price.fn( data.all[[n]] ) )	
	bt.prep(data, align='remove.na', fill.gaps=T)

	#*****************************************************************
	# Prepare data
	#****************************************************************** 	
	prices = data$prices
	rets.12m.rolling = 100 * (prices / mlag(prices, 252) - 1)
	spread.12m.rolling = rets.12m.rolling[,1] - rets.12m.rolling[,2]
		
	# Plot side by side
	layout(1)
	plota(spread.12m.rolling, type='l', 
		main = paste('12 Month Rolling Returns Spread % for', names[1], 'and', names[2]))
		abline(h=0, col='gray')
}


###############################################################################
#' Plot all proxies overlaying the longest one
#'
#' @param data.all list or enviroment that holds proxy time series 
#' @param names names or indexs of time series, \strong{defaults to all time series} 
#' @param price.fn name of price function, \strong{defaults to Ad} 
#'
#' @return nothing
#'
#' @examples
#' \dontrun{ 
#' tickers = spl('HYG,VWEHX')
#' data = new.env()
#'   getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)   
#'
#' proxy.overlay.plot(data)
#' }
#' @export 
############################################################################### 
proxy.overlay.plot <- function(data.all, names = ls(data.all), price.fn=Ad)
{
	#*****************************************************************
	# Prepare data
	#****************************************************************** 	
	data = new.env()
		data$symbolnames = names
	for(n in data$symbolnames) 
		data[[n]] = make.stock.xts( price.fn( data.all[[n]] ) )	
		
	bt.prep(data, align='keep.all', fill.gaps=T)

	#*****************************************************************
	# Prepare data
	#****************************************************************** 	
	prices = data$prices
		prices = scale.one(prices, T)
	
	# Plot side by side
	layout(1)
	plota.matplot(prices)
}
	
###############################################################################
#' Plot complete history for each index for Close and Adjusted, and create summary table
#'
#' @param data list or enviroment that holds proxy time series 
#' @param names names or indexs of time series, \strong{defaults to all time series} 
#'
#' @return nothing
#'
#' @examples
#' \dontrun{ 
#' tickers = spl('HYG,VWEHX')
#' data = new.env()
#'   getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)   
#'
#' proxy.prices(data)
#' }
#' @export 
############################################################################### 
proxy.prices <- function(data, names = ls(data)) {
	n.names = len(names)
	temp = list()
	
#	layout(1:(n.names+1))	
	layout(1:n.names)
	for(n in names) {
		plota.matplot(cbind(Cl(data[[n]]),Ad(data[[n]])),main=n)
		temp[[ paste(n, 'Price') ]] = Cl(data[[n]])
		temp[[ paste(n, 'Total') ]] = Ad(data[[n]])
	}
	
	# compute stats
	temp = compute.stats( lapply(temp, function(x) ifna(x/mlag(x) -1,NA)),
			list(
				Mean=function(x) 252*mean(x,na.rm=T),
				StDev=function(x) sqrt(252)*sd(x,na.rm=T)
				)
			)
			
	# plot	
	temp[] = plota.format(100 * temp, 1, '', '%')	
#	plot.table(temp)			
	print(temp)
}

#' @export 
proxy.map <- function(raw.data, tickers)
{
	#*****************************************************************
	# Prepare data
	#******************************************************************
  data <- new.env()
  tickers = spl(tickers)     
  tickers = tickers[order(sapply(tickers, nchar),decreasing =T)]
  
  getSymbols.extra(tickers, src = 'yahoo', from = '1980-01-01', env = data, raw.data = raw.data, set.symbolnames = T, auto.assign = T)
    for(i in data$symbolnames) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
  bt.prep(data, align='keep.all', fill.gaps=T)
      
  layout(1)
  plota.matplot(data$prices)
}

proxy.example.test <- function() {
    #*****************************************************************
    # Load historical data
    #******************************************************************   
    load.packages('quantmod')  
    
    tickers = spl('GSG,DBC')
    data = new.env()
    getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)   
        for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
      
    # "TRJ_CRB" file was downloaded from the http://www.jefferies.com/Commodities/2cc/389
    # for "TRJ/CRB Index-Total Return"
    temp = extract.table.from.webpage( join(readLines("TRJ_CRB")), 'EODValue' )
    temp = join( apply(temp, 1, join, ','), '\n' )
    data$CRB_1 = make.stock.xts( read.xts(temp, format='%m/%d/%y' ) )
     
    # "prfmdata.csv" file was downloaded from the http://www.crbequityindexes.com/indexdata-form.php
    # for "TR/J CRB Global Commodity Equity Index", "Total Return", "All Dates"
    data$CRB_2 = make.stock.xts( read.xts("prfmdata.csv", format='%m/%d/%Y' ) )
                 
    #*****************************************************************
    # Compare
    #******************************************************************    
jpeg(filename = 'plot1.jpg', width = 500, height = 500, units = 'px', pointsize = 12)		
    proxy.test(data)    
dev.off()
jpeg(filename = 'plot2.jpg', width = 500, height = 500, units = 'px', pointsize = 12)		    
    proxy.overlay.plot(data)
dev.off()

    #*****************************************************************
    # Load historical data
    #******************************************************************   
    load.packages('quantmod')  
    
    tickers = spl('IYR,VGSIX,RWO')
    data = new.env()
    getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)   
        for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
                 
    #*****************************************************************
    # Compare
    #******************************************************************    
jpeg(filename = 'plot3.jpg', width = 500, height = 500, units = 'px', pointsize = 12)		
    proxy.test(data)    
dev.off()
jpeg(filename = 'plot4.jpg', width = 500, height = 500, units = 'px', pointsize = 12)		    
    proxy.overlay.plot(data)
dev.off()

# VGSIX,VEIEX,VBMFX,VWEHX,PEBIX,VIPSX,VTSMX,VGTSX,VFISX,VUSTX
#
#    Equity Market
#        Vanguard Total Stock Mkt (VTSMX)
#        Vanguard Total Intl Stock (VGTSX)
#        Vanguard 500 Index (VFINX)
#        Vanguard Emerging Mkts (VEIEX)
#    Fixed Income Market
#        Vanguard Short-Term Treasury (VFISX)
#        Vanguard Long-Term Treasury (VUSTX)
#        Vanguard Total Bond Market (VBMFX)
#        Vanguard High-Yield Corporate (VWEHX)
#        PIMCO Emerging Markets Bond (PEBIX)
#        Vanguard Inflation-Protected (VIPSX)
#        PIMCO Total Return (PTTRX)
#    Vanguard REIT (VGSIX)
#
}    

