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
#' Normilize all timeseries to start at one
#'
#' @param x \code{\link{xts}} time series 
#'
#' @return scaled \code{\link{xts}} time series, so that each timeseries starts at one
#'
#' @examples
#' \dontrun{ 
#' plota.matplot(scale.one(data$prices))
#' }
#' @export 
############################################################################### 
# scale.one <- function(x) x / rep.row(as.numeric(x[1,]), nrow(x))	
scale.one <- function
(
	x, 
	overlay = F, 
	main.index = which(!is.na(x[1,]))[1] 
) 
{
	index = 1:nrow(x)
	if( overlay )
		x / rep.row(apply(x, 2, 
				function(v) {
					i = index[!is.na(v)][1]
					v[i] / as.double(x[i,main.index])
				}
		), nrow(x))
	else
		x / rep.row(apply(x, 2, function(v) v[index[!is.na(v)][1]]), nrow(x))
}

###############################################################################
#' Create stock like \code{\link{xts}} object from one column time series
#'
#' @param out \code{\link{xts}} time series 
#' @param column column index to use, \strong{defaults to 1} 
#'
#' @return stock like \code{\link{xts}} object
#'
#' @export 
############################################################################### 
make.stock.xts <- function(out, column=1, ...) {
	out = out[,column]
		colnames(out) = 'Close'
	out$Adjusted = out$Open = out$High = out$Low = out$Close
		out$Volume = 0
	return(out)
}	    

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
	bt.prep(data, align='remove.na')

	#*****************************************************************
	# Prepare data
	#****************************************************************** 	
	prices = data$prices
	
	# Plot side by side
	layout(1:2, heights=c(4,1))
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
	plot.table(out)	
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
		
	bt.prep(data, align='keep.all')

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
	
	layout(1:(n.names+1))	
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
	plot.table(temp)			
}




