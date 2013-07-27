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
# Stop functionality for Backtests
# Copyright (C) 2013  Michael Kapler
#
# For more information please visit my blog at www.SystematicInvestor.wordpress.com
# or drop me a line at TheSystematicInvestor at gmail
###############################################################################



###############################################################################
# Timed Exit: exit trade after nlen bars
#' @export 
###############################################################################
bt.exrem.time.exit <- function(signal, nlen, create.weight = T) {
	signal[is.na(signal)] = FALSE
	
	signal.index = which(signal)
		nsignal.index = len(signal.index)
		nperiods = len(signal)	
		signal.index.exit = iif(signal.index + nlen - 1 > nperiods, nperiods, signal.index + nlen)
				
	if(!create.weight) {
		for(i in 1:nsignal.index) {
			if( signal[ signal.index[i] ] ) {
				signal[ (signal.index[i]+1) : signal.index.exit[i] ] = FALSE
			}
		}	
		return(signal)
	} else {
		temp = signal * NA

		for(i in 1:nsignal.index) {
			if( signal[ signal.index[i] ] ) {
				signal[ (signal.index[i]+1) : signal.index.exit[i] ] = FALSE
				temp[ signal.index.exit[i] ] = 0
			}
		}	
				
		temp[signal] = 1
		return(temp)
	}
}



###############################################################################
# Enforce minimum holding period before taking another signal
#' @export 
###############################################################################
bt.min.holding.period <- function(x, nlen) {
	x = coredata(x)
	
	enter = x != 0
    enter[is.na(enter)] = FALSE
    enter.index = which(enter)
    
    for(t in enter.index)
	    if( enter[ t ] ) {
			index = t + nlen
			enter[ t : index ] = FALSE
		    x[ t : index ] = x[t]
		}
	return(x)
}


###############################################################################
# Matrix versions of time/price/time.price stops
#' @export 
###############################################################################
bt.time.stop <- function(weight, nlen)
{
	# same as bt.exrem.time.exit function!!!
    bt.apply.matrix(weight, bt.ts.time.stop, nlen)
}


# based on bt.apply.matrix
#' @export 
bt.price.stop <- function(b, price, pstop)
{
	out = b
	out[] = NA
	nsymbols = ncol(b)
	
	if(is.null(dim(pstop))) pstop = rep.row(pstop, nrow(b))
	
	for( i in 1:nsymbols )
		out[,i] = bt.ts.price.stop(coredata(b[,i]), coredata(price[,i]), coredata(pstop[,i]))
	return(out)
}


# based on bt.apply.matrix
#' @export 
bt.time.price.stop <- function(b, nlen, price, pstop)
{
	out = b
	out[] = NA
	nsymbols = ncol(b)
	
	if(is.null(dim(pstop))) pstop = rep.row(pstop, nrow(b))
	
	for( i in 1:nsymbols )
		out[,i] = bt.ts.time.price.stop(coredata(b[,i]), nlen, coredata(price[,i]), coredata(pstop[,i]))
	return(out)
}


###############################################################################
# enter signal: weight != 0
# exit signal : weight == 0 or 1 to -1 flip signal
# no signal   : is.na(weight)
#' @export 
###############################################################################
bt.ts.trade.index <- function(x)
{
    # index of enter signals
    enter = x != 0
    enter[is.na(enter)] = FALSE
    enter.index = which(enter)
    

    # index of exit signals corresponding to enter signals
    # capute both x == 0 and 1 to -1 flip signal
    temp = ifna.prev(x)
		temp0 = mlag(temp)    	
    exit = temp0 != 0 & temp != temp0
    exit[ !exit ] = NA
    exit = ifna.prevx.rev(exit)
    
    list(enter = enter, enter.index = enter.index, exit = exit)
}


###############################################################################
# Exit position if holding periods > nlen
#' @export 
###############################################################################
bt.ts.time.stop <- function(x, nlen)
{
	# get index of trades
	temp = bt.ts.trade.index(x)
		enter = temp$enter
		enter.index = temp$enter.index
		exit = temp$exit


    # loop over all enter signals and apply stop
    for(t in enter.index)
	if( enter[ t ] )
		if( exit[ t ] < t + nlen )
			enter[ t : exit[ t ] ] = FALSE
		else {
			enter[ t : (t + nlen) ] = FALSE
		    x[ (t + nlen) ] = 0
		}		    
    return(x)
}


time.stop.test <- function() {
	bt.ts.time.stop(c(1,1,1,0,1,1,NA,1,0),2)
	bt.ts.time.stop(c(1,0,1,1,1,1,1,1,1),3)
}


###############################################################################
# Exit long  position if price.today < price.enter - stop
# Exit short position if price.today > price.enter + stop
#
# price SHOULD NOT contain any non-leading NA's!!!
#
#' @export 
###############################################################################
bt.ts.price.stop <- function(x, price, pstop)
{
	price = coredata(price)
	pstop = coredata(pstop)

	if(length(pstop) == 1) pstop = fast.rep(pstop, len(x))

	# faster which
	dummy = 1:length(x)

	# get index of trades
	temp = bt.ts.trade.index(x)
		enter = temp$enter
		enter.index = temp$enter.index
		exit = temp$exit

 
	# loop over all enter signals and apply stop
    for(t in enter.index)
	if( enter[ t ] ) {
		if( x[ t ] > 0 )
			temp = price[ t : exit[ t ] ] < price[ t ] - pstop[ t ]
		else
			temp = price[ t : exit[ t ] ] > price[ t ] + pstop[ t ]
			
		if( any(temp, na.rm=T) ) {
			iexit = t - 1 + dummy[temp][1]
			enter[ t : iexit ] = FALSE
		    x[ iexit ] = 0				
		} else
			enter[ t : exit[ t ] ] = FALSE			
	}
    return(x)
}


price.stop.test <- function() {
	bt.ts.price.stop(c(1,1,1,1,1,1,NA,1,0), 
				c(1,1,0.9,0.7,1,1,1,1,0),
				0.2
				)
				
	bt.ts.price.stop(-c(1,1,1,1,1,1,NA,1,0), 
				c(1,1,0.9,1.7,1,1,1,1,0),
				0.2
				)
	
}


###############################################################################
# Exit position if either time stop or price stop
#
# price SHOULD NOT contain any non-leading NA's!!!
#
#' @export 
###############################################################################
bt.ts.time.price.stop <- function(x, nlen, price, pstop)
{
	price = coredata(price)
	pstop = coredata(pstop)

	if(length(pstop) == 1) pstop = fast.rep(pstop, len(x))

	# faster which
	dummy = 1:length(x)
	
	# get index of trades
	temp = bt.ts.trade.index(x)
		enter = temp$enter
		enter.index = temp$enter.index
		exit = temp$exit
	

	# loop over all enter signals and apply time and price stop    
    for(t in enter.index)
	if( enter[ t ] ) {
		if( x[ t ] > 0 )
			temp = price[ t : exit[ t ] ] < price[ t ] - pstop[ t ]
		else
			temp = price[ t : exit[ t ] ] > price[ t ] + pstop[ t ]
			
		if( any(temp, na.rm=T) ) {
			iexit = t - 1 + dummy[temp][1]
			
			if( iexit < t + nlen ) {
				enter[ t : iexit ] = FALSE
			    x[ iexit ] = 0				
			} else {
				enter[ t : (t + nlen) ] = FALSE
			    x[ (t + nlen) ] = 0
			}			
		} else
			if( exit[ t ] < t + nlen )
				enter[ t : exit[ t ] ] = FALSE
			else {
				enter[ t : (t + nlen) ] = FALSE
			    x[ (t + nlen) ] = 0
			}
	}
    return(x)
}

time.price.stop.test <- function() {
	bt.ts.time.price.stop(c(1,1,1,1,1,1,NA,1,0), 
				4,
				c(1,1,0.9,0.7,1,1,1,1,0),
				0.2
				)
				
	bt.ts.time.price.stop(-c(1,1,1,1,1,1,NA,1,0), 
				4,
				c(1,1,0.9,1.7,1,1,1,1,0),
				0.2
				)
	
}







###############################################################################
# Price stop with user defined fn
###############################################################################
# stop.fn is expected to return a boolean array of size tend - tstart + 1
# with TRUE(s) idicating that stop was activated
#
# price SHOULD NOT contain any non-leading NA's!!!
#
# time comparison with xts is usually tricky; hence it is always wise to 
# provide all inputs in after coredata
#
custom.stop.fn <- function(x, price, stop.fn, ...)
{
	price = coredata(price)

	if(is.character(stop.fn)) stop.fn = match.fun(stop.fn)

	# faster which
	dummy = 1:length(x)
		
	# get index of trades
	temp = bt.ts.trade.index(x)
		enter = temp$enter
		enter.index = temp$enter.index
		exit = temp$exit


    # loop over all enter signals and apply stop
    for(t in enter.index)
	if( enter[ t ] ) {
		# temp = stop.fn(x[ t ], price, t, exit[ t ], ...)
		temp = stop.fn(x[ t ], price, t, exit[ (t + 1) ], ...)
			
		if( any(temp, na.rm=T) ) {
			iexit = t - 1 + dummy[temp][1]
			enter[ t : iexit ] = FALSE
		    x[ iexit ] = 0				
		} else
			enter[ t : exit[ t ] ] = FALSE			
	}
    return(x)
}
	


# note that this is a custom function because HHV (i.e. cummax) and is path dependent
custom.trailing.stop.test <- function(weight, price, tstart, tend, sma, nstop) {
	index = tstart : tend
	if(weight > 0) {
		# trailing stop
		temp = price[ index ] < cummax(0.9 * sma[ index ])
		
		# profit target
		temp = temp | price[ index ] > cummax(1.1 * sma[ index ])				
	} else {
		temp = price[ index ] > cummax(1.1 * sma[ index ])
	}
	
	# time stop
	if( tend - tstart > nstop ) temp[ (nstop + 1) ] = T
	
	return( temp )	
}


###############################################################################
# Tests
###############################################################################
custom.stop.fn.test <- function() {
	signal = c(1,1,1,1,1,1,NA,1,0)
	price = c(1,1,0.9,0.7,1,1,1,1,0)
	custom.stop.fn(signal, price,
				custom.trailing.stop.test,
				sma = ifna(SMA(price, 2), price),
				nstop = 20
				)
				

	signal = -c(1,1,1,1,1,1,NA,1,0)
	price = c(1,1,0.9,1.7,1,1,1,1,0)
	custom.stop.fn(signal, price,
				custom.trailing.stop.test,
				sma = ifna(SMA(price, 2), price),
				nstop = 4
				)
	
}


###############################################################################
# Tests
# http://www.optionetics.com/market/articles/2012/08/22/kaeppels-corner-the-40-week-cycle-and-theory-versus-reality
###############################################################################
custom.trailing.stop <- function(weight, price, tstart, tend, pstop) {
	index = tstart : tend
	if(weight > 0) {
		temp = price[ index ] < (1 - pstop) * cummax(price[ index ])
	} else {
		temp = price[ index ] > (1 + pstop) * cummin(price[ index ])
	}
	return( temp )	
}	

custom.fixed.stop <- function(weight, price, tstart, tend, pstop) {
	index = tstart : tend
	if(weight > 0)
		price[ index ] < (1 - pstop) * price[ tstart ]
	else
		price[ index ] > (1 + pstop) * price[ tstart ]
}


kaeppels.custom.strategy.plot <- function(
	data,
	models,
	name,
	dates = '::',
	main = NULL,
	layout = NULL,		# flag to idicate if layout is already set	
	...
) {
	if(is.null(layout)) layout(1:2)
	
	# highlight logic based on weight
    weight = models[[name]]$weight[dates]
    	col = iif(weight > 0, 'green', iif(weight < 0, 'red', 'white'))
    	plota.control$col.x.highlight = col.add.alpha(col, 100)
    	highlight = T
       	     
    plota(data$prices[dates], type='l', x.highlight = highlight, ...)
   	plota.legend('Long,Short,Not Invested','green,red,white')
   	
    if(!is.null(main))
    	legend('top', legend=main, bty='n')
}


bt.stop.test <- function()
{
	load.packages('quantmod')
	
	#*****************************************************************
	# Load historical data
	#****************************************************************** 	
	tickers = spl('SPY')	
	tickers = spl('^DJI')
	tickers = spl('DIA')
	tickers = spl('^GSPC')
	
	data <- new.env()
		getSymbols(tickers, src = 'yahoo', from = '1967-01-01', env = data, auto.assign = T)
	bt.prep(data, align='keep.all', dates='1967:04:21::')

	#*****************************************************************
	# Code Strategies
	#****************************************************************** 
	prices = data$prices   
	nperiods = nrow(prices)	
	
	models = list()
	
	#*****************************************************************
	# Code Strategies
	#****************************************************************** 
	data$weight[] = NA
		data$weight[] = 1
	models$buy.hold = bt.run.share(data, clean.signal=T)
	

	#*****************************************************************
	# 40W cycle
	#******************************************************************
	# start 40W - 21-04-1967
	# 280 days
	# 140 days bullish
	# 180 days bear
	# bull 13-07-2012
	# bear 30-11-2012
	# bull 19-4-2013
	#******************************************************************
	start.cycle = as.Date("1967-04-21")
	
	diff = data$dates - start.cycle
	diff.cyc = diff / 280
	diff.int = as.integer(diff.cyc)
	
	signal=iif((diff.cyc-diff.int) < 0.5, 1, 0)
		# to prevent entering the same signal after the stop
		signal = exrem(signal)

	data$weight[] = NA
		data$weight[] = signal
	models$cycle = bt.run.share(data, clean.signal=T, trade.summary = TRUE)

	#*****************************************************************
	# Add 12.5% fixed stop loss
	#****************************************************************** 
	# same as custom.fixed.stop
	#data$weight[] = NA
	#	data$weight[] = bt.ts.price.stop(signal, prices, 8.5/100 * prices)    	
	#models$cycle.12.stop = bt.run.share(data, clean.signal=T, trade.summary = TRUE)
		
	pstop = 8.5 / 100
	
	data$weight[] = NA
		data$weight[] = custom.stop.fn(coredata(signal), coredata(prices), custom.fixed.stop, pstop = pstop)
	models$cycle.fixed.stop = bt.run.share(data, clean.signal=T, trade.summary = TRUE)
	
	data$weight[] = NA
		data$weight[] = custom.stop.fn(coredata(signal), coredata(prices), custom.trailing.stop, pstop = pstop)
	models$cycle.trailing.stop = bt.run.share(data, clean.signal=T, trade.summary = TRUE)	
	#*****************************************************************
	# Create Report
	#****************************************************************** 
	plotbt.custom.report.part1(models)
	plotbt.custom.report.part2(models$cycle)
	plotbt.strategy.sidebyside(models)
	plotbt.custom.report.part3(models$cycle, trade.summary = TRUE)
 
	
	strategy.performance.snapshoot(models, T)

 
	plotbt.custom.report.part2(models$cycle.trailing.stop)	
	plotbt.custom.report.part3(models$cycle.trailing.stop, trade.summary = TRUE)
	#models$cycle.trailing.stop$trade.summary$trades
	
	

	#*****************************************************************
	# Create Plot
	#****************************************************************** 
	layout(1:3)
	kaeppels.custom.strategy.plot(data, models, 'cycle', dates = '2009:04::', layout=T, main = '40 week cycle', plotX = F)
	kaeppels.custom.strategy.plot(data, models, 'cycle.fixed.stop', dates = '2009:04::', layout=T, main = '40 week cycle fixed stop', plotX = F)
	kaeppels.custom.strategy.plot(data, models, 'cycle.trailing.stop', dates = '2009:04::', layout=T, main = '40 week cycle trailing stop')
		
}

