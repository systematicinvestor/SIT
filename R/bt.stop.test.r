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
# Test for Stop functionality for Backtests
# Copyright (C) 2013  Michael Kapler
#
# For more information please visit my blog at www.SystematicInvestor.wordpress.com
# or drop me a line at TheSystematicInvestor at gmail
###############################################################################



###############################################################################
# Helper function to visualize signal / strategy
#
# plot strategy, highligh invested periods with green
#' @export 
###############################################################################
bt.stop.strategy.plot <- function(
	data,
	model,
	dates = '::',
	main = NULL,
	layout = NULL,		# flag to idicate if layout is already set	
	extra.plot.fn = NULL,
	...
) {
	# highlight logic based on weight
    weight = model$weight[dates]
    	col = iif(weight > 0, 'green', iif(weight < 0, 'gray', 'white'))
    	plota.control$col.x.highlight = col.add.alpha(col, 100)
    	highlight = T
    
    if(is.null(layout)) layout(1)
       	     
    plota(data$prices[dates], type='l', x.highlight = highlight, ...)
    
    if(!is.null(extra.plot.fn)) match.fun(extra.plot.fn)()
    
   	plota.legend('Long,Short,Not Invested','green,gray,white')
   	
    if(!is.null(main))
    	legend('top', legend=main, bty='n')    	
}


###############################################################################
# Tests
# http://www.optionetics.com/market/articles/2012/08/22/kaeppels-corner-the-40-week-cycle-and-theory-versus-reality
###############################################################################
bt.stop.kaeppels.40w.test <- function()
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
	# Stops
	#****************************************************************** 
	fixed.stop <- function(weight, price, tstart, tend, pstop) {
		index = tstart : tend
		if(weight > 0)
			price[ index ] < (1 - pstop) * price[ tstart ]
		else
			price[ index ] > (1 + pstop) * price[ tstart ]
	}	
	
	trailing.stop <- function(weight, price, tstart, tend, pstop) {
		index = tstart : tend
		if(weight > 0) {
			temp = price[ index ] < (1 - pstop) * cummax(price[ index ])
		} else {
			temp = price[ index ] > (1 + pstop) * cummin(price[ index ])
		}
		return( temp )	
	}	
		
	#*****************************************************************
	# Add 12.5% fixed stop loss
	#****************************************************************** 
	# same as custom.fixed.stop
	#data$weight[] = NA
	#	data$weight[] = bt.ts.price.stop(signal, prices, 8.5/100 * prices)    	
	#models$cycle.12.stop = bt.run.share(data, clean.signal=T, trade.summary = TRUE)
		
	pstop = 8.5 / 100
	
	data$weight[] = NA
		data$weight[] = custom.stop.fn(coredata(signal), coredata(prices), fixed.stop, pstop = pstop)
	models$cycle.fixed.stop = bt.run.share(data, clean.signal=T, trade.summary = TRUE)
	
	data$weight[] = NA
		data$weight[] = custom.stop.fn(coredata(signal), coredata(prices), trailing.stop, pstop = pstop)
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
	dates = '2009:04::'
	
	layout(1:3)
	bt.stop.strategy.plot(data, models$cycle, dates = dates, layout=T, main = '40 week cycle', plotX = F)
	bt.stop.strategy.plot(data, models$cycle.fixed.stop, dates = dates, layout=T, main = '40 week cycle fixed stop', plotX = F)
	bt.stop.strategy.plot(data, models$cycle.trailing.stop, dates = dates, layout=T, main = '40 week cycle trailing stop')

}
	
	



###############################################################################
# MA Cross strategy with various stop examples
#
# http://www.investopedia.com/articles/trading/08/trailing-stop-loss.asp
###############################################################################
bt.stop.ma.cross.test <- function()
{
	load.packages('quantmod')
	
	#*****************************************************************
	# Load historical data
	#****************************************************************** 	
	tickers = spl('SPY')	
	
	data <- new.env()
		getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)		
	bt.prep(data, align='keep.all', dates='1999::')

	#*****************************************************************
	# Code Strategies
	#****************************************************************** 
	prices = data$prices   
	
	models = list()
	
	#*****************************************************************
	# Code Strategies
	#****************************************************************** 
	data$weight[] = NA
		data$weight[] = 1
	models$buy.hold = bt.run.share(data, clean.signal=T)

	#*****************************************************************
	# Code Strategies : MA Cross Over
	#****************************************************************** 
	sma.fast = SMA(prices, 20)
	sma.slow = SMA(prices, 50)

	buy.signal = iif(cross.up(sma.fast, sma.slow), 1, NA)
	
	data$weight[] = NA
		data$weight[] = iif(cross.up(sma.fast, sma.slow), 1, iif(cross.dn(sma.fast, sma.slow), 0, NA))
	models$ma.cross = bt.run.share(data, clean.signal=T, trade.summary = TRUE)

	#*****************************************************************
	# Stops
	#****************************************************************** 
	# fixed stop: exit trade once price falls below % from entry price
	fixed.stop <- function(weight, price, tstart, tend, pstop) {
		index = tstart : tend
		if(weight > 0)
			price[ index ] < (1 - pstop) * price[ tstart ]
		else
			price[ index ] > (1 + pstop) * price[ tstart ]
	}
	
	# trailing stop: exit trade once price falls below % from max price since start of trade
	trailing.stop <- function(weight, price, tstart, tend, pstop) {
		index = tstart : tend
		if(weight > 0) {
			temp = price[ index ] < (1 - pstop) * cummax(price[ index ])
		} else {
			temp = price[ index ] > (1 + pstop) * cummin(price[ index ])
		}
		return( temp )	
	}	
	
	# trailing stop: exit trade once price either
	# - falls below % from max price since start of trade OR
	# - rises above % from entry price
	trailing.stop.profit.target <- function(weight, price, tstart, tend, pstop, pprofit) {
		index = tstart : tend
		if(weight > 0) {
			temp = price[ index ] < (1 - pstop) * cummax(price[ index ])
			
			# profit target
			temp = temp | price[ index ] > (1 + pprofit) * price[ tstart ]
		} else {
			temp = price[ index ] > (1 + pstop) * cummin(price[ index ])
			
			# profit target
			temp = temp | price[ index ] < (1 - pprofit) * price[ tstart ]		
		}
		return( temp )	
	}		
	
	#*****************************************************************
	# Exit using fixed stop
	#****************************************************************** 
	data$weight[] = NA
		data$weight[] = custom.stop.fn(coredata(buy.signal), coredata(prices), fixed.stop, 
		pstop = 1/100)
	models$ma.cross.fixed.stop = bt.run.share(data, clean.signal=T, trade.summary = TRUE)
		
	#*****************************************************************
	# Exit using trailing stop
	#****************************************************************** 
	data$weight[] = NA
		data$weight[] = custom.stop.fn(coredata(buy.signal), coredata(prices), trailing.stop, 
		pstop = 1/100)
	models$ma.cross.trailing.stop = bt.run.share(data, clean.signal=T, trade.summary = TRUE)
	
	#*****************************************************************
	# Exit using trailing stop or profit target
	#****************************************************************** 		
	data$weight[] = NA
		data$weight[] = custom.stop.fn(coredata(buy.signal), coredata(prices), trailing.stop.profit.target, 
		pstop = 1/100, pprofit = 1.5/100)
	models$ma.cross.trailing.stop.profit.target = bt.run.share(data, clean.signal=T, trade.summary = TRUE)
			
	#*****************************************************************
	# Create Report
	#****************************************************************** 	
jpeg(filename = 'plot1.jpg', width = 500, height = 500, units = 'px', pointsize = 12)		
	
	strategy.performance.snapshoot(models, T)

dev.off()		
	#*****************************************************************
	# Create Plot
	#****************************************************************** 	
	dates = '2010::2010'
	# add moving averages to the strategy plot
	extra.plot.fn <- function() {
		plota.lines(sma.fast, col='red')
		plota.lines(sma.slow, col='blue')
	}
	
jpeg(filename = 'plot2.jpg', width = 500, height = 500, units = 'px', pointsize = 12)		

    layout(1:4)
	bt.stop.strategy.plot(data, models$ma.cross, dates = dates, layout=T, main = 'MA Cross', extra.plot.fn = extra.plot.fn, plotX = F)
	bt.stop.strategy.plot(data, models$ma.cross.fixed.stop, dates = dates, layout=T, main = 'Fixed Stop', plotX = F)
	bt.stop.strategy.plot(data, models$ma.cross.trailing.stop, dates = dates, layout=T, main = 'Trailing Stop', plotX = F)
	bt.stop.strategy.plot(data, models$ma.cross.trailing.stop.profit.target, dates = dates, layout=T, main = 'Trailing Stop and Profit Target')
	
dev.off()		
}



###############################################################################
# MA Cross strategy with pull-back and various stop examples
#
# http://www.investopedia.com/articles/trading/08/trailing-stop-loss.asp
###############################################################################
bt.stop.ma.cross.pullback.test <- function()
{
	load.packages('quantmod')
	
	#*****************************************************************
	# Load historical data
	#****************************************************************** 	
	tickers = spl('SPY')	
	tickers = spl('^GSPC')
	
	data <- new.env()
		getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
	bt.prep(data, align='keep.all', dates='1999::')

	#*****************************************************************
	# Code Strategies
	#****************************************************************** 
	prices = data$prices   
	
	models = list()
	
	#*****************************************************************
	# Code Strategies
	#****************************************************************** 
	data$weight[] = NA
		data$weight[] = 1
	models$buy.hold = bt.run.share(data, clean.signal=T)

	#*****************************************************************
	# Code Strategies
	#****************************************************************** 
	sma.fast = SMA(prices, 50)
	sma.slow = SMA(prices, 200)

	data$weight[] = NA
		data$weight[] = iif(cross.up(prices, sma.slow), 1, iif(cross.dn(prices, sma.slow), 0, NA))
	models$ma.crossover = bt.run.share(data, clean.signal=T, trade.summary = TRUE)

	data$weight[] = NA
		data$weight[] = iif(cross.up(prices, sma.slow), 1, iif(cross.dn(prices, 0.95 * sma.slow), 0, NA))
	models$ma.crossover.pullback = bt.run.share(data, clean.signal=T, trade.summary = TRUE)

	
	#*****************************************************************
	# Stops
	#****************************************************************** 
	fixed.stop <- function(weight, price, tstart, tend, pstop) {
		index = tstart : tend
		if(weight > 0)
			price[ index ] < (1 - pstop) * price[ tstart ]
		else
			price[ index ] > (1 + pstop) * price[ tstart ]
	}	
	
	trailing.stop <- function(weight, price, tstart, tend, pstop) {
		index = tstart : tend
		if(weight > 0) {
			temp = price[ index ] < (1 - pstop) * cummax(price[ index ])
		} else {
			temp = price[ index ] > (1 + pstop) * cummin(price[ index ])
		}
		return( temp )	
	}
			
	#*****************************************************************
	# Code Strategies
	#****************************************************************** 	
	signal = iif(cross.up(prices, sma.slow), 1, iif(cross.dn(prices, sma.slow), 0, NA))
	signal = iif(cross.up(prices, sma.slow), 1, NA)
	signal = iif(cross.up(prices, sma.slow), 1, iif(cross.dn(prices, 0.95 * sma.slow), 0, NA))
	pstop = 8.5 / 100
	
	data$weight[] = NA
		data$weight[] = custom.stop.fn(coredata(signal), coredata(prices), fixed.stop, pstop = pstop)
	models$cycle.fixed.stop = bt.run.share(data, clean.signal=T, trade.summary = TRUE)
	
	data$weight[] = NA
		data$weight[] = custom.stop.fn(coredata(signal), coredata(prices), trailing.stop, pstop = pstop)
	models$cycle.trailing.stop = bt.run.share(data, clean.signal=T, trade.summary = TRUE)	

	#*****************************************************************
	# Create Report
	#****************************************************************** 	
	strategy.performance.snapshoot(models, T)


	#*****************************************************************
	# Create Plot
	#****************************************************************** 	
	dates = '::'
	# add moving averages to the strategy plot
	extra.plot.fn <- function() {
	    plota.lines(sma.slow, col='blue')
    	plota.lines(0.95 * sma.slow, col='red')
	}	
	
	layout(1:4)
	bt.stop.strategy.plot(data, models$ma.crossover, dates = dates, layout=T, main = 'base', plotX = F)
	bt.stop.strategy.plot(data, models$ma.crossover.pullback, dates = dates, layout=T, main = 'base + 5% pull-back', extra.plot.fn = extra.plot.fn, plotX = F)	
	bt.stop.strategy.plot(data, models$cycle.fixed.stop, dates = dates, layout=T, main = 'fixed stop', plotX = F)
	bt.stop.strategy.plot(data, models$cycle.trailing.stop, dates = dates, layout=T, main = 'trailing stop')
	
	
}	
	




###############################################################################
# Dual Moving Average System
# http://www.tradingblox.com/Manuals/UsersGuideHTML/dualmovingaverage.htm
###############################################################################
bt.stop.dual.ma.strategy <- function
(
	data,
	short.ma.len = 50,
	long.ma.len = 200,
	n.atr = NA,
	atr.len = 39	
) 
{
	#*****************************************************************
	# Stop
	#****************************************************************** 	
	atr.trailing.stop <- function(weight, price, tstart, tend, atr) {
		index = tstart : tend
		if(weight > 0)
			price[ index ] < cummax( price[ index ] ) - atr[ index ]
		else
			price[ index ] > cummin( price[ index ] ) + atr[ index ]
	}	

	#*****************************************************************
	# The MA Crossover system
	#****************************************************************** 	
	prices = data$prices 
	short.ma = SMA(prices, short.ma.len)
	long.ma = SMA(prices, long.ma.len)
	
	signal = iif(cross.up(short.ma, long.ma), 1, iif(cross.dn(short.ma, long.ma), 0, NA))
	if( !is.na(n.atr) ) {
		atr = bt.apply(data, function(x) ATR(HLC(x), atr.len)[,'atr'])
	
		signal = custom.stop.fn(coredata(signal), coredata(prices),
							atr.trailing.stop, atr = coredata(n.atr * atr))
	}
	
	data$weight[] = NA
		data$weight[] = signal
	bt.run.share(data, clean.signal=T)
}


bt.stop.dual.ma.test <- function()
{
	load.packages('quantmod')
	
	#*****************************************************************
	# Load historical data
	#****************************************************************** 	
	tickers = spl('SPY')	
	
	data <- new.env()
		getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
	bt.prep(data, align='keep.all')

	#*****************************************************************
	# Code Strategies
	#****************************************************************** 
	models = list()
	
	#*****************************************************************
	# Code Strategies
	#****************************************************************** 
	data$weight[] = NA
		data$weight[] = 1
	models$buy.hold = bt.run.share(data, clean.signal=T)

	#*****************************************************************
	# Dual Moving Average System parameters
	#****************************************************************** 	
	models$ma = bt.stop.dual.ma.strategy(data, 50, 200)
	
	models$ma.stop = bt.stop.dual.ma.strategy(data, 50, 200, 5, 39)
		
	#*****************************************************************
	# Create Report
	#****************************************************************** 
	
	strategy.performance.snapshoot(models, T)


	#*****************************************************************
	# Create Plot
	#****************************************************************** 	
	dates = '2010::'
	# add moving averages to the strategy plot
	extra.plot.fn <- function() {
		short.ma.len = 50
		long.ma.len = 200
		n.atr = 5
		atr.len = 39	    
	    
		short.ma = SMA(data$prices, short.ma.len)
		long.ma = SMA(data$prices, long.ma.len)	
		atr = bt.apply(data, function(x) ATR(HLC(x), atr.len)[,'atr'])
		
	    plota.lines(short.ma, col='red')
	    plota.lines(long.ma, col='blue')    
	    plota.lines(data$prices - n.atr * atr, col='orange')    
	}	
	
	layout(1:2)
	bt.stop.strategy.plot(data, models$ma, dates = dates, layout=T, main = 'MA base', extra.plot.fn=extra.plot.fn, plotX = F)
	bt.stop.strategy.plot(data, models$ma.stop, dates = dates, layout=T, main = 'base + 5% pull-back', extra.plot.fn=extra.plot.fn)
	
	
	
}


		
