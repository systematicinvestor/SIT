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
# Evaluting Sample Trading Strategies using Backtesting library in 
# the Systematic Investor Toolbox
# Copyright (C) 2011  Michael Kapler
#
# For more information please visit my blog at www.SystematicInvestor.wordpress.com
# or drop me a line at TheSystematicInvestor at gmail
###############################################################################

bt.empty.test <- function() 
{	
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load.packages('quantmod')	
	tickers = spl('SPY')

	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)	
	bt.prep(data, align='keep.all', dates='1970::2011')

	#*****************************************************************
	# Code Strategies
	#****************************************************************** 
	prices = data$prices    
	
	# Buy & Hold	
	data$weight[] = 0
	buy.hold = bt.run(data, trade.summary=T)
	
	#*****************************************************************
	# Create Report
	#****************************************************************** 	
	
	plotbt.custom.report.part1( buy.hold, trade.summary =T)
	plotbt.custom.report.part2( buy.hold, trade.summary =T)
	plotbt.custom.report.part3( buy.hold, trade.summary =T)

}

###############################################################################
# How to use execution.price functionality
###############################################################################
bt.execution.price.test <- function() 
{	 
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load.packages('quantmod')	
	tickers = spl('SPY')

	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)			
	bt.prep(data, align='keep.all', dates='1970::')	
	
	#*****************************************************************
	# Code Strategies
	#****************************************************************** 
	prices = data$prices   
	nperiods = nrow(prices)
	
	models = list()
	
	#*****************************************************************
	# Buy & Hold
	#****************************************************************** 
	data$weight[] = 0
		data$execution.price[] = NA
		data$weight[] = 1
	models$buy.hold = bt.run.share(data, clean.signal=T)

	#*****************************************************************
	# MA cross-over strategy
	#****************************************************************** 
	sma.fast = SMA(prices, 50)
	sma.slow = SMA(prices, 200)
		signal = iif(sma.fast >= sma.slow, 1, -1)
	
	data$weight[] = NA
		data$execution.price[] = NA
		data$weight[] = signal
	models$ma.crossover = bt.run.share(data, clean.signal=T, trade.summary = TRUE)

	#*****************************************************************
	# MA cross-over strategy, add 10c per share commission
	#*****************************************************************	
	data$weight[] = NA
		data$execution.price[] = NA
		data$weight[] = signal
	models$ma.crossover.com = bt.run.share(data, commission = 0.1, clean.signal=T)
	
	#*****************************************************************
	# MA cross-over strategy:
	# Exit trades at the close on the day of the signal
	# Enter trades at the open the next day after the signal	
	#****************************************************************** 
	popen = bt.apply(data, Op)		
	signal.new = signal
		trade.start	 = which(signal != mlag(signal) & signal != 0)
		signal.new[trade.start] = 0
		trade.start = trade.start + 1
		
	data$weight[] = NA
		data$execution.price[] = NA
		data$execution.price[trade.start,] = popen[trade.start,]
		data$weight[] = signal.new
	models$ma.crossover.enter.next.open = bt.run.share(data, clean.signal=T, trade.summary = TRUE)
			
	
	#*****************************************************************
	# Create Report
	#****************************************************************** 	
	# put all reports into one pdf file
	#pdf(file = 'report.pdf', width=8.5, height=11)
		models = rev(models)
	

png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')		
		
		# Plot perfromance
		plotbt(models, plotX = T, log = 'y', LeftMargin = 3)	    	
			mtext('Cumulative Performance', side = 2, line = 1)

dev.off()				

png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')		
			
		# Plot trades
		plotbt.custom.report.part3(models$ma.crossover, trade.summary = TRUE)		
		
dev.off()					
png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')		

		plotbt.custom.report.part3(models$ma.crossover.enter.next.open, trade.summary = TRUE)		
		
dev.off()			
	#dev.off()	

	
	
		
	
	
	
		
		
		
		
		
	#*****************************************************************
	# Simple example showing the difference in a way commission is integrated into returns
	#****************************************************************** 	
	commission = 4
	data$weight[] = NA
		data$execution.price[] = NA
		data$weight[201,] = 1
		data$weight[316,] = 0				
		data$execution.price[201,] = prices[201,] + commission
		data$execution.price[316,] = prices[316,] - commission		
	models$test.com = bt.run.share(data, clean.signal=T, trade.summary=T)

	data$weight[] = NA
		data$execution.price[] = NA
		data$weight[201,] = 1
		data$weight[316,] = 0					
	models$test.com.new = bt.run.share(data, commission=commission, trade.summary=T, clean.signal=T)

	cbind(last(models$test.com$equity), last(models$test.com.new$equity),
		as.double(prices[316] - commission)/as.double(prices[201] + commission))
	
	as.double(prices[202]) / as.double(prices[201] + commission)-1
	models$test.com$equity[202]-1
	
	as.double(prices[202] - commission) / as.double(prices[201])-1
	models$test.com.new$equity[202]-1
	
	
	#plotbt.custom.report.part1(models)
	
	#*****************************************************************
	# Example showing the difference in a way commission is integrated into returns
	#****************************************************************** 		
	commission = 0.1
	sma.fast = SMA(prices, 50)
	sma.slow = SMA(prices, 200)

	weight = iif(sma.fast >= sma.slow, 1, -1)	
		weight[] = bt.exrem(weight)
		index = which(!is.na(weight))
		trade.start = index+1		
		trade.end = c(index[-1],nperiods)
		trade.direction = sign(weight[index])
		
		
	data$weight[] = NA
		data$execution.price[] = NA
		data$weight[] = weight
	models$test.com.new = bt.run.share(data, commission=commission, trade.summary=T, clean.signal=T)
	
	
	data$weight[] = NA
		data$execution.price[] = NA
		
		index = which(trade.direction > 0)
		data$execution.price[trade.start[index],] = prices[trade.start[index],] + commission
		data$execution.price[trade.end[index],] = prices[trade.end[index],] - commission
		
		index = which(trade.direction < 0)
		data$execution.price[trade.start[index],] = prices[trade.start[index],] - commission
		data$execution.price[trade.end[index],] = prices[trade.end[index],] + commission
		
		data$weight[trade.start,] = trade.direction
		data$weight[trade.end,] = 0		
		
	models$test.com = bt.run.share(data, clean.signal=T, trade.summary=T)

		
	#plotbt.custom.report.part1(models)
	
}



###############################################################################
# Cross Pollination from Timely Portfolio
# http://timelyportfolio.blogspot.ca/2011/08/drawdown-visualization.html
# http://timelyportfolio.blogspot.ca/2011/08/lm-system-on-nikkei-with-new-chart.html
###############################################################################
bt.timelyportfolio.visualization.test <- function() 
{
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load.packages('quantmod')	
	tickers = spl('SPY')

	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)		
	bt.prep(data, align='keep.all', dates='2000::2011')

	#*****************************************************************
	# Code Strategies
	#****************************************************************** 
	prices = data$prices    
	
	# Buy & Hold	
	data$weight[] = 1
	buy.hold = bt.run(data)	
	
	# Strategy
	ma10 = bt.apply.matrix(prices, EMA, 10)
	ma50 = bt.apply.matrix(prices, EMA, 50)
	ma200 = bt.apply.matrix(prices, EMA, 200)
	data$weight[] = NA;
		data$weight[] = iif(ma10 > ma50 & ma50 > ma200, 1, 
						iif(ma10 < ma50 & ma50 < ma200, -1, 0))
	strategy = bt.run.share(data, clean.signal=F)
	
	
	#*****************************************************************
	# Visualization of system Entry and Exit based on
	# http://timelyportfolio.blogspot.ca/2011/08/lm-system-on-nikkei-with-new-chart.html
	#****************************************************************** 	
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')										
	
	layout(1)
	plota(strategy$eq, type='l', ylim=range(buy.hold$eq,strategy$eq))
	
		col = iif(strategy$weight > 0, 'green', iif(strategy$weight < 0, 'red', 'gray'))
	plota.lines(buy.hold$eq, type='l', col=col)		
	
		plota.legend('strategy,Long,Short,Not Invested','black,green,red,gray')
		
dev.off()			
	#*****************************************************************	
	# Drawdown Visualization 
	# 10% drawdowns in yellow and 15% drawdowns in orange
	# http://timelyportfolio.blogspot.ca/2011/08/drawdown-visualization.html
	#*****************************************************************	
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')										
	
	layout(1:2)
	drawdowns = compute.drawdown(strategy$eq)
	highlight = drawdowns < -0.1
	
	plota.control$col.x.highlight = iif(drawdowns < -0.15, 'orange', iif(drawdowns < -0.1, 'yellow', 0))	
	
	plota(strategy$eq, type='l', plotX=F, x.highlight = highlight, ylim=range(buy.hold$eq,strategy$eq))
		plota.legend('strategy,10% Drawdown,15%  Drawdown','black,yellow,orange')
		
	plota(100*drawdowns, type='l', x.highlight = highlight)
		plota.legend('drawdown', 'black', x='bottomleft')
	
dev.off()		
	#*****************************************************************
	# Create Report
	#****************************************************************** 		
png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')										
	
	plota.control$col.x.highlight = iif(drawdowns < -0.15, 'orange', iif(drawdowns < -0.1, 'yellow', 0))				
	highlight = drawdowns < -0.1

	plotbt.custom.report.part1(strategy, buy.hold, x.highlight = highlight)
	
dev.off()	
		
	
}


###############################################################################
# Improving Trend-Following Strategies With Counter-Trend Entries by david varadi
# http://cssanalytics.wordpress.com/2011/07/29/improving-trend-following-strategies-with-counter-trend-entries/
###############################################################################
bt.improving.trend.following.test <- function() 
{	
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load.packages('quantmod')	
	tickers = spl('SPY')

	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)		
	bt.prep(data, align='keep.all', dates='1970::2011')

	#*****************************************************************
	# Code Strategies
	#****************************************************************** 
	prices = data$prices    
	
	# Buy & Hold	
	data$weight[] = 1
	buy.hold = bt.run(data)	

	# Trend-Following strategy: Long[Close > SMA(10) ]
	sma = bt.apply(data, function(x) { SMA(Cl(x), 10) } )	
	data$weight[] = NA
		data$weight[] = iif(prices >= sma, 1, 0)
	trend.following = bt.run(data, trade.summary=T)			

	# Trend-Following With Counter-Trend strategy: Long[Close > SMA(10), DVB(1) CounterTrend ]
	dv = bt.apply(data, function(x) { DV(HLC(x), 1, TRUE) } )	
	data$weight[] = NA
		data$weight[] = iif(prices > sma & dv < 0.25, 1, data$weight)
		data$weight[] = iif(prices < sma & dv > 0.75, 0, data$weight)
	trend.following.dv1 = bt.run(data, trade.summary=T)			

	#*****************************************************************
	# Create Report
	#****************************************************************** 	
	
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')										
	plotbt.custom.report.part1(trend.following.dv1, trend.following, buy.hold)
dev.off()	


png(filename = 'plot2.png', width = 1200, height = 800, units = 'px', pointsize = 12, bg = 'white')	
	plotbt.custom.report.part2(trend.following.dv1, trend.following, buy.hold)
dev.off()	
	

	#*****************************************************************
	# Sensitivity Analysis
	#****************************************************************** 
	ma.lens = seq(10, 100, by = 10)
	dv.lens = seq(1, 5, by = 1)

	# precompute indicators
	mas = matrix(double(), nrow(prices), len(ma.lens))
	dvs = matrix(double(), nrow(prices), len(dv.lens))

	for(i in 1:len(ma.lens)) {
		ma.len = ma.lens[i]
		mas[, i] = bt.apply(data, function(x) { SMA(Cl(x), ma.len) } )
	}
	for(i in 1:len(dv.lens)) {
		dv.len = dv.lens[i]
		dvs[,i] = bt.apply(data, function(x) { DV(HLC(x), dv.len, TRUE) } )
	}

	# allocate matrixes to store backtest results
	dummy = matrix(double(), len(ma.lens), 1+len(dv.lens))
		rownames(dummy) = paste('SMA', ma.lens)
		colnames(dummy) = c('NO', paste('DV', dv.lens))
		
	out = list()
		out$Cagr = dummy
		out$Sharpe = dummy
		out$DVR = dummy
		out$MaxDD = dummy
	
	# evaluate strategies
	for(ima in 1:len(ma.lens)) {
		sma = mas[, ima]
		cat('SMA =', ma.lens[ima], '\n')

		for(idv in 0:len(dv.lens)) {			
			if( idv == 0 ) {
				data$weight[] = NA
					data$weight[] = iif(prices > sma, 1, 0)			
			} else {
				dv = dvs[, idv]
				
				data$weight[] = NA
					data$weight[] = iif(prices > sma & dv < 0.25, 1, data$weight)
					data$weight[] = iif(prices < sma & dv > 0.75, 0, data$weight)
			}
			strategy = bt.run(data, silent=T)			
			
			# add 1 to account for benchmark case, no counter-trend
			idv = idv + 1
			out$Cagr[ima, idv] = compute.cagr(strategy$equity)
			out$Sharpe[ima, idv] = compute.sharpe(strategy$ret)
			out$DVR[ima, idv] = compute.DVR(strategy)
			out$MaxDD[ima, idv] = compute.max.drawdown(strategy$equity)
		}
	}

	#*****************************************************************
	# Create Report
	#****************************************************************** 
	
png(filename = 'plot3.png', width = 800, height = 600, units = 'px', pointsize = 12, bg = 'white')										

	layout(matrix(1:4,nrow=2))	
	for(i in names(out)) {
		temp = out[[i]]
		temp[] = plota.format( 100 * temp, 1, '', '' )
		plot.table(temp, smain = i, highlight = T, colorbar = F)
	}
	
dev.off()	
	
}	
	
	
###############################################################################
# Simple, Long-Term Indicator Near to Giving Short Signal By Woodshedder 
# http://ibankcoin.com/woodshedderblog/2011/08/28/simple-long-term-indicator-near-to-giving-short-signal/
###############################################################################
bt.roc.cross.test <- function() 
{	
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load.packages('quantmod')
	tickers = spl('SPY')

	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)		
	bt.prep(data, align='keep.all', dates='1970::2011')

	#*****************************************************************
	# Code Strategies
	#****************************************************************** 
	prices = data$prices    
	
	# Buy & Hold	
	data$weight[] = 1
	buy.hold = bt.run(data)	

	
	# Strategy: calculate the 5 day rate of change (ROC5) and the 252 day rate of change (ROC252).
	#  Buy (or cover short) at the close if yesterday the ROC252 crossed above the ROC5 and today the ROC252 is still above the ROC5.
	#  Sell (or open short) at the close if yesterday the ROC5 crossed above the ROC252 and today the ROC5 is still above the ROC252.
	roc5 = prices / mlag(prices,5)
	roc252 = prices / mlag(prices,252)
	
	roc5.1 = mlag(roc5,1)
	roc5.2 = mlag(roc5,2)
	roc252.1 = mlag(roc252,1)
	roc252.2 = mlag(roc252,2)
	
	data$weight[] = NA
		data$weight$SPY[] = iif(roc252.2 < roc5.2 & roc252.1 > roc5.1 & roc252 > roc5, 1, data$weight$SPY)
		data$weight$SPY[] = iif(roc252.2 > roc5.2 & roc252.1 < roc5.1 & roc252 < roc5, -1, data$weight$SPY)
	roc.cross = bt.run(data, trade.summary=T)					
       
	#*****************************************************************
	# Create Report
	#****************************************************************** 	
	
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')										
	plotbt.custom.report.part1(roc.cross, buy.hold, trade.summary=T)
dev.off()	

png(filename = 'plot2.png', width = 1200, height = 800, units = 'px', pointsize = 12, bg = 'white')	
	plotbt.custom.report.part2(roc.cross, buy.hold, trade.summary=T)
dev.off()	

png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')	
	plotbt.custom.report.part3(roc.cross, buy.hold, trade.summary=T)
dev.off()	


		
	#*****************************************************************
	# Code Strategies
	#****************************************************************** 
	
	# When shorting always use 	type = 'share' backtest to get realistic results
	# The type = 'weight' backtest assumes that we are constantly adjusting our position
	# to keep all cash = shorts
	data$weight[] = NA
		data$weight$SPY[] = iif(roc252.2 < roc5.2 & roc252.1 > roc5.1 & roc252 > roc5, 1, data$weight$SPY)
		data$weight$SPY[] = iif(roc252.2 > roc5.2 & roc252.1 < roc5.1 & roc252 < roc5, -1, data$weight$SPY)	
		
		capital = 100000
		data$weight[] = (capital / prices) * bt.exrem(data$weight)
	roc.cross.share = bt.run(data, type='share', trade.summary=T, capital=capital)					
		

	#*****************************************************************
	# Create Report
	#****************************************************************** 	
	
png(filename = 'plot4.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')										
	plotbt.custom.report.part1(roc.cross.share, roc.cross, buy.hold, trade.summary=T)
dev.off()	

png(filename = 'plot5.png', width = 1200, height = 800, units = 'px', pointsize = 12, bg = 'white')	
	plotbt.custom.report.part2(roc.cross.share, roc.cross, buy.hold, trade.summary=T)
dev.off()	
	
	
}



###############################################################################
# Rotational Trading Strategies : ETF Sector Strategy
# http://www.etfscreen.com/sectorstrategy.php
###############################################################################
bt.rotational.trading.test <- function() 
{
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load.packages('quantmod')	
	tickers = spl('XLY,XLP,XLE,XLF,XLV,XLI,XLB,XLK,XLU,IWB,IWD,IWF,IWM,IWN,IWO,IWP,IWR,IWS,IWV,IWW,IWZ')	

	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)		
	bt.prep(data, align='keep.all', dates='1970::2011')

	#*****************************************************************
	# Code Strategies
	#****************************************************************** 
	prices = data$prices  
	n = len(tickers)  

	# find month ends
	month.ends = endpoints(prices, 'months')
		month.ends = month.ends[month.ends > 0]		
		
	# Equal Weight
	data$weight[] = NA
		data$weight[month.ends,] = ntop(prices, n)[month.ends,]	
	equal.weight = bt.run.share(data, clean.signal=F)
		
	
	
	# Rank on 6 month return
	position.score = prices / mlag(prices, 126)	
	
	# Select Top 2 funds
	data$weight[] = NA
		data$weight[month.ends,] = ntop(position.score[month.ends,], 2)	
		capital = 100000
		data$weight[] = (capital / prices) * bt.exrem(data$weight)		
	top2 = bt.run(data, type='share', trade.summary=T, capital=capital)

	# Seletop Top 2 funds,  and Keep then till they are in 1:6 rank
	data$weight[] = NA
		data$weight[month.ends,] = ntop.keep(position.score[month.ends,], 2, 6)	
		capital = 100000
		data$weight[] = (capital / prices) * bt.exrem(data$weight)		
	top2.keep6 = bt.run(data, type='share', trade.summary=T, capital=capital)
	
	#*****************************************************************
	# Create Report
	#****************************************************************** 
				
	# put all reports into one pdf file
	pdf(file = 'report.pdf', width=8.5, height=11)
		plotbt.custom.report(top2.keep6, top2, equal.weight, trade.summary=T)
	dev.off()	
	

png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')										
	plotbt.custom.report.part1(top2.keep6, top2, equal.weight, trade.summary=T)
dev.off()	

png(filename = 'plot2.png', width = 1200, height = 800, units = 'px', pointsize = 12, bg = 'white')	
	plotbt.custom.report.part2(top2.keep6, top2, equal.weight, trade.summary=T)
dev.off()	

png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')	
	plotbt.custom.report.part3(top2.keep6, top2, equal.weight, trade.summary=T)
dev.off()	

	
		
}

###############################################################################
# A Quantitative Approach to Tactical Asset Allocation by M. Faber (2006)
# http://www.mebanefaber.com/timing-model/
###############################################################################
bt.timing.model.test <- function() 
{
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load.packages('quantmod')	
	tickers = spl('VTI,VEU,IEF,VNQ,DBC')	
	tickers = spl('VTI,EFA,IEF,ICF,DBC,SHY')	

	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)		
		for(i in ls(data)) cat( i, format(index(data[[i]][1,]), '%d%b%y'), '\n')

	# extend data for Commodities
	CRB = get.CRB()
		index = max(which( index(CRB) < index(data$DBC[1,]) ))
		scale = as.vector(Cl(data$DBC[1,])) / as.vector(Cl(CRB[(index + 1),]))
		temp = CRB[1 : (index + 1),] * repmat(scale, index + 1, 6)		
	data$DBC = rbind( temp[1:index,], data$DBC )
		
	bt.prep(data, align='remove.na', dates='1970::2011')

	#*****************************************************************
	# Code Strategies
	#****************************************************************** 
	prices = data$prices 	
	n = len(tickers)  
	
	# ignore cash when selecting funds
	position.score = prices
		position.score$SHY = NA
	
	# find month ends
	month.ends = date.month.ends(index(prices))
		
	# Equal Weight
	data$weight[] = NA
		data$weight[month.ends,] = ntop(position.score[month.ends,], n)	
		capital = 100000
		data$weight[] = (capital / prices) * data$weight
	equal.weight = bt.run(data, type='share', capital=capital)
		
	# BuyRule, price > 10 month SMA
	sma = bt.apply.matrix(prices, SMA, 200)
	buy.rule = prices > sma
		buy.rule = ifna(buy.rule, F)
	
	# Strategy
	weight = ntop(position.score[month.ends,], n)	
		# keep in cash the rest of the funds
		weight[!buy.rule[month.ends,]] = 0
		weight$SHY = 1 - rowSums(weight)

	data$weight[] = NA		
		data$weight[month.ends,] = weight		
		capital = 100000
		data$weight[] = (capital / prices) * data$weight
	timing = bt.run(data, type='share', trade.summary=T, capital=capital)
	
	#*****************************************************************
	# Code Strategies : Daily
	#****************************************************************** 
	weight = ntop(position.score, n)	
		# keep in cash the rest of the funds
		weight[!buy.rule] = 0
		weight$SHY = 1 - rowSums(weight)

	data$weight[] = NA
		data$weight[] = weight		
		capital = 100000
		data$weight[] = (capital / prices) * data$weight
	timing.d = bt.run(data, type='share', trade.summary=T, capital=capital)	
	
	#*****************************************************************
	# Create Report
	#****************************************************************** 
			
	# put all reports into one pdf file
	pdf(file = 'report.pdf', width=8.5, height=11)
		plotbt.custom.report(timing, timing.d, equal.weight, trade.summary=T)
	dev.off()
	
	#*****************************************************************
	# Code Strategies : Daily with Counter-Trend Entries by david varadi
	# see bt.improving.trend.following.test
	#****************************************************************** 
	dv = bt.apply(data, function(x) { DV(HLC(x), 1, TRUE) } )	
	
	data$weight[] = NA
		data$weight[] = iif(prices > sma & dv < 0.25, 0.2, data$weight)
		data$weight[] = iif(prices < sma & dv > 0.75, 0, data$weight)
		data$weight$SHY = 0
		
		
		data$weight = bt.apply.matrix(data$weight, ifna.prev)
		data$weight$SHY = 1 - rowSums(data$weight)
		
		data$weight = bt.exrem(data$weight)

		capital = 100000
		data$weight[] = (capital / prices) * data$weight
	timing.d1 = bt.run(data, type='share', trade.summary=T, capital=capital)

	# compute turnover	
	models = variable.number.arguments(timing.d1, timing.d, timing, equal.weight)
		sapply(models, compute.turnover, data)
	
	
	#*****************************************************************
	# Create Report
	#****************************************************************** 
	
	plotbt.custom.report.part1(timing.d1, timing.d, timing, equal.weight)

}

###############################################################################
# Monthly End-of-the-Month (MEOM) by Quanting Dutchman
# http://quantingdutchman.wordpress.com/2010/06/30/strategy-2-monthly-end-of-the-month-meom/
###############################################################################
bt.meom.test <- function() 
{	
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load.packages('quantmod')	
	tickers = spl('DIA,EEM,EFA,EWH,EWJ,EWT,EWZ,FXI,GLD,GSG,IEF,ILF,IWM,IYR,QQQ,SPY,VNQ,XLB,XLE,XLF,XLI,XLP,XLU,XLV,XLY,XLK')	
	
	# Alternatively use Dow Jones Components
	# tickers = dow.jones.components()
	
	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '1995-01-01', env = data, auto.assign = T)
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)	
	bt.prep(data, align='keep.all', dates='1995::')
	
	#*****************************************************************
	# Code Strategies
	#****************************************************************** 
	prices = data$prices   
	n = ncol(prices)
	nperiods = nrow(prices)

	# Equal Weight
	data$weight[] = ntop(prices, n)
	equal.weight = bt.run(data)	
	
		
	# find month ends
	month.ends = endpoints(prices, 'months')
		month.ends = month.ends[month.ends > 0]		
	month.ends2 = iif(month.ends + 2 > nperiods, nperiods, month.ends + 2)
								   
	# Strategy MEOM - Equal Weight
	data$weight[] = NA
		data$weight[month.ends,] = ntop(prices, n)[month.ends,]	
		data$weight[month.ends2,] = 0
		
		capital = 100000
		data$weight[] = (capital / prices) * data$weight
	meom.equal.weight = bt.run(data, type='share', capital=capital)

	#*****************************************************************
	# Rank1 = MA( C/Ref(C,-2), 5 ) * MA( C/Ref(C,-2), 40 )
	#****************************************************************** 
			
	# BuyRule = C > WMA(C, 89)
	buy.rule = prices > bt.apply.matrix(prices, function(x) { WMA(x, 89) } )		
		buy.rule = ifna(buy.rule, F)
		
	# 2-day returns
	ret2 = ifna(prices / mlag(prices, 2), 0)
	
	# Rank1 = MA( C/Ref(C,-2), 5 ) * MA( C/Ref(C,-2), 40 )
	position.score = bt.apply.matrix(ret2, SMA, 5) * bt.apply.matrix(ret2, SMA, 40)
		position.score[!buy.rule] = NA
			
	# Strategy MEOM - top 2    
	data$weight[] = NA;
		data$weight[month.ends,] = ntop(position.score[month.ends,], 2)		
		data$weight[month.ends2,] = 0		
		
		capital = 100000
		data$weight[] = (capital / prices) * data$weight
	meom.top2.rank1 = bt.run(data, type='share', trade.summary=T, capital=capital)
	
	#*****************************************************************
	# Rank2 = MA( C/Ref(C,-2), 5 ) * Ref( MA( C/Ref(C,-2), 10 ), -5 )
	#****************************************************************** 	
	
	# Rank2 = MA( C/Ref(C,-2), 5 ) * Ref( MA( C/Ref(C,-2), 10 ), -5 )
	position.score = bt.apply.matrix(ret2, SMA, 5) * mlag( bt.apply.matrix(ret2, SMA, 10), 5)
		position.score[!buy.rule] = NA
	
	# Strategy MEOM - top 2    
	data$weight[] = NA
		data$weight[month.ends,] = ntop(position.score[month.ends,], 2)		
		data$weight[month.ends2,] = 0		
		
		capital = 100000
		data$weight[] = (capital / prices) * data$weight
	meom.top2.rank2 = bt.run(data, type='share', trade.summary=T, capital=capital)
	
	#*****************************************************************
	# Create Report
	#****************************************************************** 
				
	# put all reports into one pdf file
	pdf(file = 'report.pdf', width=8.5, height=11)
		plotbt.custom.report(meom.top2.rank2, meom.top2.rank1, meom.equal.weight, equal.weight, trade.summary=T)
	dev.off()	

	
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')										
	plotbt.custom.report.part1(meom.top2.rank2, meom.top2.rank1, meom.equal.weight, equal.weight, trade.summary=T)
dev.off()	

png(filename = 'plot2.png', width = 1200, height = 800, units = 'px', pointsize = 12, bg = 'white')	
	plotbt.custom.report.part2(meom.top2.rank2, meom.top2.rank1, meom.equal.weight, equal.weight, trade.summary=T)
dev.off()	

png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')	
	plotbt.custom.report.part3(meom.top2.rank2, meom.top2.rank1, meom.equal.weight, equal.weight, trade.summary=T)
dev.off()	

	#*****************************************************************
	# Modify MEOM logic -  maybe sell in 1 day
	#****************************************************************** 
	
	month.ends1 = iif(month.ends + 1 > nperiods, nperiods, month.ends + 1)
		
	# Strategy MEOM - top 2, maybe sell in 1 day
	data$weight[] = NA
		data$weight[month.ends,] = ntop(position.score[month.ends,], 2)		
		data$weight[month.ends2,] = 0		

		# Close next day if Today's Close > Today's Open
		popen = bt.apply(data, Op)
		data$weight[month.ends1,] = iif((prices > popen)[month.ends1,], 0, NA)		
				
		capital = 100000
		data$weight[] = (capital / prices) * data$weight
	meom.top2.rank2.hold12 = bt.run(data, type='share', trade.summary=T, capital=capital)

png(filename = 'plot4.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')										
	plotbt.custom.report.part1(meom.top2.rank2.hold12, meom.top2.rank2, meom.top2.rank1, meom.equal.weight, equal.weight, trade.summary=T)
dev.off()	

png(filename = 'plot5.png', width = 1200, height = 800, units = 'px', pointsize = 12, bg = 'white')	
	plotbt.custom.report.part2(meom.top2.rank2.hold12, meom.top2.rank2, meom.top2.rank1, meom.equal.weight, equal.weight, trade.summary=T)
dev.off()	
	
			
}


###############################################################################
# Intraday Backtest
# The FX intraday free data was 
# http://www.fxhistoricaldata.com/EURUSD/
###############################################################################
bt.intraday.test <- function() 
{
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load.packages('quantmod')	

	EURUSD = getSymbols.fxhistoricaldata('EURUSD', 'hour', auto.assign = F, download=F)
	SPY = getSymbols('SPY', src = 'yahoo', from = '1980-01-01', auto.assign = F)
	
	
	#*****************************************************************
	# Reference intraday period
	#****************************************************************** 
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')											
	plota(EURUSD['2012:03:06 10::2012:03:06 21'], type='candle', main='EURUSD on 2012:03:06 from 10 to 21')
dev.off()			
	
	#*****************************************************************
	# Plot hourly and daily prices on the same chart
	#****************************************************************** 	
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')										
		
	# two Y axis plot	
	dates= '2012:01:01::2012:01:11'
	y = SPY[dates]
	plota(y, type = 'candle', LeftMargin=3)
			
	y = EURUSD[dates]
	plota2Y(y, ylim = range(OHLC(y), na.rm=T), las=1, col='red', col.axis = 'red')
		plota.ohlc(y, col=plota.candle.col(y))
	plota.legend('SPY(rhs),EURUSD(lhs)', 'black,red', list(SPY[dates],EURUSD[dates]))

dev.off()	
	

	#*****************************************************************
	# Universe: Currency Majors
	# http://en.wikipedia.org/wiki/Currency_pair	
	#****************************************************************** 
	tickers = spl('EURUSD,USDJPY,GBPUSD,AUDUSD,USDCHF,USDCAD')
	
	#*****************************************************************
	# Daily Backtest
	#****************************************************************** 
	data <- new.env()
	getSymbols.fxhistoricaldata(tickers, 'day', data, download=F)
	bt.prep(data, align='remove.na', dates='1990::')
	
	prices = data$prices   
	n = len(tickers)  
	models = list()
	
	# Equal Weight
	data$weight[] = NA
		data$weight[] = ntop(prices, n)
	models$equal.weight = bt.run.share(data, clean.signal=F)
	
	# Timing by M. Faber
	sma = bt.apply.matrix(prices, SMA, 200)
	data$weight[] = NA
		data$weight[] = ntop(prices, n) * (prices > sma)
	models$timing = bt.run.share(data, clean.signal=F)
	
	# Report
	models = rev(models)
png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')										
	plotbt.custom.report.part1(models)
dev.off()	
png(filename = 'plot4.png', width = 1200, height = 800, units = 'px', pointsize = 12, bg = 'white')	
	plotbt.custom.report.part2(models)
dev.off()	
	
	#*****************************************************************
	# Intraday Backtest
	#****************************************************************** 
	data <- new.env()	
	getSymbols.fxhistoricaldata(tickers, 'hour', data, download=F)	
	bt.prep(data, align='remove.na', dates='1990::')
	
	prices = data$prices   
	n = len(tickers)  
	models = list()
	
	# Equal Weight
	data$weight[] = NA
		data$weight[] = ntop(prices, n)
	models$equal.weight = bt.run.share(data, clean.signal=F)
	
	# Timing by M. Faber
	sma = bt.apply.matrix(prices, SMA, 200)
	data$weight[] = NA
		data$weight[] = ntop(prices, n) * (prices > sma)
	models$timing = bt.run.share(data, clean.signal=F)
	
	# Report
	models = rev(models)
png(filename = 'plot5.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')										
	plotbt.custom.report.part1(models)
dev.off()	
png(filename = 'plot6.png', width = 1200, height = 800, units = 'px', pointsize = 12, bg = 'white')	
	plotbt.custom.report.part2(models)
dev.off()	

}	
	
	
			



###############################################################################
# Forecast-Free Algorithms: A New Benchmark For Tactical Strategies
# Rebalancing was done on a weekly basis and quarterly data was used to estimate correlations.
# http://cssanalytics.wordpress.com/2011/08/09/forecast-free-algorithms-a-new-benchmark-for-tactical-strategies/
#
# Minimum Variance Sector Rotation
# http://quantivity.wordpress.com/2011/04/20/minimum-variance-sector-rotation/
#
# The volatility mystery continues
# http://www.portfolioprobe.com/2011/12/05/the-volatility-mystery-continues/
###############################################################################
bt.min.var.test <- function() 
{
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load.packages('quantmod,quadprog,lpSolve')
	tickers = spl('SPY,QQQ,EEM,IWM,EFA,TLT,IYR,GLD')

		
	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
		
	data.weekly <- new.env()
		for(i in tickers) data.weekly[[i]] = to.weekly(data[[i]], indexAt='endof')
					
	bt.prep(data, align='remove.na', dates='1990::')
	bt.prep(data.weekly, align='remove.na', dates='1990::')

	#*****************************************************************
	# Code Strategies
	#****************************************************************** 
	prices = data$prices   
	n = ncol(prices)
	
	# find week ends
	week.ends = endpoints(prices, 'weeks')
		week.ends = week.ends[week.ends > 0]		

		
	# Equal Weight 1/N Benchmark
	data$weight[] = NA
		data$weight[week.ends,] = ntop(prices[week.ends,], n)		
		
		capital = 100000
		data$weight[] = (capital / prices) * data$weight
	equal.weight = bt.run(data, type='share', capital=capital)
		
	#*****************************************************************
	# Create Constraints
	#*****************************************************************
	constraints = new.constraints(n, lb = -Inf, ub = +Inf)
	#constraints = new.constraints(n, lb = 0, ub = 1)
	
	# SUM x.i = 1
	constraints = add.constraints(rep(1, n), 1, type = '=', constraints)		

		
	ret = prices / mlag(prices) - 1
	weight = coredata(prices)
		weight[] = NA
		
	for( i in week.ends[week.ends >= (63 + 1)] ) {
		# one quarter = 63 days
		hist = ret[ (i- 63 +1):i, ]
		
		# create historical input assumptions
		ia = create.historical.ia(hist, 252)
			s0 = apply(coredata(hist),2,sd)		
			ia$cov = cor(coredata(hist), use='complete.obs',method='pearson') * (s0 %*% t(s0))
			
		weight[i,] = min.risk.portfolio(ia, constraints)
	}

	# Minimum Variance
	data$weight[] = weight		
		capital = 100000
		data$weight[] = (capital / prices) * data$weight
	min.var.daily = bt.run(data, type='share', capital=capital)

	#*****************************************************************
	# Code Strategies: Weekly
	#****************************************************************** 
	
	retw = data.weekly$prices / mlag(data.weekly$prices) - 1
	weightw = coredata(prices)
		weightw[] = NA
	
	for( i in week.ends[week.ends >= (63 + 1)] ) {	
		# map
		j = which(index(ret[i,]) == index(retw))
		
		# one quarter = 13 weeks
		hist = retw[ (j- 13 +1):j, ]
		
		# create historical input assumptions
		ia = create.historical.ia(hist, 52)
			s0 = apply(coredata(hist),2,sd)		
			ia$cov = cor(coredata(hist), use='complete.obs',method='pearson') * (s0 %*% t(s0))

		weightw[i,] = min.risk.portfolio(ia, constraints)
	}	
		
	data$weight[] = weightw		
		capital = 100000
		data$weight[] = (capital / prices) * data$weight
	min.var.weekly = bt.run(data, type='share', capital=capital, trade.summary = T)
	#min.var.weekly$trade.summary$trades
	#*****************************************************************
	# Create Report
	#****************************************************************** 
	
	
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')										
	plotbt.custom.report.part1(min.var.weekly, min.var.daily, equal.weight)
dev.off()	

png(filename = 'plot2.png', width = 1200, height = 800, units = 'px', pointsize = 12, bg = 'white')	
	plotbt.custom.report.part2(min.var.weekly, min.var.daily, equal.weight)
dev.off()	

png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')										
	layout(1:2)
	plotbt.transition.map(min.var.daily$weight)
		legend('topright', legend = 'min.var.daily', bty = 'n')
	plotbt.transition.map(min.var.weekly$weight)
		legend('topright', legend = 'min.var.weekly', bty = 'n')
dev.off()	

}


###############################################################################
# Backtest various asset allocation strategies based on the idea
# Forecast-Free Algorithms: A New Benchmark For Tactical Strategies
# http://cssanalytics.wordpress.com/2011/08/09/forecast-free-algorithms-a-new-benchmark-for-tactical-strategies/
#
# Extension to http://systematicinvestor.wordpress.com/2011/12/13/backtesting-minimum-variance-portfolios/
###############################################################################
bt.aa.test <- function() 
{
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load.packages('quantmod,quadprog,corpcor,lpSolve')
	tickers = spl('SPY,QQQ,EEM,IWM,EFA,TLT,IYR,GLD')
	#tickers = dow.jones.components()


	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)							
	bt.prep(data, align='remove.na', dates='1990::2011')
 
	
	#*****************************************************************
	# Code Strategies
	#****************************************************************** 
	prices = data$prices   
	n = ncol(prices)
	
	# find week ends
	period.ends = endpoints(prices, 'weeks')
			period.annual.factor = 52

#	period.ends = endpoints(prices, 'months')
#			period.annual.factor = 12

		period.ends = period.ends[period.ends > 0]

	#*****************************************************************
	# Create Constraints
	#*****************************************************************
	constraints = new.constraints(n, lb = 0, ub = 1)
	
	# SUM x.i = 1
	constraints = add.constraints(rep(1, n), 1, type = '=', constraints)		
	
	#*****************************************************************
	# Code Strategies
	#****************************************************************** 		
	ret = prices / mlag(prices) - 1
	start.i = which(period.ends >= (63 + 1))[1]

	#min.risk.fns = spl('min.risk.portfolio,min.maxloss.portfolio,min.mad.portfolio,min.cvar.portfolio,min.cdar.portfolio,min.cor.insteadof.cov.portfolio,min.mad.downside.portfolio,min.risk.downside.portfolio,min.avgcor.portfolio,find.erc.portfolio,min.gini.portfolio')	
	min.risk.fns = spl('min.risk.portfolio,min.maxloss.portfolio')
	
	# Gini risk measure optimization takes a while, uncomment below to add Gini risk measure
	# min.risk.fns = c(min.risk.fns, 'min.gini.portfolio')
	
	weight = NA * prices[period.ends,]
	weights = list()
		# Equal Weight 1/N Benchmark
		weights$equal.weight = weight
			weights$equal.weight[] = ntop(prices[period.ends,], n)	
			weights$equal.weight[1:start.i,] = NA
			
		for(f in min.risk.fns) weights[[ gsub('\\.portfolio', '', f) ]] = weight
		
	risk.contributions = list()	
		for(f in names(weights)) risk.contributions[[ f ]] = weight
			
	# construct portfolios			
	for( j in start.i:len(period.ends) ) {
		i = period.ends[j]
		
		# one quarter = 63 days
		hist = ret[ (i- 63 +1):i, ]
		
		include.index = rep(TRUE, n)
# new logic, require all assets to have full price history
#include.index = count(hist)== 63       
#hist = hist[ , include.index]

		
		# create historical input assumptions
		ia = create.historical.ia(hist, 252)
			s0 = apply(coredata(hist),2,sd)		
			ia$correlation = cor(coredata(hist), use='complete.obs',method='pearson')
			ia$cov = ia$correlation * (s0 %*% t(s0))
		
		# find optimal portfolios under different risk measures
		for(f in min.risk.fns) {
			# set up initial solution
			constraints$x0 = weights[[ gsub('\\.portfolio', '', f) ]][(j-1), include.index]
		
			weights[[ gsub('\\.portfolio', '', f) ]][j, include.index] = match.fun(f)(ia, constraints)
		}
		
		
		# compute risk contributions implied by portfolio weihgts
		for(f in names(weights)) {
			risk.contributions[[ f ]][j, include.index] = portfolio.risk.contribution(weights[[ f ]][j, include.index], ia)
		}

		if( j %% 10 == 0) cat(j, '\n')
	}
	
	#*****************************************************************
	# Create strategies
	#****************************************************************** 		
	models = list()
	for(i in names(weights)) {
		data$weight[] = NA
			data$weight[period.ends,] = weights[[i]]	
		models[[i]] = bt.run.share(data, clean.signal = F)
	}
		
	#*****************************************************************
	# Create Report
	#****************************************************************** 
	models = rev(models)
		weights = rev(weights)
		risk.contributions = rev(risk.contributions)

png(filename = 'plot1.png', width = 800, height = 600, units = 'px', pointsize = 12, bg = 'white')										
	# Plot perfromance
	plotbt(models, plotX = T, log = 'y', LeftMargin = 3)	    	
		mtext('Cumulative Performance', side = 2, line = 1)
dev.off()	

png(filename = 'plot2.png', width = 1200, height = 800, units = 'px', pointsize = 12, bg = 'white')		
	# Plot Strategy Statistics  Side by Side
	plotbt.strategy.sidebyside(models)
dev.off()	

	
png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')		
	# Plot Portfolio Turnover for each strategy
	layout(1)
	barplot.with.labels(sapply(models, compute.turnover, data), 'Average Annual Portfolio Turnover')
dev.off()	

	

png(filename = 'plot4.png', width = 600, height = 1600, units = 'px', pointsize = 12, bg = 'white')	
	# Plot transition maps
	layout(1:len(models))
	for(m in names(models)) {
		plotbt.transition.map(models[[m]]$weight, name=m)
			legend('topright', legend = m, bty = 'n')
	}
dev.off()	

png(filename = 'plot5.png', width = 600, height = 1600, units = 'px', pointsize = 12, bg = 'white')	
	# Plot risk contributions
	layout(1:len(risk.contributions))
	for(m in names(risk.contributions)) {
		plotbt.transition.map(risk.contributions[[m]], name=paste('Risk Contributions',m))
			legend('topright', legend = m, bty = 'n')
	}
dev.off()	

	
png(filename = 'plot6.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')		
	# Plot portfolio concentration stats
	layout(1:2)	
	plota.matplot(lapply(weights, portfolio.concentration.gini.coefficient), main='Gini Coefficient')
	plota.matplot(lapply(weights, portfolio.concentration.herfindahl.index), main='Herfindahl Index')
	#plota.matplot(lapply(weights, portfolio.turnover), main='Turnover')
dev.off()	


png(filename = 'plot7.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')		
	# Compute stats
	out = compute.stats(weights,
		list(Gini=function(w) mean(portfolio.concentration.gini.coefficient(w), na.rm=T),
			Herfindahl=function(w) mean(portfolio.concentration.herfindahl.index(w), na.rm=T),
			Turnover=function(w) period.annual.factor * mean(portfolio.turnover(w), na.rm=T)
			)
		)
	
	out[] = plota.format(100 * out, 1, '', '%')
	plot.table(t(out))
dev.off()		


png(filename = 'plot8.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')		
	# Plot Portfolio Turnover for each strategy
	layout(1)
	barplot.with.labels(sapply(weights, function(w) period.annual.factor * mean(portfolio.turnover(w), na.rm=T)), 'Average Annual Portfolio Turnover')
dev.off()	

}


###############################################################################
# Investigate Rebalancing methods:
# 1. Periodic Rebalancing: rebalance to the target mix every month, quarter, year.
# 2. Maximum Deviation Rebalancing: rebalance to the target mix when asset weights deviate more than a given percentage from the target mix.
# 3. Same as 2, but rebalance half-way to target
###############################################################################


# helper function to create barplot with labels
barplot.with.labels <- function(data, main, plotX = TRUE, label=c('level','name','both')) {
	par(mar=c( iif(plotX, 6, 2), 4, 2, 2))
	x = barplot(100 * data, main = main, las = 2, names.arg = iif(plotX, names(data), ''))
	
	if(label[1] == 'level') text(x, 100 * data, round(100 * data,1), adj=c(0.5,1), xpd = TRUE)
	if(label[1] == 'name') text(x, 0 * data, names(data), adj=c(-0.1,1), srt=90, xpd = TRUE)	
	if(label[1] == 'both') 
		text(x, 0 * data, paste(round(100 * data), '% ', names(data), sep=''), adj=c(-0.1,1), srt=90, xpd = TRUE)
}


bt.rebalancing.test <- function() 
{
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load.packages('quantmod')
	tickers = spl('SPY,TLT')
	
	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '1900-01-01', env = data, auto.assign = T)
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
	bt.prep(data, align='remove.na', dates='1900::2011')
	
	#*****************************************************************
	# Code Strategies
	#****************************************************************** 
	prices = data$prices   
	nperiods = nrow(prices)
	target.allocation = matrix(c(0.5, 0.5), nrow=1)
	
	# Buy & Hold	
	data$weight[] = NA	
		data$weight[1,] = target.allocation
		capital = 100000
		data$weight[] = (capital / prices) * data$weight
	buy.hold = bt.run(data, type='share', capital=capital)

	
	# Rebalance periodically
	models = list()
	for(period in spl('months,quarters,years')) {
		data$weight[] = NA	
			data$weight[1,] = target.allocation
			
			period.ends = endpoints(prices, period)
				period.ends = period.ends[period.ends > 0]		
			data$weight[period.ends,] = repmat(target.allocation, len(period.ends), 1)
						
			capital = 100000
			data$weight[] = (capital / prices) * data$weight
		models[[period]] = bt.run(data, type='share', capital=capital)	
	}
	models$buy.hold = buy.hold				
	
	# Compute Portfolio Turnover 
	compute.turnover(models$years, data)		
	
	# Compute Portfolio Maximum Deviation
	compute.max.deviation(models$years, target.allocation)		
		
	#*****************************************************************
	# Create Report
	#****************************************************************** 				
	# put all reports into one pdf file
	pdf(file = 'report.pdf', width=8.5, height=11)
		plotbt.custom.report(models)
	dev.off()	
	
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')										
	plotbt.custom.report.part1(models)
dev.off()	
	
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
	# Plot BuyHold and Monthly Rebalancing Weights
	layout(1:2)
	plotbt.transition.map(models$buy.hold$weight, 'buy.hold', spl('red,orange'))
		abline(h=50)
	plotbt.transition.map(models$months$weight, 'months', spl('red,orange'))
		abline(h=50)		
dev.off()

png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')		
	# Plot Portfolio Turnover for each Rebalancing method
	layout(1)
	barplot.with.labels(sapply(models, compute.turnover, data), 'Average Annual Portfolio Turnover')
dev.off()	

	#*****************************************************************
	# Code Strategies that rebalance based on maximum deviation
	#****************************************************************** 
	
	# rebalance to target.allocation when portfolio weights are 5% away from target.allocation
	models$smart5.all = bt.max.deviation.rebalancing(data, buy.hold, target.allocation, 5/100, 0) 
	
	# rebalance half-way to target.allocation when portfolio weights are 5% away from target.allocation
	models$smart5.half = bt.max.deviation.rebalancing(data, buy.hold, target.allocation, 5/100, 0.5) 
		
	#*****************************************************************
	# Create Report
	#****************************************************************** 			
			
png(filename = 'plot4.png', width = 600, height = 800, units = 'px', pointsize = 12, bg = 'white')
	# Plot BuyHold, Years and Max Deviation Rebalancing Weights	
	layout(1:4)
	plotbt.transition.map(models$buy.hold$weight, 'buy.hold', spl('red,orange'))
		abline(h=50)
	plotbt.transition.map(models$smart5.all$weight, 'Max Deviation 5%, All the way', spl('red,orange'))
		abline(h=50)
	plotbt.transition.map(models$smart5.half$weight, 'Max Deviation 5%, Half the way', spl('red,orange'))
		abline(h=50)
	plotbt.transition.map(models$years$weight, 'years', spl('red,orange'))
		abline(h=50)
dev.off()	

png(filename = 'plot5.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')		
	# Plot Portfolio Turnover for each Rebalancing method
	layout(1:2)
	barplot.with.labels(sapply(models, compute.turnover, data), 'Average Annual Portfolio Turnover', F)
	barplot.with.labels(sapply(models, compute.max.deviation, target.allocation), 'Maximum Deviation from Target Mix')
dev.off()

png(filename = 'plot6.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')		
	# Plot Strategy Statistics  Side by Side
	plotbt.strategy.sidebyside(models)
dev.off()	
	
	#*****************************************************************
	# Periodic Rebalancing Seasonality
	#****************************************************************** 			
	# maQuant annual rebalancing (september/october showed the best results)	
	months = spl('Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec')		
	period.ends = endpoints(prices, 'months')
		period.ends = period.ends[period.ends > 0]		
	models = list()
	for(i in 1:12) {
		index = which( date.month(index(prices)[period.ends]) == i )
		data$weight[] = NA	
			data$weight[1,] = target.allocation			
			data$weight[period.ends[index],] = repmat(target.allocation, len(index), 1)
						
			capital = 100000
			data$weight[] = (capital / prices) * data$weight
		models[[ months[i] ]] = bt.run(data, type='share', capital=capital)	
	}

png(filename = 'plot7.png', width = 1200, height = 500, units = 'px', pointsize = 12, bg = 'white')		
	plotbt.strategy.sidebyside(models)
dev.off()	
		
	layout(1)
	barplot.with.labels(sapply(models, compute.turnover, data), 'Average Annual Portfolio Turnover')
}


# Rebalancing method based on maximum deviation
bt.max.deviation.rebalancing <- function
(
	data,
	model, 
	target.allocation, 
	max.deviation = 3/100, 
	rebalancing.ratio = 0,	# 0 means rebalance all-way to target.allocation
							# 0.5 means rebalance half-way to target.allocation
	start.index = 1,
	period.ends = 1:nrow(model$weight)							
) 
{
	nperiods = nrow(model$weight)
	action.index = rep(F, nperiods)
	
	start.index = period.ends[start.index]
	start.index0 = start.index
	
	while(T) {	
		# find rows that violate max.deviation
		weight = model$weight
		index = apply(abs(weight - repRow(target.allocation, nperiods)), 1, max) > max.deviation
		index = which( index[period.ends] )
	
		if( len(index) > 0 ) {
			index = period.ends[index]
			index = index[ index > start.index ]
		
			if( len(index) > 0 ) {
				action.index[index[1]] = T
				
				data$weight[] = NA	
					data$weight[start.index0,] = target.allocation
					
					temp = repRow(target.allocation, sum(action.index))
					data$weight[action.index,] = temp + 
						rebalancing.ratio * (weight[action.index,] - temp)					
					
				model = bt.run.share(data, clean.signal=F, silent=T)
				
				start.index = index[1]
			} else break			
		} else break		
	}
	return(model)
}




	



bt.rebalancing1.test <- function() 
{
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load.packages('quantmod')
	# SHY - cash
	tickers = spl('SPY,TLT,GLD,FXE,USO,SHY')
	
	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '1900-01-01', env = data, auto.assign = T)
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
	bt.prep(data, align='remove.na', dates='1900::2011')
	
	#*****************************************************************
	# Code Strategies
	#****************************************************************** 
	prices = data$prices   
	nperiods = nrow(prices)
	target.allocation = matrix(rep(1/6,6), nrow=1)
	
	# Buy & Hold	
	data$weight[] = NA	
		data$weight[1,] = target.allocation
		capital = 100000
		data$weight[] = (capital / prices) * data$weight
	buy.hold = bt.run(data, type='share', capital=capital)

	
	# Rebalance periodically
	models = list()
	for(period in spl('months,quarters,years')) {
		data$weight[] = NA	
			data$weight[1,] = target.allocation
			
			period.ends = endpoints(prices, period)
				period.ends = period.ends[period.ends > 0]		
			data$weight[period.ends,] = repmat(target.allocation, len(period.ends), 1)
						
			capital = 100000
			data$weight[] = (capital / prices) * data$weight
		models[[period]] = bt.run(data, type='share', capital=capital)	
	}
	models$buy.hold = buy.hold				
	

	#*****************************************************************
	# Code Strategies that rebalance based on maximum deviation
	#****************************************************************** 
	
	# rebalance to target.allocation when portfolio weights are 3% away from target.allocation
	models$smart3.all = bt.max.deviation.rebalancing(data, buy.hold, target.allocation, 3/100, 0) 
	
	# rebalance half-way to target.allocation when portfolio weights are 3% away from target.allocation
	models$smart3.half = bt.max.deviation.rebalancing(data, buy.hold, target.allocation, 3/100, 0.5) 
		
	#*****************************************************************
	# Create Report
	#****************************************************************** 			
			
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')		

	# Plot Portfolio Turnover for each Rebalancing method
	layout(1:2)
	barplot.with.labels(sapply(models, compute.turnover, data), 'Average Annual Portfolio Turnover', F)
	barplot.with.labels(sapply(models, compute.max.deviation, target.allocation), 'Maximum Deviation from Target Mix')
	
	
dev.off()

png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')		
	# Plot Strategy Statistics  Side by Side
	plotbt.strategy.sidebyside(models)
dev.off()	
	
}


###############################################################################
# Rotational Trading: how to reduce trades and improve returns by Frank Hassler
# http://engineering-returns.com/2011/07/06/rotational-trading-how-to-reducing-trades-and-improve-returns/
###############################################################################
# Custom Summary function to replicate tables from Engineering Returns
engineering.returns.kpi <- function
(
	bt,		# backtest object
	trade.summary = NULL
) 
{	
	if( !is.null(bt$trade.summary) ) trade.summary = bt$trade.summary
	
	out = list()
	out$Period = join( format( range(index(bt$equity)), '%b%Y'), ' - ')
		
	out$Cagr = compute.cagr(bt$equity)
	out$DVR = compute.DVR(bt)
	out$Sharpe = compute.sharpe(bt$ret)		
	out$R2 = compute.R2(bt$equity)
		
	if( !is.null(trade.summary) ) {
		out$Win.Percent = trade.summary$stats['win.prob', 'All']
		out$Avg.Trade = trade.summary$stats['avg.pnl', 'All']
	}
		
	out$MaxDD = compute.max.drawdown(bt$equity)

	# format
	out = lapply(out, function(x) if(is.double(x)) round(100*x,1) else x)
				
	if( !is.null(trade.summary) ) out$Num.Trades = trade.summary$stats['ntrades', 'All']			
			
	return( list(System=out))
}


bt.rotational.trading.trades.test <- function() 
{
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load.packages('quantmod')	
	tickers = spl('XLY,XLP,XLE,XLF,XLV,XLI,XLB,XLK,XLU,IWB,IWD,IWF,IWM,IWN,IWO,IWP,IWR,IWS,IWV,IWW,IWZ')

	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)		
	bt.prep(data, align='remove.na', dates='1970::2011')

	#*****************************************************************
	# Code Strategies : weekly rebalancing
	#****************************************************************** 
	prices = data$prices  
	n = len(tickers)  

	# find week ends
	week.ends = endpoints(prices, 'weeks')
		week.ends = week.ends[week.ends > 0]		

		
	# Rank on ROC 200
	position.score = prices / mlag(prices, 200)	
		position.score.ma = position.score		
		buy.rule = T

	# Select Top 2 funds daily
	data$weight[] = NA
		data$weight[] = ntop(position.score, 2)	
		capital = 100000
		data$weight[] = (capital / prices) * bt.exrem(data$weight)				
	top2.d = bt.run(data, type='share', trade.summary=T, capital=capital)

	# Select Top 2 funds weekly
	data$weight[] = NA
		data$weight[week.ends,] = ntop(position.score[week.ends,], 2)	
		capital = 100000
		data$weight[] = (capital / prices) * bt.exrem(data$weight)		
	top2.w = bt.run(data, type='share', trade.summary=T, capital=capital)
	
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')		
	# Plot Strategy Metrics Side by Side
	plotbt.strategy.sidebyside(top2.d, top2.w, perfromance.fn = 'engineering.returns.kpi')	
dev.off()
	
	#*****************************************************************
	# Code Strategies : different entry/exit rank
	#****************************************************************** 
	
	# Select Top 2 funds, Keep till they are in 4/6 rank
	data$weight[] = NA
		data$weight[] = ntop.keep(position.score, 2, 4)	
		capital = 100000
		data$weight[] = (capital / prices) * bt.exrem(data$weight)		
	top2.d.keep4 = bt.run(data, type='share', trade.summary=T, capital=capital)
	
	data$weight[] = NA
		data$weight[] = ntop.keep(position.score, 2, 6)	
		capital = 100000
		data$weight[] = (capital / prices) * bt.exrem(data$weight)		
	top2.d.keep6 = bt.run(data, type='share', trade.summary=T, capital=capital)

png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')				
	# Plot Strategy Metrics Side by Side
	plotbt.strategy.sidebyside(top2.d, top2.d.keep4, top2.d.keep6, perfromance.fn = 'engineering.returns.kpi')
dev.off()

	#*****************************************************************
	# Code Strategies : Rank smoothing
	#****************************************************************** 

	models = list()
	models$Bench = top2.d
	for( avg in spl('SMA,EMA') ) {
		for( i in c(3,5,10,20) ) {		
			position.score.smooth = bt.apply.matrix(position.score.ma, avg, i)	
				position.score.smooth[!buy.rule,] = NA
			
			data$weight[] = NA
				data$weight[] = ntop(position.score.smooth, 2)	
				capital = 100000
				data$weight[] = (capital / prices) * bt.exrem(data$weight)		
			models[[ paste(avg,i) ]] = bt.run(data, type='share', trade.summary=T, capital=capital)		
		}
	}
		
png(filename = 'plot3.png', width = 1200, height = 600, units = 'px', pointsize = 12, bg = 'white')				
	# Plot Strategy Metrics Side by Side
	plotbt.strategy.sidebyside(models, perfromance.fn = 'engineering.returns.kpi')
dev.off()
	
	#*****************************************************************
	# Code Strategies : Combination
	#****************************************************************** 

	# Select Top 2 funds daily, Keep till they are 6 rank, Smooth Rank by 10 day EMA
	position.score.smooth = bt.apply.matrix(position.score.ma, 'EMA', 10)	
		position.score.smooth[!buy.rule,] = NA
	data$weight[] = NA
		data$weight[] = ntop.keep(position.score.smooth, 2, 6)	
		capital = 100000
		data$weight[] = (capital / prices) * bt.exrem(data$weight)		
	top2.d.keep6.EMA10 = bt.run(data, type='share', trade.summary=T, capital=capital)
		
	# Select Top 2 funds weekly, Keep till they are 6 rank
	data$weight[] = NA
		data$weight[week.ends,] = ntop.keep(position.score[week.ends,], 2, 6)	
		capital = 100000
		data$weight[] = (capital / prices) * bt.exrem(data$weight)		
	top2.w.keep6 = bt.run(data, type='share', trade.summary=T, capital=capital)
	
	# Select Top 2 funds weekly, Keep till they are 6 rank, Smooth Rank by 10 week EMA
	position.score.smooth[] = NA
		position.score.smooth[week.ends,] = bt.apply.matrix(position.score.ma[week.ends,], 'EMA', 10)	
			position.score.smooth[!buy.rule,] = NA
	
	data$weight[] = NA
		data$weight[week.ends,] = ntop.keep(position.score.smooth[week.ends,], 2, 6)	
		capital = 100000
		data$weight[] = (capital / prices) * bt.exrem(data$weight)		
	top2.w.keep6.EMA10 = bt.run(data, type='share', trade.summary=T, capital=capital)
	
		
png(filename = 'plot4.png', width = 800, height = 600, units = 'px', pointsize = 12, bg = 'white')				
	# Plot Strategy Metrics Side by Side
	plotbt.strategy.sidebyside(top2.d, top2.d.keep6, top2.d.keep6.EMA10, top2.w, top2.w.keep6, top2.w.keep6.EMA10, perfromance.fn = 'engineering.returns.kpi')
dev.off()
	





	#*****************************************************************
	# Possible Improvements to reduce drawdowns
	#****************************************************************** 
	# Equal Weight
	data$weight[] = ntop(prices, n)
	ew = bt.run(data)	

	# Avoiding severe draw downs
	# http://engineering-returns.com/2010/07/26/rotational-trading-system/
	# Only trade the system when the index is either above the 200 MA or 30 MA
	# Usually these severe draw downs  happen bellow the 200MA average and 
	# the second 30 MA average will help to get in when the recovery happens	
	buy.rule = (ew$equity > SMA(ew$equity,200)) | (ew$equity > SMA(ew$equity,30))
	buy.rule = (ew$equity > SMA(ew$equity,200))
		buy.rule = ifna(buy.rule, F)
		    	
	# Rank using TSI by Frank Hassler, TSI is already smoothed and slow varying, 
	# so SMA will filter will not very effective
	#http://engineering-returns.com/tsi/
	position.score = bt.apply(data, function(x) TSI(HLC(x)) )		
		position.score.ma = position.score
		position.score[!buy.rule,] = NA
		
}



###############################################################################
# Charting the Santa Claus Rally
# http://ibankcoin.com/woodshedderblog/2011/12/15/charting-the-santa-claus-rally/
#
# Trading Calendar
# http://www.cxoadvisory.com/trading-calendar/
###############################################################################
bt.december.trading.test <- function() 
{
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load.packages('quantmod')	
	tickers = spl('SPY')

	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)		
	bt.prep(data, align='remove.na', dates='1970::2011')

	#*****************************************************************
	# Code Strategies
	#****************************************************************** 
	prices = data$prices  
	n = len(tickers)  
	ret = prices / mlag(prices) - 1

	
	# find prices in December
	dates = index(prices)
	years = date.year(dates)	
	index = which(date.month(dates) == 12)
	
	# rearrange data in trading days
	trading.days = sapply(tapply(ret[index,], years[index], function(x) coredata(x)), function(x) x[1:22])
		
	# average return each trading days, excluding current year
	avg.trading.days = apply(trading.days[, -ncol(trading.days)], 1, mean, na.rm=T)
	current.year = trading.days[, ncol(trading.days)]
	
	# cumulative
	avg.trading.days = 100 * ( cumprod(1 + avg.trading.days) - 1 )
	current.year = 100 * ( cumprod(1 + current.year) - 1 )
	
	#*****************************************************************
	# Create Plot
	#****************************************************************** 	
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')	
	# plot	
	par(mar=c(4,4,1,1))
	plot(avg.trading.days, type='b', col=1,
		ylim=range(avg.trading.days,current.year,na.rm=T),
		xlab = 'Number of Trading Days in December',
		ylab = 'Avg % Profit/Loss'
		)
		lines(current.year, type='b', col=2)
	grid()
	plota.legend('Avg SPY,SPY Dec 2011', 1:2)
dev.off()	
		
	#*****************************************************************
	# Code Strategies
	#****************************************************************** 	
	# Buy & Hold	
	data$weight[] = 1
		capital = 100000
		data$weight[] = (capital / prices) * data$weight
	buy.hold = bt.run(data, type='share', capital=capital)

	
	# Find Last trading days in November and December
	index = which(date.month(dates) == 11)
	last.day.november = match(tapply(dates[index], years[index], function(x) tail(x,1)), dates)
	index = which(date.month(dates) == 12)
	last.day.december = match(tapply(dates[index], years[index], function(x) tail(x,1)), dates)
	
	# December
	data$weight[] = NA	
		data$weight[last.day.november,] = 1
		data$weight[last.day.december,] = 0
		capital = 100000
		data$weight[] = (capital / prices) * data$weight
	december = bt.run(data, type='share', capital=capital, trade.summary=T)
	
	#*****************************************************************
	# Create Report
	#****************************************************************** 
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
	plotbt.custom.report.part1(december, buy.hold, trade.summary=T)	
dev.off()	

png(filename = 'plot3.png', width = 1200, height = 800, units = 'px', pointsize = 12, bg = 'white')	
	plotbt.custom.report.part2(december, buy.hold, trade.summary=T)	
dev.off()	
	
}


###############################################################################
# Seasonality Case Study
# Historical Seasonality Analysis: What company in DOW is likely to do well in January? 
###############################################################################
bt.seasonality.test <- function() 
{	
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load.packages('quantmod')		
	tickers = dow.jones.components()
	
	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)	
	bt.prep(data, align='keep.all', dates='1970::2011')
		
	#*****************************************************************
	# Compute monthly returns
	#****************************************************************** 
	prices = data$prices   
	n = ncol(prices)	
	
	# find month ends
	month.ends = endpoints(prices, 'months')
	
	prices = prices[month.ends,]
	ret = prices / mlag(prices) - 1

	# keep only January	
	ret = ret[date.month(index(ret)) == 1, ]
	
	# keep last 20 years
	ret = last(ret,20)

	#*****************************************************************
	# Compute stats
	#****************************************************************** 
	stats = matrix(rep(NA,2*n), nc=n)
		colnames(stats) = colnames(prices)
		rownames(stats) = spl('N,Positive')
		
	for(i in 1:n) {
		stats['N',i] = sum(!is.na(ret[,i]))
		stats['Positive',i] = sum(ret[,i]>0, na.rm=T)	
	}
	sort(stats['Positive',], decreasing =T)
	
png(filename = 'plot1.png', width = 600, height = 200, units = 'px', pointsize = 12, bg = 'white')										
	plot.table(stats[, order(stats['Positive',], decreasing =T)[1:10]])
dev.off()	
	
	
	
}


###############################################################################
# Volatility Forecasting using Garch(1,1) based
#
# Regime Switching System Using Volatility Forecast by Quantum Financier
# http://quantumfinancier.wordpress.com/2010/08/27/regime-switching-system-using-volatility-forecast/
###############################################################################
# Benchmarking Garch algorithms 
# garch from tseries package is faster than garchFit from fGarch package
###############################################################################
bt.test.garch.speed <- function() 
{
	load.packages('tseries,fGarch,rbenchmark')	

	temp = garchSim(n=252)

	test1 <- function() {
		fit1=garch(temp, order = c(1, 1), control = garch.control(trace = F))
	}
	test2 <- function() {
		fit2=garchFit(~ garch(1,1), data = temp, include.mean=FALSE, trace=F)
	}
		 	
	benchmark(
		test1(),
		test2(),
		columns=spl('test,replications,elapsed,relative'),
		order='relative',
		replications=100
	)
}

###############################################################################
# One day ahead forecast functions for garch (tseries) and garchFit(fGarch)
# Sigma[t]^2 = w + a* Sigma[t-1]^2 + b*r[t-1]^2
# r.last - last return, h.last - last volatility
###############################################################################
garch.predict.one.day <- function(fit, r.last) 
{
	h.last = tail( fitted(fit)[,1] ,1)			
	sqrt(sum( coef(fit) * c(1,  r.last^2, h.last^2) ))	
}

# same as predict( fit, n.ahead=1, doplot=F)[3]
garchFit.predict.one.day <- function(fit, r.last) 
{
	h.last = tail(sqrt(fit@h.t), 1)
	sqrt(sum( fit@fit$matcoef[,1] * c(1,  r.last^2, h.last^2) ))
}
	
###############################################################################
# Forecast Volatility using Garch
# garch from tseries is fast, but does not consistently converge
# garchFit from fGarch is slower, but converges consistently
###############################################################################
bt.forecast.garch.volatility <- function(ret.log, est.period = 252) 
{		
	nperiods = nrow(ret.log)		
	garch.vol = NA * ret.log
	
	for( i in (est.period + 1) : nperiods ) {
		temp = as.vector(ret.log[ (i - est.period + 1) : i, ])
		r.last =  tail( temp, 1 )
		
		fit = tryCatch( garch(temp, order = c(1, 1), control = garch.control(trace = F)),
	    				error=function( err ) FALSE, warning=function( warn ) FALSE )
	                    
		if( !is.logical( fit ) ) {
			if( i == est.period + 1 ) garch.vol[1:est.period] = fitted(fit)[,1]
			garch.vol[i] = garch.predict.one.day(fit, r.last)
		} else {
			fit = tryCatch( garchFit(~ garch(1,1), data = temp, include.mean=FALSE, trace=F),
	    				error=function( err ) FALSE, warning=function( warn ) FALSE )
	    				
			if( !is.logical( fit ) ) {
				if( i == est.period + 1 ) garch.vol[1:est.period] = sqrt(fit@h.t)
				garch.vol[i] = garchFit.predict.one.day(fit, r.last)
			} 
		}			
		if( i %% 100 == 0) cat(i, '\n')
	}
	garch.vol[] = ifna.prev(coredata(garch.vol))
	return(garch.vol)
}	

###############################################################################
# Volatility Forecasting using Garch(1,1) based
###############################################################################
bt.volatility.garch <- function() 
{
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load.packages('quantmod')	
	tickers = 'SPY'

	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)		
	bt.prep(data, align='remove.na', dates='2000::2012')

	#*****************************************************************
	# Code Strategies
	#****************************************************************** 
	prices = data$prices  
	n = len(tickers)  
	nperiods = nrow(prices)
	
	# Buy & Hold	
	data$weight[] = 1
	buy.hold = bt.run(data)	

		
	# Mean-Reversion(MR) strategy - RSI2
	rsi2 = bt.apply.matrix(prices, RSI, 2)
	data$weight[] = NA
		data$weight[] = iif(rsi2 < 50, 1, -1)	
		capital = 100000
		data$weight[] = (capital / prices) * bt.exrem(data$weight)
	mr = bt.run(data, type='share', capital=capital, trade.summary=T)
				
		
	# Trend Following(TF) strategy - MA 50/200 crossover
	sma.short = bt.apply.matrix(prices, SMA, 50)
	sma.long = bt.apply.matrix(prices, SMA, 200)
	data$weight[] = NA
		data$weight[] = iif(sma.short > sma.long, 1, -1)
		capital = 100000
		data$weight[] = (capital / prices) * bt.exrem(data$weight)
	tf = bt.run(data, type='share', capital=capital, trade.summary=T)
		
	#*****************************************************************
	# Create Report
	#****************************************************************** 
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')										
	
	plotbt.custom.report.part1(mr, tf, buy.hold, trade.summary=T)
	
dev.off()		
	#*****************************************************************
	# Regime Switching  Historical
	#****************************************************************** 
	#classify current volatility by percentile using a 252 day lookback period
	#The resulting series oscillate between 0 and 1, and is smoothed using a 21 day percentrankSMA (developed by David Varadi) using a 252 day lookback period.
	#percentrank(MA(percentrank(Stdev( diff(log(close)) ,21),252),21),250)

	ret.log = bt.apply.matrix(prices, ROC, type='continuous')
	hist.vol = bt.apply.matrix(ret.log, runSD, n = 21)
	vol.rank = percent.rank(SMA(percent.rank(hist.vol, 252), 21), 250)

	# Regime Switching  Historical
	data$weight[] = NA
		data$weight[] = iif(vol.rank > 0.5, 
							iif(rsi2 < 50, 1, -1),
							iif(sma.short > sma.long, 1, -1)
						)
		capital = 100000
		data$weight[] = (capital / prices) * bt.exrem(data$weight)
	regime.switching = bt.run(data, type='share', capital=capital, trade.summary=T)
	
	#*****************************************************************
	# Create Report
	#****************************************************************** 
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')										
		
	plotbt.custom.report.part1(regime.switching, mr, tf, buy.hold, trade.summary=T)
	
dev.off()		

	#*****************************************************************
	# Regime Switching using Garch
	#****************************************************************** 		
	load.packages('tseries,fGarch')	
	garch.vol = bt.forecast.garch.volatility(ret.log, 252)	
	vol.rank = percent.rank(SMA(percent.rank(garch.vol, 252), 21), 250)

	# Regime Switching Garch
	data$weight[] = NA
		data$weight[] = iif(vol.rank > 0.5, 
							iif(rsi2 < 50, 1, -1),
							iif(sma.short > sma.long, 1, -1)
						)
		capital = 100000
		data$weight[] = (capital / prices) * bt.exrem(data$weight)
	regime.switching.garch = bt.run(data, type='share', capital=capital, trade.summary=T)
	
	#*****************************************************************
	# Create Report
	#****************************************************************** 
png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')										
	
	plotbt.custom.report.part1(regime.switching.garch, regime.switching, buy.hold, trade.summary=T)
	
dev.off()	
}





###############################################################################
# Time Series Matching
#
# Based on Jean-Robert Avettand-Fenoel - How to Accelerate Model Deployment using Rook
# http://www.londonr.org/Sep%2011%20LondonR_AvettandJR.pdf
###############################################################################
bt.matching.test <- function() 
{
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load.packages('quantmod')	
	tickers = 'SPY'

	data = getSymbols(tickers, src = 'yahoo', from = '1950-01-01', auto.assign = F)

	#*****************************************************************
	# New: logic moved to functions
	#****************************************************************** 
	
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
	obj = bt.matching.find(Cl(data), plot=T)
dev.off()

png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')	
	matches = bt.matching.overlay(obj, plot=T)
dev.off()

png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')		
	bt.matching.overlay.table(obj, matches, plot=T)
dev.off()	
	

	#*****************************************************************
	# Original logic: Setup search
	#****************************************************************** 
	data = last(data, 252*10)
	reference = coredata(Cl(data))
		n = len(reference)
		query = reference[(n-90+1):n]	
		reference = reference[1:(n-90)]
		
		n.query = len(query)
		n.reference = len(reference)

	#*****************************************************************
	# Compute Distance
	#****************************************************************** 		
	dist = rep(NA, n.reference)
	query.normalized = (query - mean(query)) / sd(query)
	
	for( i in n.query : n.reference ) {
		window = reference[ (i - n.query + 1) : i]
		window.normalized = (window - mean(window)) / sd(window)
		dist[i] = stats:::dist(rbind(query.normalized, window.normalized))
	}

	#*****************************************************************
	# Find Matches
	#****************************************************************** 			
	min.index = c()
	n.match = 10
	
	# only look at the minimums 
	temp = dist
		temp[ temp > mean(dist, na.rm=T) ] = NA
		
	# remove n.query, points to the left/right of the minimums
	for(i in 1:n.match) {
		if(any(!is.na(temp))) {
			index = which.min(temp)
			min.index[i] = index
			temp[max(0,index - 2*n.query) : min(n.reference,(index + n.query))] = NA
		}
	}
	n.match = len(min.index)
		
	#*****************************************************************
	# Plot Matches
	#****************************************************************** 		
	dates = index(data)[1:len(dist)]
	
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
	par(mar=c(2, 4, 2, 2))
	plot(dates, dist, type='l',col='gray', main='Top Matches', ylab='Euclidean Distance', xlab='')
		abline(h = mean(dist, na.rm=T), col='darkgray', lwd=2)
		points(dates[min.index], dist[min.index], pch=22, col='red', bg='red')
		text(dates[min.index], dist[min.index], 1:n.match, adj=c(1,1), col='black',xpd=TRUE)
dev.off()			
		
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
	plota(data, type='l', col='gray', main=tickers)
		plota.lines(last(data,90), col='blue')
		for(i in 1:n.match) {
		plota.lines(data[(min.index[i]-n.query + 1):min.index[i]], col='red')
		}
		text(index4xts(data)[min.index - n.query/2], reference[min.index - n.query/2], 1:n.match, 
			adj=c(1,-1), col='black',xpd=TRUE)
		plota.legend('Pattern,Match Number','blue,red')
dev.off()			
	
	#*****************************************************************
	# Overlay all Matches
	#****************************************************************** 		
	matches = matrix(NA, nr=(n.match+1), nc=3*n.query)
	temp = c(rep(NA, n.query), reference, query)
	for(i in 1:n.match) {
		matches[i,] = temp[ (min.index[i] - n.query + 1):(min.index[i] + 2*n.query) ]	
	}
	#reference[min.index] == matches[,(2*n.query)]
	
	matches[(n.match+1),] = temp[ (len(temp) - 2*n.query + 1):(len(temp) + n.query) ]		
	#matches[(n.match+1), (n.query+1):(2*n.query)] == query
	
	for(i in 1:(n.match+1)) {
		matches[i,] = matches[i,] / matches[i,n.query]
	}
		

	#*****************************************************************
	# Plot all Matches
	#****************************************************************** 				
	temp = 100 * ( t(matches[,-c(1:n.query)]) - 1)
	
png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')	
	par(mar=c(2, 4, 2, 2))
	matplot(temp, type='l',col='gray',lwd=2, lty='dotted', xlim=c(1,2.5*n.query),
		main = paste('Pattern Prediction with', n.match, 'neighbours'),ylab='Normalized', xlab='')
		lines(temp[,(n.match+1)], col='black',lwd=4)
	
	points(rep(2*n.query,n.match), temp[2*n.query,1:n.match], pch=21, lwd=2, col='gray', bg='gray')
				
	bt.plot.dot.label <- function(x, data, xfun, col='red') {
		for(j in 1:len(xfun)) {
			y = match.fun(xfun[[j]])(data)
			points(x, y, pch=21, lwd=4, col=col, bg=col)
			text(x, y, paste(names(xfun)[j], ':', round(y,1),'%'),
				adj=c(-0.1,0), cex = 0.8, col=col,xpd=TRUE)			
		}
	}
	
	bt.plot.dot.label(2*n.query, temp[2*n.query,1:n.match], 
		list(Min=min,Max=max,Median=median,'Bot 25%'=function(x) quantile(x,0.25),'Top 75%'=function(x) quantile(x,0.75)))
	bt.plot.dot.label(n.query, temp[n.query,(n.match+1)], list(Current=min))
dev.off()	
	
	#*****************************************************************
	# Table with predictions
	#****************************************************************** 		
	temp = matrix( double(), nr=(n.match+4), 6)
		rownames(temp) = c(1:n.match, spl('Current,Min,Average,Max'))
		colnames(temp) = spl('Start,End,Return,Week,Month,Quarter')
		
	# compute returns
	temp[1:(n.match+1),'Return'] = matches[,2*n.query]/ matches[,n.query]
	temp[1:(n.match+1),'Week'] = matches[,(2*n.query+5)]/ matches[,2*n.query]
	temp[1:(n.match+1),'Month'] = matches[,(2*n.query+20)]/ matches[,2*n.query]
	temp[1:(n.match+1),'Quarter'] = matches[,(2*n.query+60)]/ matches[,2*n.query]
			
	# compute average returns
	index = spl('Return,Week,Month,Quarter')
	temp['Min', index] = apply(temp[1:(n.match+1),index],2,min,na.rm=T)
	temp['Average', index] = apply(temp[1:(n.match+1),index],2,mean,na.rm=T)
	temp['Max', index] = apply(temp[1:(n.match+1),index],2,max,na.rm=T)
	
	# format
	temp[] = plota.format(100*(temp-1),1,'','%')
		
	# enter dates
	temp['Current', 'Start'] = format(index(last(data,90)[1]), '%d %b %Y')
	temp['Current', 'End'] = format(index(last(data,1)[1]), '%d %b %Y')
	for(i in 1:n.match) {
		temp[i, 'Start'] = format(index(data[min.index[i] - n.query + 1]), '%d %b %Y')
		temp[i, 'End'] = format(index(data[min.index[i]]), '%d %b %Y')	
	}
		
	# plot table
png(filename = 'plot4.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')	
	plot.table(temp, smain='Match Number')
dev.off()		
	
}	

###############################################################################
# Time Series Matching Backtest
#
# New weighting scheme : seight each match by its distance
###############################################################################
bt.matching.backtest.test <- function() 
{
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load.packages('quantmod')	
	tickers = spl('SPY,^GSPC')

	data <- new.env()
	quantmod:::getSymbols(tickers, src = 'yahoo', from = '1950-01-01', env = data, auto.assign = T)
	bt.prep(data, align='keep.all')

	# compare common part [ SPY and ^GSPC match only if not adjusted for dividends]
	#temp = data$prices['1993:01:29::']
	#plot(temp[,1]/as.double(temp[1,1]) - temp[,2]/as.double(temp[1,2]), main='Diff between SPY and ^GSPC')

	# combine SPY and ^GSPC
	scale = as.double( data$prices$SPY['1993:01:29'] / data$prices$GSPC['1993:01:29'] )
	hist = c(scale * data$prices$GSPC['::1993:01:28'], data$prices$SPY['1993:01:29::'])

	#*****************************************************************
	# Backtest setup:
	# Starting January 1994, each month search for the 10 best matches 
	# similar to the last 90 days in the last 10 years of history data
	#
	# Invest next month if distance weighted prediction is positive
	# otherwise stay in cash
	#****************************************************************** 
	# find month ends
	month.ends = endpoints(hist, 'months')
		month.ends = month.ends[month.ends > 0]		
	
	start.index = which(date.year(index(hist[month.ends])) == 1994)[1]
	weight = hist * NA
	
	for( i in start.index : len(month.ends) ) {
		#obj = bt.matching.find(hist[1:month.ends[i],], n.match=10, normalize.fn = normalize.mean, plot=T)
		#matches = bt.matching.overlay(obj, future=hist[(month.ends[i]+1):(month.ends[i]+22),], plot=T)
		#bt.matching.overlay.table(obj, matches, weights=NA, plot=T)

		obj = bt.matching.find(hist[1:month.ends[i],], normalize.fn = normalize.first)
		matches = bt.matching.overlay(obj)
		
		# compute prediction for next month
		n.match = len(obj$min.index)
		n.query = len(obj$query)				
		month.ahead.forecast = matches[,(2*n.query+22)]/ matches[,2*n.query] - 1
		
		# Average, mean(month.ahead.forecast[1:n.match]) 
		weights = rep(1/n.match, n.match)
		avg.direction = weighted.mean(month.ahead.forecast[1:n.match], w=weights)
				
		# Distance weighted average
		temp = round(100*(obj$dist / obj$dist[1] - 1))		
			n.weight = max(temp) + 1
			weights = (n.weight - temp) / ( n.weight * (n.weight+1) / 2)
		weights = weights / sum(weights)
			# barplot(weights)
		avg.direction = weighted.mean(month.ahead.forecast[1:n.match], w=weights)
		
		# Logic
		weight[month.ends[i]] = 0
		if( avg.direction > 0 ) weight[month.ends[i]] = 1
		
		# print progress		
		if( i %% 10 == 0) cat(i, '\n')
	}
		
	#*****************************************************************
	# Code Strategies
	#****************************************************************** 
	tickers = 'SPY'
	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '1950-01-01', env = data, auto.assign = T)
	bt.prep(data, align='keep.all')
	
	prices = data$prices  
	
	# Buy & Hold	
	data$weight[] = 1
	buy.hold = bt.run(data)	

	# Strategy
	data$weight[] = NA
		data$weight[] = weight['1993:01:29::']
		capital = 100000
		data$weight[] = (capital / prices) * bt.exrem(data$weight)
	test = bt.run(data, type='share', capital=capital, trade.summary=T)
		
	#*****************************************************************
	# Create Report
	#****************************************************************** 
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')										
	
	plotbt.custom.report.part1(test, buy.hold, trade.summary=T)
	
dev.off()		
	
	
	
}


	
###############################################################################
# Time Series Matching helper functions
###############################################################################
# functions to normalize data
###############################################################################
normalize.mean <- function(x) { x - mean(x) }
normalize.mean.sd <- function(x) { (x - mean(x)) / sd(x) }
normalize.first <- function(x) { x/as.double(x[1]) }

###############################################################################
# functions to compute distance
###############################################################################
dist.euclidean <- function(x) { stats:::dist(x) }

###############################################################################
# Find historical matches similar to the given query(pattern)
###############################################################################
bt.matching.find <- function
(
	data,				# time series
	n.query=90, 		# length of pattern i.e. last 90 days
	n.reference=252*10, # length of history to look for pattern
	n.match=10, 		# number of matches to find
	normalize.fn = normalize.mean.sd, 	# function to normalize data
	dist.fn = dist.euclidean,	# function to compute distance
	plot=FALSE,			# flag to create plot
	plot.dist=FALSE,	# flag to create distance plot	
	layout = NULL,		# flag to idicate if layout is already set	
	main = NULL			# plot title
)
{
	#*****************************************************************
	# Setup search
	#****************************************************************** 
	data = last(data, n.reference)
	reference = coredata(data)
		n = len(reference)
		query = reference[(n - n.query + 1):n]	
		reference = reference[1:(n - n.query)]
		
		main = paste(main, join(format(range(index(data)[(n - n.query + 1):n]), '%d%b%Y'), ' - '))
			
		n.query = len(query)
		n.reference = len(reference)

		dist.fn.name = ''
		if(is.character(dist.fn)) {
			dist.fn.name = paste('with',dist.fn)
			dist.fn = get(dist.fn)					
		}
		
	#*****************************************************************
	# Compute Distance
	#****************************************************************** 		
	dist = rep(NA, n.reference)
	query.normalized = match.fun(normalize.fn)(query)	
	
	for( i in n.query : n.reference ) {
		window = reference[ (i - n.query + 1) : i]
		window.normalized = match.fun(normalize.fn)(window)	
		dist[i] = match.fun(dist.fn)(rbind(query.normalized, window.normalized))
		
		# print progress		
		if( i %% 100 == 0) cat(i, '\n')
	}

	#*****************************************************************
	# Find Matches
	#****************************************************************** 			
	min.index = c()
	
	# only look at the minimums 
	temp = dist
		temp[ temp > mean(dist, na.rm=T) ] = NA
		
	# remove n.query, points to the left/right of the minimums
	for(i in 1:n.match) {
		if(any(!is.na(temp))) {
			index = which.min(temp)
			min.index[i] = index
			temp[max(0,index - 2*n.query) : min(n.reference,(index + n.query))] = NA
		}
	}
	n.match = len(min.index)

	
	#*****************************************************************
	# Plot Matches
	#****************************************************************** 		
	if(plot) {	
		dates = index(data)[1:len(dist)]
	
		if(is.null(layout)) {
			if(plot.dist) layout(1:2) else layout(1)		
		}
		par(mar=c(2, 4, 2, 2))
		
		if(plot.dist) {
		plot(dates, dist, type='l',col='gray', main=paste('Top Historical Matches for', main, dist.fn.name), ylab='Distance', xlab='')
			abline(h = mean(dist, na.rm=T), col='darkgray', lwd=2)
			points(dates[min.index], dist[min.index], pch=22, col='red', bg='red')
			text(dates[min.index], dist[min.index], 1:n.match, adj=c(1,1), col='black',xpd=TRUE)
		}
		
		plota(data, type='l', col='gray', LeftMargin = 1,
			main=iif(!plot.dist, paste('Top Historical Matches for', main), NULL)
			)
			plota.lines(last(data,90), col='blue')
			for(i in 1:n.match) {
			plota.lines(data[(min.index[i]-n.query + 1):min.index[i]], col='red')
			}
			text(index4xts(data)[min.index - n.query/2], reference[min.index - n.query/2], 1:n.match, 
				adj=c(1,-1), col='black',xpd=TRUE)
			plota.legend(paste('Pattern: ', main, ',Match Number'),'blue,red')	
	}
	
	return(list(min.index=min.index, dist=dist[min.index], query=query, reference=reference, dates = index(data), main = main))
}
		

###############################################################################
# Create matrix that overlays all matches one on top of each other
###############################################################################
# helper function to plot dots and labels
###############################################################################
bt.plot.dot.label <- function(x, data, xfun, col='red') {
	for(j in 1:len(xfun)) {
		y = match.fun(xfun[[j]])(data)
		points(x, y, pch=21, lwd=4, col=col, bg=col)
		text(x, y, paste(names(xfun)[j], ':', round(y,1),'%'),
			adj=c(-0.1,0), cex = 0.8, col=col,xpd=TRUE)			
	}
}

bt.matching.overlay <- function
(
	obj, 				# object from bt.matching.find function
	future=NA,			# time series of future, only used for plotting
 	plot=FALSE,			# flag to create plot
 	plot.index=NA,		# range of data to plot
	layout = NULL		# flag to idicate if layout is already set	
)
{
	min.index = obj$min.index
	query = obj$query
	reference = obj$reference
	
	n.match = len(min.index)
	n.query = len(query)
	n.reference = len(reference)

	#*****************************************************************
	# Overlay all Matches
	#****************************************************************** 		
	matches = matrix(NA, nr=(n.match+1), nc=3*n.query)
	temp = c(rep(NA, n.query), reference, query, future)
	for(i in 1:n.match) {
		matches[i,] = temp[ (min.index[i] - n.query + 1):(min.index[i] + 2*n.query) ]	
	}
	#reference[min.index] == matches[,(2*n.query)]
	
	matches[(n.match+1),] = temp[ (n.reference + 1):(n.reference + 3*n.query) ]		
	#matches[(n.match+1), (n.query+1):(2*n.query)] == query
	
	for(i in 1:(n.match+1)) {
		matches[i,] = matches[i,] / iif(!is.na(matches[i,n.query]), matches[i,n.query], matches[i,(n.query+1)])
	}
	
	#*****************************************************************
	# Plot all Matches
	#****************************************************************** 				
	if(plot) {		
		temp = 100 * ( t(matches[,-c(1:n.query)]) - 1)
		if(!is.na(plot.index[1])) temp=temp[plot.index,]
		n = nrow(temp)
		
		if(is.null(layout)) layout(1)
		#par(mar=c(4, 2, 2, 2), ...)
		par(mar=c(4, 2, 2, 2))
		
		matplot(temp, type='n',col='gray',lwd=2, lty='dotted', xlim=c(1, n + 0.15*n),
			main = paste(obj$main,'Historical Pattern Prediction with', n.match, 'neighbours'),ylab='Normalized', xlab = 'Trading Days')
			
		col=adjustcolor('yellow', 0.5)
		rect(0, par('usr')[3],n.query, par('usr')[4], col=col, border=col)
		box()

		
		matlines(temp, col='gray',lwd=2, lty='dotted')
		lines(temp[,(n.match+1)], col='black',lwd=4)
		
		
			
		points(rep(n, n.match), temp[n, 1:n.match], pch=21, lwd=2, col='gray', bg='gray')
						
		bt.plot.dot.label(n, temp[n, 1:n.match], 
			list(Min=min,Max=max,Median=median,'Bot 25%'=function(x) quantile(x,0.25),'Top 75%'=function(x) quantile(x,0.75)))
		bt.plot.dot.label(n.query, temp[n.query,(n.match+1)], list(Current=min))	
	}
	
	return(matches)
}	


###############################################################################
# Create matches summary table
###############################################################################
bt.matching.overlay.table <- function
(
	obj, 				# object from bt.matching.find function
	matches, 			# matches from bt.matching.overlay function
	weights=NA, 		# weights to compute average
 	plot=FALSE,			# flag to create plot
	layout = NULL		# flag to idicate if layout is already set	
)
{
	min.index = obj$min.index
	query = obj$query
	reference = obj$reference
	dates = obj$dates
	
	n.match = len(min.index)
	n.query = len(query)
	n.reference = len(reference)
	
	if(is.na(weights)) weights = rep(1/n.match, n.match)

	#*****************************************************************
	# Table with predictions
	#****************************************************************** 		
	temp = matrix( double(), nr=(n.match + 4), 6)
		rownames(temp) = c(1:n.match, spl('Current,Min,Average,Max'))
		colnames(temp) = spl('Start,End,Return,Week,Month,Quarter')
		
	# compute returns
	temp[1:(n.match+1),'Return'] = matches[,2*n.query]/ matches[,n.query]
	temp[1:(n.match+1),'Week'] = matches[,(2*n.query+5)]/ matches[,2*n.query]
	temp[1:(n.match+1),'Month'] = matches[,(2*n.query+20)]/ matches[,2*n.query]
	temp[1:(n.match+1),'Quarter'] = matches[,(2*n.query+60)]/ matches[,2*n.query]
			
	# compute average returns
	index = spl('Return,Week,Month,Quarter')
	temp['Min', index] = apply(temp[1:(n.match+0),index],2,min,na.rm=T)
	#temp['Average', index] = apply(temp[1:(n.match+0),index],2,mean,na.rm=T)
	temp['Average', index] = apply(temp[1:(n.match+0),index],2,weighted.mean,w=weights,na.rm=T)
	temp['Max', index] = apply(temp[1:(n.match+0),index],2,max,na.rm=T)
	
	# format
	temp[] = plota.format(100*(temp-1),1,'','%')
		
	# enter dates
	temp['Current', 'Start'] = format(dates[(n.reference+1)], '%d %b %Y')
	temp['Current', 'End'] = format(dates[len(dates)], '%d %b %Y')
	for(i in 1:n.match) {
		temp[i, 'Start'] = format(dates[min.index[i] - n.query + 1], '%d %b %Y')
		temp[i, 'End'] = format(dates[min.index[i]], '%d %b %Y')	
	}
		
	# plot table
	if(plot) {			
		if(is.null(layout)) layout(1)
		plot.table(temp, smain='Match Number')	
	}
	
	return(temp)
}



###############################################################################
# Time Series Matching with Dynamic time warping
#
# Based on Jean-Robert Avettand-Fenoel - How to Accelerate Model Deployment using Rook
# http://www.londonr.org/Sep%2011%20LondonR_AvettandJR.pdf
###############################################################################
# functions to compute distance
###############################################################################
#dist.euclidean <- function(x) { stats:::dist(x) }
dist.MOdist <- function(x) { MOdist(t(x)) }
dist.DTW <- function(x) { dtw(x[1,], x[2,])$distance }


bt.matching.dtw.test <- function() 
{
	#*****************************************************************
	# Example of Dynamic time warping from dtw help
	#****************************************************************** 
	load.packages('dtw')
	
	# A noisy sine wave as query
	idx = seq(0,6.28,len=100)
	query = sin(idx)+runif(100)/10
	
	# A cosine is for reference; sin and cos are offset by 25 samples
	reference = cos(idx)
	
	# map one to one, typical distance
	alignment<-dtw(query, reference, keep=TRUE)
	alignment$index1 = 1:100
	alignment$index2 = 1:100

png(filename = 'plot0.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
	plot(alignment,main='Example of 1 to 1 mapping', type='two',off=3)
dev.off()

	# map one to many, dynamic time warping
	alignment<-dtw(query, reference, keep=TRUE)

png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
	plot(alignment,main='Example of 1 to many mapping (DTW)', type='two',off=3)
dev.off()
	
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load.packages('quantmod')	
	tickers = 'SPY'

	data = getSymbols(tickers, src = 'yahoo', from = '1950-01-01', auto.assign = F)	

	#*****************************************************************
	# Euclidean distance	
	#****************************************************************** 
	
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
	obj = bt.matching.find(Cl(data), normalize.fn = normalize.mean, dist.fn = 'dist.euclidean', plot=T)
dev.off()

png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
	matches = bt.matching.overlay(obj, plot.index=1:90, plot=T)
dev.off()

png(filename = 'plot4.png', width = 600, height = 800, units = 'px', pointsize = 12, bg = 'white')
	layout(1:2)
	matches = bt.matching.overlay(obj, plot=T, layout=T)
	bt.matching.overlay.table(obj, matches, plot=T, layout=T)
dev.off()

	#*****************************************************************
	# Dynamic time warping distance	
	#****************************************************************** 
	# http://en.wikipedia.org/wiki/Dynamic_time_warping
	# http://dtw.r-forge.r-project.org/
	#****************************************************************** 

png(filename = 'plot5.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
	obj = bt.matching.find(Cl(data), normalize.fn = normalize.mean, dist.fn = 'dist.DTW', plot=T)
dev.off()

png(filename = 'plot6.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
	matches = bt.matching.overlay(obj, plot.index=1:90, plot=T)
dev.off()


png(filename = 'plot7.png', width = 600, height = 800, units = 'px', pointsize = 12, bg = 'white')
	layout(1:2)
	matches = bt.matching.overlay(obj, plot=T, layout=T)
	bt.matching.overlay.table(obj, matches, plot=T, layout=T)
dev.off()

	#*****************************************************************
	# Dynamic time warping distance	
	#****************************************************************** 

png(filename = 'plot8.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
	obj = bt.matching.find(Cl(data), normalize.fn = normalize.mean, dist.fn = 'dist.DTW1', plot=T)
dev.off()

png(filename = 'plot9.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
	matches = bt.matching.overlay(obj, plot.index=1:90, plot=T)
dev.off()


png(filename = 'plot10.png', width = 600, height = 800, units = 'px', pointsize = 12, bg = 'white')
	layout(1:2)
	matches = bt.matching.overlay(obj, plot=T, layout=T)
	bt.matching.overlay.table(obj, matches, plot=T, layout=T)
dev.off()



	#*****************************************************************
	# Dynamic time warping distance	
	#****************************************************************** 

png(filename = 'plot11.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
	obj = bt.matching.find(Cl(data), normalize.fn = normalize.mean, dist.fn = 'dist.DDTW', plot=T)
dev.off()

png(filename = 'plot12.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
	matches = bt.matching.overlay(obj, plot.index=1:90, plot=T)
dev.off()


png(filename = 'plot13.png', width = 600, height = 800, units = 'px', pointsize = 12, bg = 'white')
	layout(1:2)
	matches = bt.matching.overlay(obj, plot=T, layout=T)
	bt.matching.overlay.table(obj, matches, plot=T, layout=T)
dev.off()



}		


###############################################################################      
# Derivative Dynamic Time Warping by Eamonn J. Keogh and Michael J. Pazzani
# http://www.cs.rutgers.edu/~mlittman/courses/statai03/DDTW-2001.pdf
# 
# page 3
# To align two sequences using DTW we construct an n-by-m matrix where the (ith, jth)
# element of the matrix contains the distance d(qi,cj) between the two points qi and cj
# (Typically the Euclidean distance is used, so d(qi,cj) = (qi - cj)2 ).
# 
# page 6
# With DDTW the distance measure d(qi,cj) is not Euclidean but rather the square of the 
# difference of the estimated derivatives of qi and cj. 
# This estimate is simply the average of the slope of the line through the point in 
# question and its left neighbor, and the slope of the line through the left neighbor and the
# right neighbor. Empirically this estimate is more robust to outliers than any estimate
# considering only two datapoints. Note the estimate is not defined for the first and last
# elements of the sequence. Instead we use the estimates of the second and next-to-last
# elements respectively.
###############################################################################
derivative.est <- function(x) {
	x = as.vector(x)
	n = len(x)
	d = (( x - mlag(x) ) + ( mlag(x,-1)- mlag(x) ) / 2) / 2
	d[1] = d[2]
	d[n] = d[(n-1)]
	d
}

dist.DDTW <- function(x) { 
	y = x
	x[1,] = derivative.est(x[1,])
	x[2,] = derivative.est(x[2,])
	
	alignment = dtw(x[1,], x[2,])
	stats:::dist(rbind(y[1,alignment$index1],y[2,alignment$index2]))
	#proxy::dist(y[1,alignment$index1],y[2,alignment$index2],method='Euclidean',by_rows=F)	
}	

dist.DTW1 <- function(x) { 
	alignment = dtw(x[1,], x[2,])
	stats:::dist(rbind(x[1,alignment$index1],x[2,alignment$index2]))
}


bt.ddtw.test <- function() 
{
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load.packages('quantmod')	
	tickers = 'SPY'

	data = getSymbols(tickers, src = 'yahoo', from = '1950-01-01', auto.assign = F)	

	#*****************************************************************
	# Setup
	#****************************************************************** 
	load.packages('dtw')
	
	query = as.vector(coredata(last(Cl(data['2011::2011']), 60)))
	reference = as.vector(coredata(last(Cl(data['2010::2010']), 60)))	
	
	#*****************************************************************
	# Dynamic Time Warping 	
	#****************************************************************** 
	alignment = dtw(query, reference, keep=TRUE)

png(filename = 'plot1.ddtw.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
	plot(alignment,main='DTW Alignment', type='two',off=20)
dev.off()
		
	
	#*****************************************************************
	# Derivative Dynamic Time Warping by Eamonn J. Keogh and Michael J. Pazzani
	# http://www.cs.rutgers.edu/~mlittman/courses/statai03/DDTW-2001.pdf
	#****************************************************************** 
	derivative.est <- function(x) {
		x = as.vector(x)
		n = len(x)
		d = (( x - mlag(x) ) + ( mlag(x,-1)- mlag(x) ) / 2) / 2
		d[1] = d[2]
		d[n] = d[(n-1)]
		d
	}
	
	alignment0 = dtw(derivative.est(query), derivative.est(reference), keep=TRUE)
	alignment$index1 = alignment0$index1
	alignment$index2 = alignment0$index2
		
png(filename = 'plot2.ddtw.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
	plot(alignment,main='Derivative DTW Alignment', type='two',off=20)
dev.off()
	
	
}
	




###############################################################################
# Position Sizing
#
# Money Management Position Sizing
# http://www.trading-plan.com/money_position_sizing.html
#
# Position Sizing is Everything
# http://www.leighdrogen.com/position-sizing-is-everything/
###############################################################################
bt.position.sizing.test <- function() 
{
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load.packages('quantmod')	
	tickers = spl('SPY')

	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)			
	bt.prep(data, align='keep.all', dates='1970::')	
	
	#*****************************************************************
	# Code Strategies
	#****************************************************************** 
	prices = data$prices   
	nperiods = nrow(prices)
	
	models = list()
	
	#*****************************************************************
	# Buy & Hold
	#****************************************************************** 
	data$weight[] = 0
		data$weight[] = 1
	models$buy.hold = bt.run.share(data, clean.signal=T)

	#*****************************************************************
	# Volatility Position Sizing - ATR
	#****************************************************************** 
	atr = bt.apply(data, function(x) ATR(HLC(x),20)[,'atr'])
		
	# http://www.leighdrogen.com/position-sizing-is-everything/	
	# position size in units = ((porfolio size * % of capital to risk)/(ATR*2)) 
	data$weight[] = NA
		capital = 100000
		
		# risk 2% of capital, assuming 2 atr stop
		data$weight[] = (capital * 2/100) / (2 * atr)
		
		# make sure you are not commiting more than 100%
		# http://www.trading-plan.com/money_position_sizing.html
		max.allocation = capital / prices
		data$weight[] = iif(data$weight > max.allocation, max.allocation,data$weight)
		
	models$buy.hold.2atr = bt.run(data, type='share', capital=capital)					

	#*****************************************************************
	# Create Report
	#****************************************************************** 	
	models = rev(models)
	
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')										
	plotbt.custom.report.part1(models)
dev.off()	


png(filename = 'plot2.png', width = 1200, height = 800, units = 'px', pointsize = 12, bg = 'white')	
	plotbt.custom.report.part2(models)
dev.off()	
		

}	



###############################################################################
# Trading Equity Curve with Volatility Position Sizing
###############################################################################
bt.volatility.position.sizing.test <- function() 
{
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load.packages('quantmod')	
	tickers = 'SPY'
	
	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)		
	bt.prep(data, align='keep.all', dates='1994::')
	
	#*****************************************************************
	# Buy and Hold
	#****************************************************************** 
	models = list()
	prices = data$prices
	
	data$weight[] = 1
	models$buy.hold = bt.run.share(data, clean.signal=T)

	#*****************************************************************
	# Buy and Hold with target 10% Volatility
	#****************************************************************** 
	ret.log = bt.apply.matrix(prices, ROC, type='continuous')
	hist.vol = sqrt(252) * bt.apply.matrix(ret.log, runSD, n = 60)

	data$weight[] = 0.1 / hist.vol
	models$buy.hold.volatility.weighted = bt.run.share(data, clean.signal=T)

	#*****************************************************************
	# Buy and Hold with target 10% Volatility and Max Total leverage 100%
	#****************************************************************** 		
	data$weight[] = 0.1 / hist.vol
		rs = rowSums(data$weight)
		data$weight[] = data$weight / iif(rs > 1, rs, 1) 			
	models$buy.hold.volatility.weighted.100 = bt.run.share(data, clean.signal=T)
		
	#*****************************************************************
	# Same, rebalanced Monthly
	#****************************************************************** 
	period.ends = endpoints(prices, 'months')
		period.ends = period.ends[period.ends > 0]
		
	data$weight[] = NA
	data$weight[period.ends,] = 0.1 / hist.vol[period.ends,]
		rs = rowSums(data$weight[period.ends,])
		data$weight[period.ends,] = data$weight[period.ends,] / iif(rs > 1, rs, 1) 			
	models$buy.hold.volatility.weighted.100.monthly = bt.run.share(data, clean.signal=T)
		
	#*****************************************************************
	# Create Report
	#****************************************************************** 
png(filename = 'plot1.png', width = 800, height = 600, units = 'px', pointsize = 12, bg = 'white')										
	# Plot performance
	plotbt(models, plotX = T, log = 'y', LeftMargin = 3)	    	
		mtext('Cumulative Performance', side = 2, line = 1)
dev.off()	

png(filename = 'plot2.png', width = 1600, height = 1000, units = 'px', pointsize = 12, bg = 'white')		
	plotbt.custom.report.part2(rev(models))
dev.off()	
	
png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')		
	# Plot Portfolio Turnover for each strategy
	layout(1)
	barplot.with.labels(sapply(models, compute.turnover, data), 'Average Annual Portfolio Turnover', plotX = F, label='both')
dev.off()	
	
	

	#*****************************************************************
	# Next let's examine other volatility measures
	#****************************************************************** 
	models = models[c('buy.hold' ,'buy.hold.volatility.weighted.100.monthly')]

		
	# TTR volatility calc types
	calc = c("close", "garman.klass", "parkinson", "rogers.satchell", "gk.yz", "yang.zhang")
	
	ohlc = OHLC(data$SPY)
	for(icalc in calc) {
		vol = volatility(ohlc, calc = icalc, n = 60, N = 252)
		
		data$weight[] = NA
		data$weight[period.ends,] = 0.1 / vol[period.ends,]
			rs = rowSums(data$weight[period.ends,])
			data$weight[period.ends,] = data$weight[period.ends,] / iif(rs > 1, rs, 1) 			
		models[[icalc]] = bt.run.share(data, clean.signal=T)
	}
	
	#*****************************************************************
	# Create Report
	#****************************************************************** 
png(filename = 'plot4.png', width = 800, height = 600, units = 'px', pointsize = 12, bg = 'white')										
	# Plot performance
	plotbt(models, plotX = T, log = 'y', LeftMargin = 3)	    	
		mtext('Cumulative Performance', side = 2, line = 1)
dev.off()	
	
png(filename = 'plot5.png', width = 1600, height = 600, units = 'px', pointsize = 12, bg = 'white')
	plotbt.strategy.sidebyside(models)
dev.off()	


	#*****************************************************************
	# Volatility Position Sizing applied to MA cross-over strategy's Equity Curve
	#****************************************************************** 
	models = list()	
	
	sma.fast = SMA(prices, 50)
	sma.slow = SMA(prices, 200)
	weight = iif(sma.fast >= sma.slow, 1, -1)
	
	data$weight[] = weight
	models$ma.crossover = bt.run.share(data, clean.signal=T)
		
	#*****************************************************************
	# Target 10% Volatility
	#****************************************************************** 
	ret.log = bt.apply.matrix(models$ma.crossover$equity, ROC, type='continuous')
	hist.vol = sqrt(252) * bt.apply.matrix(ret.log, runSD, n = 60)
		
	data$weight[] = NA
		data$weight[period.ends,] = (0.1 / hist.vol[period.ends,]) * weight[period.ends,]
		# limit total leverage to 100%		
		rs = rowSums(data$weight[period.ends,])
		data$weight[period.ends,] = data$weight[period.ends,] / iif(abs(rs) > 1, abs(rs), 1) 			
	models$ma.crossover.volatility.weighted.100.monthly = bt.run.share(data, clean.signal=T)
	
	#*****************************************************************
	# Create Report
	#****************************************************************** 
png(filename = 'plot6.png', width = 800, height = 600, units = 'px', pointsize = 12, bg = 'white')										
	# Plot performance
	plotbt(models, plotX = T, log = 'y', LeftMargin = 3)	    	
		mtext('Cumulative Performance', side = 2, line = 1)
dev.off()	
	
png(filename = 'plot7.png', width = 1200, height = 800, units = 'px', pointsize = 12, bg = 'white')		
	plotbt.custom.report.part2(rev(models))
dev.off()	
	
	
	#*****************************************************************
	# Apply Volatility Position Sizing Timing stretegy by M. Faber
	#****************************************************************** 
	tickers = spl('SPY,QQQ,EEM,IWM,EFA,TLT,IYR,GLD')
	
	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)		
	bt.prep(data, align='remove.na', dates='1994::')
	
	#*****************************************************************
	# Code Strategies
	#****************************************************************** 
	prices = data$prices   
		n = ncol(prices)
	models = list()

	period.ends = endpoints(prices, 'months')
		period.ends = period.ends[period.ends > 0]
		
	#*****************************************************************
	# Equal Weight
	#****************************************************************** 
	data$weight[] = NA
		data$weight[period.ends,] = ntop(prices[period.ends,], n)
		data$weight[1:200,] = NA
	models$equal.weight = bt.run.share(data, clean.signal=F)
				
	#*****************************************************************
	# Timing by M. Faber
	#****************************************************************** 
	sma = bt.apply.matrix(prices, SMA, 200)
	
	weight = ntop(prices, n) * (prices > sma)
	data$weight[] = NA
		data$weight[period.ends,] = weight[period.ends,]
	models$timing = bt.run.share(data, clean.signal=F)
		
	#*****************************************************************
	# Timing with target 10% Volatility
	#****************************************************************** 
	ret.log = bt.apply.matrix(models$timing$equity, ROC, type='continuous')
	hist.vol = bt.apply.matrix(ret.log, runSD, n = 60)
		hist.vol = sqrt(252) * as.vector(hist.vol)
	
	data$weight[] = NA
		data$weight[period.ends,] = (0.1 / hist.vol[period.ends]) * weight[period.ends,]
		rs = rowSums(data$weight)
		data$weight[] = data$weight / iif(rs > 1, rs, 1) 				
		data$weight[1:200,] = NA
	models$timing.volatility.weighted.100.monthly = bt.run.share(data, clean.signal=T)
	

	#*****************************************************************
	# Create Report
	#****************************************************************** 
png(filename = 'plot8.png', width = 800, height = 600, units = 'px', pointsize = 12, bg = 'white')										
	# Plot performance
	plotbt(models, plotX = T, log = 'y', LeftMargin = 3)	    	
		mtext('Cumulative Performance', side = 2, line = 1)
dev.off()	
	
png(filename = 'plot9.png', width = 1200, height = 800, units = 'px', pointsize = 12, bg = 'white')		
	plotbt.custom.report.part2(rev(models))
dev.off()	
		
	
}	
	




###############################################################################
# Rolling Correlation
# http://www.activetradermag.com/index.php/c/Trading_Strategies/d/Trading_correlation
###############################################################################
bt.rolling.cor.test <- function() 
{
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load.packages('quantmod')	
	tickers = sp500.components()$tickers
	
	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)		
	bt.prep(data, align='keep.all', dates='1970::')

	spy = getSymbols('SPY', src = 'yahoo', from = '1970-01-01', auto.assign = F)
		ret.spy = coredata( Cl(spy) / mlag(Cl(spy))-1 )
	
	#*****************************************************************
	# Code Logic
	#****************************************************************** 
	prices = data$prices['1993:01:29::']  
		nperiods = nrow(prices)
			
	ret = prices / mlag(prices) - 1
		ret = coredata(ret)
		
	# require at least 100 stocks with prices
	index = which((count(t(prices)) > 100 ))
		index = index[-c(1:252)]
		
	# average correlation among S&P 500 components
	avg.cor = NA * prices[,1]
	
	# average correlation between the S&P 500 index (SPX) and its component stocks
	avg.cor.spy = NA * prices[,1]
	
	for(i in index) {
		hist = ret[ (i- 252 +1):i, ]
		hist = hist[ , count(hist)==252, drop=F]
			nleft = ncol(hist)
		
		correlation = cor(hist, use='complete.obs',method='pearson')
		avg.cor[i,] = (sum(correlation) - nleft) / (nleft*(nleft-1))
		
		avg.cor.spy[i,] = sum(cor(ret.spy[ (i- 252 +1):i, ], hist, use='complete.obs',method='pearson')) / nleft
		
		if( i %% 100 == 0) cat(i, 'out of', nperiods, '\n')
	}
		
	#*****************************************************************
	# Create Report
	#****************************************************************** 				
png(filename = 'plot.sp500.cor.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')										

	
 	sma50 = SMA(Cl(spy), 50)
 	sma200 = SMA(Cl(spy), 200)
 	
 	cols = col.add.alpha(spl('green,red'),50)
	plota.control$col.x.highlight = iif(sma50 > sma200, cols[1], cols[2])
	highlight = sma50 > sma200 | sma50 < sma200
			
	plota(avg.cor, type='l', ylim=range(avg.cor, avg.cor.spy, na.rm=T), x.highlight = highlight,
			main='Average 252 day Pairwise Correlation for stocks in SP500')
		plota.lines(avg.cor.spy, type='l', col='blue')
		plota.legend('Pairwise Correlation,Correlation with SPY,SPY 50-day SMA > 200-day SMA,SPY 50-day SMA < 200-day SMA', 
		c('black,blue',cols))
		
dev.off()	
		
}



###############################################################################
# Volatility Quantiles
###############################################################################
bt.volatility.quantiles.test <- function() 
{
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load.packages('quantmod')	
	tickers = sp500.components()$tickers
	
	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
		#save(data, file='data.sp500.components.Rdata') 
		#load(file='data.sp500.components.Rdata') 	
		
		# remove companies with less than 5 years of data
		rm.index = which( sapply(ls(data), function(x) nrow(data[[x]])) < 1000 )	
		rm(list=names(rm.index), envir=data)
		
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)		
	bt.prep(data, align='keep.all', dates='1994::')
	
	
	
	data.spy <- new.env()
	getSymbols('SPY', src = 'yahoo', from = '1970-01-01', env = data.spy, auto.assign = T)
	bt.prep(data.spy, align='keep.all', dates='1994::')
	
	#*****************************************************************
	# Code Strategies
	#****************************************************************** 
	# setdiff(index(data.spy$prices), index(data$prices))
	# setdiff(index(data$prices),index(data.spy$prices))
	prices = data$prices
		nperiods = nrow(prices)
		n = ncol(prices)
			
	models = list()
	
	# SPY
	data.spy$weight[] = NA
		data.spy$weight[] = 1
	models$spy = bt.run(data.spy)
	
	# Equal Weight
	data$weight[] = NA
		data$weight[] = ntop(prices, 500)
	models$equal.weight = bt.run(data)
	
	#*****************************************************************
	# Create Quantiles based on the historical one year volatility 
	#****************************************************************** 
	# setup re-balancing periods
#	period.ends = 1:nperiods
	period.ends = endpoints(prices, 'weeks')
#	period.ends = endpoints(prices, 'months')
		period.ends = period.ends[period.ends > 0]
	
	# compute historical one year volatility	
	p = bt.apply.matrix(coredata(prices), ifna.prev)	
	ret = p / mlag(p) - 1		
	sd252 = bt.apply.matrix(ret, runSD, 252)		
		
	# split stocks in the S&amp;P 500 into Quantiles using one year historical Volatility	
	n.quantiles=5
	start.t = which(period.ends >= (252+2))[1]
	quantiles = weights = p * NA			
	
	for( t in start.t:len(period.ends) ) {
		i = period.ends[t]

		factor = sd252[i,]
		ranking = ceiling(n.quantiles * rank(factor, na.last = 'keep','first') / count(factor))
	
		quantiles[i,] = ranking
		weights[i,] = 1/tapply(rep(1,n), ranking, sum)[ranking]			
	}

	quantiles = ifna(quantiles,0)
	
	#*****************************************************************
	# Create backtest for each Quintile
	#****************************************************************** 
	for( i in 1:n.quantiles) {
		temp = weights * NA
		temp[period.ends,] = 0
		temp[quantiles == i] = weights[quantiles == i]
	
		data$weight[] = NA
			data$weight[] = temp
		models[[ paste('Q',i,sep='_') ]] = bt.run(data, silent = T)
	}
	rowSums(models$Q_2$weight,na.rm=T)	
	
	#*****************************************************************
	# Create Report
	#****************************************************************** 					
	# put all reports into one pdf file
	#pdf(file = 'report.pdf', width=8.5, height=11)
	
	png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')		
		plotbt.custom.report.part1(models)		
	dev.off()		
	
	png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')		
		plotbt.strategy.sidebyside(models)
	dev.off()		

		
	

	# save summary
	#load.packages('abind')
	#out = abind(lapply(models, function(m) m$equity))	
	#	colnames(out) = names(models)
	#write.xts(make.xts(out, index(prices)), 'report.csv')	
}





###############################################################################
# Factor Attribution & Value Quantiles
###############################################################################
bt.fa.value.quantiles.test <- function() 
{
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load.packages('quantmod')	
	tickers = sp500.components()$tickers
	#tickers = dow.jones.components()
	
	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
		#save(data, file='data.sp500.components.Rdata') 
		#load(file='data.sp500.components.Rdata') 	
		
		# remove companies with less than 5 years of data
		rm.index = which( sapply(ls(data), function(x) nrow(data[[x]])) < 1000 )	
		rm(list=names(rm.index), envir=data)
		
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)		
	bt.prep(data, align='keep.all', dates='1994::')
		tickers = data$symbolnames
	
	
	data.spy <- new.env()
	getSymbols('SPY', src = 'yahoo', from = '1970-01-01', env = data.spy, auto.assign = T)
	bt.prep(data.spy, align='keep.all', dates='1994::')
	
	#*****************************************************************
	# Code Strategies
	#****************************************************************** 
	# setdiff(index(data.spy$prices), index(data$prices))
	# setdiff(index(data$prices),index(data.spy$prices))
	prices = data$prices
		nperiods = nrow(prices)
		n = ncol(prices)
			
	models = list()
	
	# SPY
	data.spy$weight[] = NA
		data.spy$weight[] = 1
	models$spy = bt.run(data.spy)
	
	# Equal Weight
	data$weight[] = NA
		data$weight[] = ntop(prices, n)
	models$equal.weight = bt.run(data)
	
	#*****************************************************************
	# Compute Factor Attribution for each ticker
	#****************************************************************** 
	periodicity = 'weeks'
	
	# load Fama/French factors
	factors = get.fama.french.data('F-F_Research_Data_Factors', periodicity = periodicity,download = F, clean = F)
	
	period.ends = endpoints(data$prices, periodicity)
		period.ends = period.ends[period.ends > 0]
	
	# add factors and align
	data.fa <- new.env()
		for(i in tickers) data.fa[[i]] = data[[i]][period.ends,]
	data.fa$factors = factors$data / 100
	bt.prep(data.fa, align='remove.na')

	
	index = match( index(data.fa$prices), index(data$prices) )
	measure = data$prices[ index, ]
		
	for(i in tickers) {
		cat(i, '\n')
		
		# Facto Loadings Regression
		obj = factor.rolling.regression(data.fa, i, 36, silent=T)
		
		measure[,i] = coredata(obj$fl$estimate$HML)
	}
		
	
	#*****************************************************************
	# Create Value Quantiles
	#****************************************************************** 
	n.quantiles=5
	start.t = 1+36
	quantiles = weights = coredata(measure) * NA			
	
	for( t in start.t:nrow(weights) ) {
		factor = as.vector(coredata(measure[t,]))
		ranking = ceiling(n.quantiles * rank(factor, na.last = 'keep','first') / count(factor))
		#tapply(factor,ranking,sum)
		
		quantiles[t,] = ranking
		weights[t,] = 1/tapply(rep(1,n), ranking, sum)[ranking]			
	}

	quantiles = ifna(quantiles,0)
	
	#*****************************************************************
	# Create backtest for each Quintile
	#****************************************************************** 
	for( i in 1:n.quantiles) {
		temp = weights * NA
			temp[] = 0
		temp[quantiles == i] = weights[quantiles == i]
	
		data$weight[] = NA
			data$weight[index,] = temp
		models[[ paste('Q',i,sep='_') ]] = bt.run(data, silent = T)
	}
	
	#*****************************************************************
	# Create Report
	#****************************************************************** 					
	# put all reports into one pdf file
	#pdf(file = 'report.pdf', width=8.5, height=11)
	
	png(filename = 'plot1.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')		
		plotbt.custom.report.part1(models)		
	dev.off()		
	
	png(filename = 'plot2.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')		
		plotbt.strategy.sidebyside(models)
	dev.off()	

}
	
	
###############################################################################
# Three Factor Rolling Regression Viewer
# http://mas.xtreemhost.com/
############################################################################### 
# New fund regression calculator
# http://www.bogleheads.org/forum/viewtopic.php?t=11506&amp;highlight=regression
#
# Factor loadings?
# http://www.bogleheads.org/forum/viewtopic.php?t=14629
#
# Efficient Frontier: Rolling Your Own: Three-Factor Analysis
# http://www.efficientfrontier.com/ef/101/roll101.htm
#
# Kenneth R French: Data Library
# http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html
############################################################################### 
# alpha: how much 'extra' return did the fund have that could not be accounted for by the model. this is almost never large or statistically significant.
# B(MKT) = market factor: most 100% stock funds have a market factor near 1.0. higher values may indicate leverage. lower values may indicate cash and or bonds.
# B(SMB) = size factor (small minus big): positive values indicate the fund's average holding is smaller than the market
# B(HML) = value factor (high minus low): positive values indicate the fund's average holding is more 'value' oriented than the market (based on book to market ratio)
# R2: measures how well the fund returns match the model (values close to 1.0 indicate a good statistical fit)
############################################################################### 
factor.rolling.regression <- function(
	data, 
	ticker = data$symbolnames[-grep('factor', data$symbolnames)], 
	window.len = 36,
	silent = F,
	custom.stats.fn = NULL
) 
{
	ticker = ticker[1]
	
	#*****************************************************************
	# Facto Loadings Regression over whole period
	#****************************************************************** 
	prices = data$prices
		nperiods = nrow(prices)
		dates = index(data$prices)
	
	# compute simple returns	
	hist.returns = ROC(prices[,ticker], type = 'discrete')	
		hist.returns = hist.returns - data$factors$RF
	yout = hist.returns
	y = coredata(yout)
	
	xout = data$factors[, -which(names(data$factors) == 'RF')]
	x = coredata(xout)
		
#	fit = summary(lm(y~x))
#		est = fit$coefficients[,'Estimate']
#		std.err = fit$coefficients[,'Std. Error']
#		r2 = fit$r.squared

	ok.index = !(is.na(y) | (rowSums(is.na(x)) > 0))
	fit = ols(cbind(1,x[ok.index,]),y[ok.index], T)
		est = fit$coefficients
		std.err = fit$seb
	 	r2 = fit$r.squared

	 		
	# Facto Loadings - fl
	fl.all = list()
		fl.all$estimate = c(est, r2)
		fl.all$std.error = c(std.err, NA)

	#*****************************************************************
	# Facto Loadings Regression over Month window
	#****************************************************************** 
    colnames = c('alpha', colnames(x), 'R2')
    
    estimate = make.xts(matrix(NA, nr = nperiods, len(colnames)), dates)
    	colnames(estimate) = colnames
    fl = list()
    	fl$estimate = estimate
    	fl$std.error = estimate
   		if( !is.null(custom.stats.fn) ) {
   			temp = match.fun(custom.stats.fn)(cbind(1,x), y, fit)
   			fl$custom = make.xts(matrix(NA, nr = nperiods, len(temp)), dates)	
   		} 	
	
	# main loop
	for( i in window.len:nperiods ) {
		window.index = (i - window.len + 1) : i
		
		if(all(!is.na(y[window.index]))) {
		xtemp = cbind(1,x[window.index,])
		ytemp = y[window.index]
		fit = ols(xtemp, ytemp, T)
			est = fit$coefficients
			std.err = fit$seb
		 	r2 = fit$r.squared		
		fl$estimate[i,] = c(est, r2)
		fl$std.error[i,] = c(std.err, NA)
		
		if( !is.null(custom.stats.fn) )
			fl$custom[i,] = match.fun(custom.stats.fn)(xtemp, ytemp, fit)
		}
		
		if( i %% 10 == 0) if(!silent) cat(i, '\n')
	}

	return(list(fl.all = fl.all, fl = fl, window.len=window.len,
		y=yout, x=xout, RF=data$factors$RF))
}



# detail plot for each factor and histogram
factor.rolling.regression.detail.plot <- function(obj) {
	#setup
	n = ncol(obj$fl$estimate)
	dates = index(obj$fl$estimate)

	layout(matrix(1:(2*n), nc=2, byrow=T))
	
	for(i in 1:n) {	
		#-------------------------------------------------------------------------
		# Time plot
		#-------------------------------------------------------------------------
		est = obj$fl$estimate[,i]
		est.std.error = ifna(obj$fl$std.error[,i], 0)
		
		plota(est, 
			ylim = range( c(
				range(est + est.std.error, na.rm=T),
				range(est - est.std.error, na.rm=T)		
				)))
	
		polygon(c(dates,rev(dates)), 
			c(coredata(est + est.std.error), 
			rev(coredata(est - est.std.error))), 
			border=NA, col=col.add.alpha('red',50))
	
		est = obj$fl.all$estimate[i]
		est.std.error = obj$fl.all$std.error[i]
	
		polygon(c(range(dates),rev(range(dates))), 
			c(rep(est + est.std.error,2),
			rep(est - est.std.error,2)),
			border=NA, col=col.add.alpha('blue',50))
		
		abline(h=0, col='blue', lty='dashed')
			
		abline(h=est, col='blue')
	
		plota.lines(obj$fl$estimate[,i], type='l', col='red')
		
		#-------------------------------------------------------------------------
		# Histogram
		#-------------------------------------------------------------------------
		par(mar = c(4,3,2,1))
		hist(obj$fl$estimate[,i], col='red', border='gray', las=1,
			xlab='', ylab='', main=colnames(obj$fl$estimate)[i])
			abline(v=obj$fl.all$estimate[i], col='blue', lwd=2)
	}
}


# style plot for 2 given factors
factor.rolling.regression.style.plot <- function(obj, 
	xfactor='HML', yfactor='SMB',
	xlim = c(-1.5, 1.5), ylim = c(-0.5, 1.5)
) {
	# Style chart	
	i = which(colnames(obj$fl$estimate) == xfactor)
		x = coredata(obj$fl$estimate[,i])
		x.e = ifna(coredata(obj$fl$std.error[,i]), 0)
	
		x.all = obj$fl.all$estimate[i]
		x.all.e = obj$fl.all$std.error[i]
		
		xlab = colnames(obj$fl$estimate)[i]
		
	i = which(colnames(obj$fl$estimate) == yfactor)
		y = coredata(obj$fl$estimate[,i])
		y.e = ifna(coredata(obj$fl$std.error[,i]), 0)
	
		y.all = obj$fl.all$estimate[i]
		y.all.e = obj$fl.all$std.error[i]

		ylab = colnames(obj$fl$estimate)[i]
		
	# plot
	layout(1)
	plot(x,y, xlab=xlab, ylab = ylab, type='n', las=1,
		xlim = range(c(x + x.e, x - x.e, xlim), na.rm=T),
		ylim = range(c(y + y.e, y - y.e, ylim), na.rm=T),
		main = paste('Style, last =', ylab, round(last(y),2), xlab, round(last(x),2))
		)		
	grid()
	abline(h=0)
	abline(v=0)
	
		
	col = col.add.alpha('pink',250)
	rect(x - x.e, y - y.e, x + x.e, y + y.e, col=col, border=NA)
	
	points(x,y, col='red', pch=20)
		points(last(x),last(y), col='black', pch=3)	
	points(x.all,y.all, col='blue', pch=15)
	
	legend('topleft', spl('Estimates,Last estimate,Overall estimate'),
		pch = c(20,3,15),
		col = spl('red,black,blue'),
		pt.bg = spl('red,black,blue'),
		bty='n'
	) 
}


# re-construct historical perfromance based on factor loadings
# compare fund perfromance to the
# - re-constructed portfolio based on the regression over whole period
# - re-constructed portfolio based on the rolling window regression
factor.rolling.regression.bt.plot <- function(obj) {
	# setup
	ticker = colnames(obj$y)
		n = ncol(obj$fl$estimate)-1
		nperiods = nrow(obj$fl$estimate)
	
	# fund, alpha, factors, RF
	ret = cbind(obj$RF, obj$y, 1, obj$x)
		colnames(ret)[1:3] = spl('RF,fund,alpha')
	prices = bt.apply.matrix(1+ifna(ret,0),cumprod)
	
	data <- new.env()
		data$symbolnames = colnames(prices)		
		
	for(i in colnames(prices)) {
		data[[i]] = prices[,i]
		colnames(data[[i]]) = 'Close'
	}

	bt.prep(data, align='keep.all')
	
	#*****************************************************************
	# Code Strategies
	#****************************************************************** 	
	
	# create models
	models = list()
	
	data$weight[] = NA
		data$weight$fund = 1
		data$weight$RF = 1
		data$weight[1:obj$window.len,] = NA
	models[[ticker]] = bt.run.share(data, clean.signal = F)

	data$weight[] = NA
		data$weight[,3:(n+2)] = t(repmat(obj$fl.all$estimate[1:n], 1, nperiods))
		data$weight$RF = 1
		data$weight[1:obj$window.len,] = NA
	models$all.alpha = bt.run.share(data, clean.signal = F)

	data$weight[] = NA
		data$weight[,3:(n+2)] = t(repmat(obj$fl.all$estimate[1:n], 1, nperiods))
		data$weight$RF = 1
		data$weight$alpha = NA
		data$weight[1:obj$window.len,] = NA
	models$all = bt.run.share(data, clean.signal = F)

	data$weight[] = NA
		data$weight[,3:(n+2)] = obj$fl$estimate[,1:n]
		data$weight$RF = 1
		data$weight[1:obj$window.len,] = NA
	models$est.alpha = bt.run.share(data, clean.signal = F)
	
	data$weight[] = NA
		data$weight[,3:(n+2)] = obj$fl$estimate[,1:n]
		data$weight$RF = 1
		data$weight$alpha = NA	
		data$weight[1:obj$window.len,] = NA
	models$est = bt.run.share(data, clean.signal = F)
				
	#*****************************************************************
	# Create Report
	#****************************************************************** 	
	# Plot perfromance
	layout(1)
	plotbt(models, plotX = T, log = 'y', LeftMargin = 3)	    	
		mtext('Cumulative Performance', side = 2, line = 1)

	
	# obj$fl.all$estimate[1]*52
	# mean(obj$fl$estimate$alpha,na.rm=T)	
}


# main function to demonstrate factor attribution
three.factor.rolling.regression <- function() {
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load.packages('quantmod')	
	tickers = 'VISVX'
	#tickers = 'IBM'

	periodicity = 'weeks'
	periodicity = 'months'
		
	data <- new.env()
	quantmod::getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
	for(i in ls(data)) {
		temp = adjustOHLC(data[[i]], use.Adjusted=T)							
		
		period.ends = endpoints(temp, periodicity)
			period.ends = period.ends[period.ends > 0]

		if(periodicity == 'months') {
			# reformat date to match Fama French Data
			monthly.dates = as.Date(paste(format(index(temp)[period.ends], '%Y%m'),'01',sep=''), '%Y%m%d')
			data[[i]] = make.xts(coredata(temp[period.ends,]), monthly.dates)
		} else
			data[[i]] = temp[period.ends,]
	}
	data.fund = data[[tickers]]
	
	#*****************************************************************
	# Fama/French factors
	#****************************************************************** 
	factors = get.fama.french.data('F-F_Research_Data_Factors', periodicity = periodicity,download = T, clean = F)

	# add factors and align
	data <- new.env()
		data[[tickers]] = data.fund
	data$factors = factors$data / 100
	bt.prep(data, align='remove.na', dates='1994::')

	#*****************************************************************
	# Facto Loadings Regression
	#****************************************************************** 
	obj = factor.rolling.regression(data, tickers, 36)

	#*****************************************************************
	# Reports
	#****************************************************************** 
png(filename = 'plot1.png', width = 600, height = 1200, units = 'px', pointsize = 12, bg = 'white')		
	factor.rolling.regression.detail.plot(obj)
dev.off()

png(filename = 'plot2.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')			
	factor.rolling.regression.style.plot(obj)
dev.off()	

png(filename = 'plot3.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')				
	factor.rolling.regression.bt.plot(obj)
dev.off()	
	
	#*****************************************************************
	# Fama/French factors + Momentum
	#****************************************************************** 
	factors = get.fama.french.data('F-F_Research_Data_Factors', periodicity = periodicity,download = F, clean = F)

	factors.extra = get.fama.french.data('F-F_Momentum_Factor', periodicity = periodicity,download = T, clean = F)	
		factors$data = merge(factors$data, factors.extra$data) 
	
	# add factors and align
	data <- new.env()
		data[[tickers]] = data.fund
	data$factors = factors$data / 100
	bt.prep(data, align='remove.na', dates='1994::')

	#*****************************************************************
	# Facto Loadings Regression
	#****************************************************************** 
	obj = factor.rolling.regression(data, tickers, 36)

	#*****************************************************************
	# Reports
	#****************************************************************** 
png(filename = 'plot4.png', width = 600, height = 1200, units = 'px', pointsize = 12, bg = 'white')		
	factor.rolling.regression.detail.plot(obj)
dev.off()

png(filename = 'plot5.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')			
	factor.rolling.regression.style.plot(obj)
dev.off()	

png(filename = 'plot6.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')			
	factor.rolling.regression.style.plot(obj, xfactor='HML', yfactor='Mom')
dev.off()	

png(filename = 'plot7.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')				
	factor.rolling.regression.bt.plot(obj)
dev.off()	

}

# exmple of using your own factors in the factor attribution
your.own.factor.rolling.regression <- function() {
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load.packages('quantmod')	
	tickers = spl('EEM,SPY')
	
	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)		
	bt.prep(data, align='remove.na')
	
	#*****************************************************************
	# Create weekly factor
	#****************************************************************** 
	prices = data$prices
	
	periodicity = 'weeks'
	period.ends = endpoints(prices, periodicity)
		period.ends = period.ends[period.ends > 0]
	
	hist.returns = ROC(prices[period.ends,], type = 'discrete')	
		hist.returns = na.omit(hist.returns)
	
	#Emerging Market over US Market i.e. MSCI EM vs S&P 500 = EEM - SPY	
	EEM_SPY = hist.returns$EEM - hist.returns$SPY
		colnames(EEM_SPY) = 'EEM_SPY'
	
	write.xts(EEM_SPY, 'EEM_SPY.csv')
	
	
	
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load.packages('quantmod')	
	tickers = 'VISVX'
		
	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
	for(i in ls(data)) {
		temp = adjustOHLC(data[[i]], use.Adjusted=T)							
		
		period.ends = endpoints(temp, periodicity)
			period.ends = period.ends[period.ends > 0]

		data[[i]] = temp[period.ends,]
	}
	data.fund = data[[tickers]]
	
	
	#*****************************************************************
	# Fama/French factors
	#****************************************************************** 
	factors = get.fama.french.data('F-F_Research_Data_Factors', periodicity = periodicity,download = F, clean = F)

	factors.extra = 100 * read.xts('EEM_SPY.csv')
		factors$data = merge(factors$data, factors.extra, join='inner')
	# add factors and align
	data <- new.env()
		data[[tickers]] = data.fund
	data$factors = factors$data / 100
	bt.prep(data, align='remove.na')
	
	#*****************************************************************
	# Check Correlations, make sure the new factor is NOT highly correlated 
	#****************************************************************** 	
	load.packages('psych')
png(filename = 'plot1.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')		
	pairs.panels(coredata(data$factors))	
dev.off()
	



	
	#*****************************************************************
	# Facto Loadings Regression
	#****************************************************************** 
	obj = factor.rolling.regression(data, tickers, 36)

	#*****************************************************************
	# Reports
	#****************************************************************** 
png(filename = 'plot2.png', width = 600, height = 1200, units = 'px', pointsize = 12, bg = 'white')		
	factor.rolling.regression.detail.plot(obj)
dev.off()

png(filename = 'plot3.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')			
	factor.rolling.regression.style.plot(obj)
dev.off()	

png(filename = 'plot4.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')			
	factor.rolling.regression.style.plot(obj, xfactor='HML', yfactor='EEM_SPY')
dev.off()	

png(filename = 'plot5.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')				
	factor.rolling.regression.bt.plot(obj)
dev.off()	


}
	




###############################################################################
# One month reversals
###############################################################################
bt.one.month.test <- function() 
{
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load.packages('quantmod')	
	tickers = sp500.components()$tickers
	#tickers = dow.jones.components()
	
	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
		#save(data, file='data.sp500.components.Rdata') 
		#load(file='data.sp500.components.Rdata') 	
		
		# remove companies with less than 5 years of data
		rm.index = which( sapply(ls(data), function(x) nrow(data[[x]])) < 1000 )	
		rm(list=names(rm.index), envir=data)
		
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)		
	bt.prep(data, align='keep.all', dates='1994::')
		tickers = data$symbolnames
	
	
	data.spy <- new.env()
	getSymbols('SPY', src = 'yahoo', from = '1970-01-01', env = data.spy, auto.assign = T)
	bt.prep(data.spy, align='keep.all', dates='1994::')
	
	
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
#save(data, data.spy, tickers, file='data.sp500.components.Rdata') 	
#load(file='data.sp500.components.Rdata') 	

	#*****************************************************************
	# Code Strategies
	#****************************************************************** 
	prices = data$prices
		n = ncol(prices)
					
	#*****************************************************************
	# Setup monthly periods
	#****************************************************************** 
	periodicity = 'months'
	
	period.ends = endpoints(data$prices, periodicity)
		period.ends = period.ends[period.ends > 0]
	
	prices = prices[period.ends, ]		
		
	#*****************************************************************
	# Create Benchmarks
	#****************************************************************** 	
	models = list()
	n.skip = 36
	n.skip = 2
	
	# SPY
	data.spy$weight[] = NA
		data.spy$weight[] = 1
		data.spy$weight[1:period.ends[n.skip],] = NA
	models$spy = bt.run(data.spy)
	
	# Equal Weight
	data$weight[] = NA
		data$weight[period.ends,] = ntop(prices, n)
		data$weight[1:period.ends[n.skip],] = NA		
	models$equal.weight = bt.run(data)
	
	#*****************************************************************
	# Create Reversal Quantiles
	#****************************************************************** 
	one.month = coredata(prices / mlag(prices))

	models = c(models, 
		bt.make.quintiles(one.month, data, period.ends, start.t=1 + n.skip, prefix='M1_'))

	
	#*****************************************************************
	# Create Report
	#****************************************************************** 	
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')					
	plotbt.custom.report.part1(models)
dev.off()	

png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')					
	plotbt.custom.report.part1(models[spl('spy,equal.weight,spread')])
dev.off()		

}	



###############################################################################
# Better one-month reversal
###############################################################################
# compute various additional stats
factor.rolling.regression.custom.stats <- function(x,y,fit) {
	n = len(y)
	e = y - x %*% fit$coefficients
	se = sd(e)
	return(c(e[n], e[n]/se))
}

# create quintiles
bt.make.quintiles <- function(
	position.score,	# position.score is a factor to form Quintiles sampled at the period.ends
	data,			# back-test object
	period.ends,	
	n.quantiles = 5,
	start.t = 2,	# first index at which to form Quintiles
	prefix = ''
) 
{
	n = ncol(position.score)
	#*****************************************************************
	# Create Quantiles
	#****************************************************************** 
	position.score = coredata(position.score)
	quantiles = weights = position.score * NA
	
	for( t in start.t:nrow(weights) ) {
		factor = as.vector(position.score[t,])
		ranking = ceiling(n.quantiles * rank(factor, na.last = 'keep','first') / count(factor))
		
		quantiles[t,] = ranking
		weights[t,] = 1/tapply(rep(1,n), ranking, sum)[ranking]			
	}
	
	quantiles = ifna(quantiles,0)
	
	#*****************************************************************
	# Create backtest for each Quintile
	#****************************************************************** 
	temp = weights * NA
	models = list()
	for( i in 1:n.quantiles) {
		temp[] = 0
		temp[quantiles == i] = weights[quantiles == i]
	
		data$weight[] = NA
			data$weight[period.ends,] = temp
		models[[ paste(prefix,'Q',i,sep='') ]] = bt.run(data, silent = T)
	}
	
	# rowSums(models$M1_Q2$weight,na.rm=T)	

	#*****************************************************************
	# Create Q1-QN spread
	#****************************************************************** 
	temp[] = 0
	temp[quantiles == 1] = weights[quantiles == 1]
	temp[quantiles == n.quantiles] = -weights[quantiles == n.quantiles]
	
	data$weight[] = NA
		data$weight[period.ends,] = temp
	models$spread = bt.run(data, silent = T)	

	return(models)
}



bt.fa.one.month.test <- function() 
{
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load.packages('quantmod')	
	info = sp500.components()
	tickers = info$tickers
	#tickers = dow.jones.components()
	
	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
		#save(data, file='data.sp500.components.Rdata') 
		#load(file='data.sp500.components.Rdata') 	
		
		# remove companies with less than 5 years of data
		rm.index = which( sapply(ls(data), function(x) nrow(data[[x]])) < 1000 )	
		rm(list=names(rm.index), envir=data)
		
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)		
	bt.prep(data, align='keep.all', dates='1994::')
		tickers = data$symbolnames
		sector = info$sector[match(tickers, info$tickers)]
	
	
	data.spy <- new.env()
	getSymbols('SPY', src = 'yahoo', from = '1970-01-01', env = data.spy, auto.assign = T)
	bt.prep(data.spy, align='keep.all', dates='1994::')
	
	
	#*****************************************************************
	# Code Strategies
	#****************************************************************** 
	# setdiff(index(data.spy$prices), index(data$prices))
	# setdiff(index(data$prices),index(data.spy$prices))
#save(data, data.spy, tickers, sector, file='data.sp500.components.Rdata') 	
#load(file='data.sp500.components.Rdata') 	



	prices = data$prices
		n = ncol(prices)
					
	#*****************************************************************
	# Setup monthly periods
	#****************************************************************** 
	periodicity = 'months'
	
	period.ends = endpoints(data$prices, periodicity)
		period.ends = period.ends[period.ends > 0]
	
	prices = prices[period.ends, ]		
		
	#*****************************************************************
	# Create Benchmarks
	#****************************************************************** 	
	models = list()
	n.skip = 36
	
	# SPY
	data.spy$weight[] = NA
		data.spy$weight[] = 1
		data.spy$weight[1:period.ends[n.skip],] = NA
	models$spy = bt.run(data.spy)
	
	# Equal Weight
	data$weight[] = NA
		data$weight[period.ends,] = ntop(prices, n)
		data$weight[1:period.ends[n.skip],] = NA		
	models$equal.weight = bt.run(data)
			
	#*****************************************************************
	# Load factors and align them with prices
	#****************************************************************** 	
	# load Fama/French factors
	factors = get.fama.french.data('F-F_Research_Data_Factors', periodicity = periodicity,download = F, clean = F)
	
	# align monthly dates
	map = match(format(index(factors$data), '%Y%m'), format(index(prices), '%Y%m'))
		dates = index(factors$data)
		dates[!is.na(map)] = index(prices)[na.omit(map)]
	index(factors$data) = as.Date(dates)
		
	
	# add factors and align
	data.fa <- new.env()
		for(i in tickers) data.fa[[i]] = data[[i]][period.ends, ]
		data.fa$factors = factors$data / 100
	bt.prep(data.fa, align='remove.na')

	
	index = match( index(data.fa$prices), index(data$prices) )
		prices = data$prices[index, ]
		
	#*****************************************************************
	# Compute Factor Attribution for each ticker
	#****************************************************************** 	

	temp = NA * prices
	factors	= list()
		factors$last.e = temp
		factors$last.e_s = temp
	
	for(i in tickers) {
		cat(i, '\n')
		
		# Facto Loadings Regression
		obj = factor.rolling.regression(data.fa, i, 36, silent=T,
			factor.rolling.regression.custom.stats)

		for(j in 1:len(factors))		
			factors[[j]][,i] = obj$fl$custom[,j]
			
	}
	
	# add base strategy
	factors$one.month = coredata(prices / mlag(prices))
	
	#save(factors, file='data.ff.factors.Rdata') 	
	load(file='data.ff.factors.Rdata') 	

	
	#*****************************************************************
	# Create Quantiles
	#****************************************************************** 
	quantiles = list()
	
	for(name in names(factors)) {
		cat(name, '\n')
		quantiles[[name]] = bt.make.quintiles(factors[[name]], data, index, start.t =  1+36, prefix=paste(name,'_',sep=''))
	}
	

	#*****************************************************************
	# Create Report
	#****************************************************************** 					
	# put all reports into one pdf file
	#pdf(file = 'report.pdf', width=8.5, height=11)
	
	png(filename = 'plot1.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')		
		plotbt.custom.report.part1(quantiles$one.month$spread,quantiles$last.e$spread,quantiles$last.e_s$spread)
	dev.off()		
	
	png(filename = 'plot2.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')		
		plotbt.strategy.sidebyside(quantiles$one.month$spread,quantiles$last.e$spread,quantiles$last.e_s$spread)
	dev.off()	


	png(filename = 'plot3.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')		
		plotbt.custom.report.part1(	quantiles$last.e )
	dev.off()		
		
	png(filename = 'plot4.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')		
		plotbt.custom.report.part1(	quantiles$last.e_s )
	dev.off()		

	
}
	


bt.fa.sector.one.month.test <- function() 
{
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load.packages('quantmod')	
	info = sp500.components()
	tickers = info$tickers
		#tickers = dow.jones.components()

	
	data <- new.env()
	#getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
	for(i in tickers) try(getSymbols(i, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T), TRUE)	
		#save(data, file='data.sp500.components.Rdata') 
		#load(file='data.sp500.components.Rdata') 	
		
		# remove companies with less than 5 years of data
		rm.index = which( sapply(ls(data), function(x) nrow(data[[x]])) < 1000 )	
		rm(list=names(rm.index), envir=data)
		
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)		
	bt.prep(data, align='keep.all', dates='1994::')
		tickers = data$symbolnames
		sector = info$sector[match(tickers, info$tickers)]
	
	
	data.spy <- new.env()
	getSymbols('SPY', src = 'yahoo', from = '1970-01-01', env = data.spy, auto.assign = T)
	bt.prep(data.spy, align='keep.all', dates='1994::')
	
	save(data, data.spy, tickers, sector, file='data.sp500.components.Rdata') 	
	#load(file='data.sp500.components.Rdata') 	

		
	#*****************************************************************
	# Code Strategies
	#****************************************************************** 
	# setdiff(index(data.spy$prices), index(data$prices))
	# setdiff(index(data$prices),index(data.spy$prices))

	prices = data$prices
		n = ncol(prices)
					
	#*****************************************************************
	# Setup monthly periods
	#****************************************************************** 
	periodicity = 'months'
	#periodicity = 'weeks'
	
	period.ends = endpoints(data$prices, periodicity)
		period.ends = period.ends[period.ends > 0]
	
	prices = prices[period.ends, ]		
		
	#*****************************************************************
	# Create Benchmarks
	#****************************************************************** 	
	models = list()
	n.skip = 36
	
	# SPY
	data.spy$weight[] = NA
		data.spy$weight[] = 1
		data.spy$weight[1:period.ends[n.skip],] = NA
	models$spy = bt.run(data.spy)
	
	# Equal Weight
	data$weight[] = NA
		data$weight[period.ends,] = ntop(prices, n)
		data$weight[1:period.ends[n.skip],] = NA		
	models$equal.weight = bt.run(data)
			
	#*****************************************************************
	# Load factors and align them with prices
	#****************************************************************** 	
	# load Fama/French factors
	factors = get.fama.french.data('F-F_Research_Data_Factors', periodicity = periodicity,download = T, clean = F)
	
	
	# align monthly dates
	if(periodicity == 'months') {
		map = match(format(index(factors$data), '%Y%m'), format(index(prices), '%Y%m'))
			dates = index(factors$data)
			dates[!is.na(map)] = index(prices)[na.omit(map)]
		index(factors$data) = as.Date(dates)
	}	
	
	# add factors and align
	data.fa <- new.env()
		for(i in tickers) data.fa[[i]] = data[[i]][period.ends, ]
		data.fa$factors = factors$data / 100
	bt.prep(data.fa, align='remove.na')

		
	index = match( index(data.fa$prices), index(data$prices) )
		prices = data$prices[index, ]
		
	#*****************************************************************
	# Compute Factor Attribution for each ticker
	#****************************************************************** 	
	temp = NA * prices
	factors	= list()
		factors$last.e = temp
		factors$last.e_s = temp
	
	for(i in tickers) {
		cat(i, '\n')
		
		# Facto Loadings Regression
		obj = factor.rolling.regression(data.fa, i, 36, silent=T,
			factor.rolling.regression.custom.stats)

		for(j in 1:len(factors))		
			factors[[j]][,i] = obj$fl$custom[,j]
			
	}

	# add base strategy
	nlag = iif(periodicity == 'months', 1, 4)
	factors$one.month = coredata(prices / mlag(prices, nlag))	
					
	save(factors, file='data.ff.factors.Rdata') 	
	#load(file='data.ff.factors.Rdata') 	

	
	
	#*****************************************************************
	# Create Quantiles
	#****************************************************************** 
	quantiles = list()
	
	for(name in names(factors)) {
		cat(name, '\n')
		quantiles[[name]] = bt.make.quintiles(factors[[name]], data, index, start.t =  1+36, prefix=paste(name,'_',sep=''))
	}

	quantiles.sn = list()
	for(name in names(factors)) {
		cat(name, '\n')
		quantiles.sn[[name]] = bt.make.quintiles.sector(sector, factors[[name]], data, index, start.t =  1+36, prefix=paste(name,'_',sep=''))
	}

	save(quantiles, quantiles.sn, file='model.quantiles.Rdata') 	
	#load(file='model.quantiles.Rdata') 	 	
	

	#*****************************************************************
	# Create Report
	#****************************************************************** 					
	# put all reports into one pdf file
	#pdf(file = 'report.pdf', width=8.5, height=11)
	
	png(filename = 'plot1.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')		
		plotbt.custom.report.part1(quantiles$one.month$spread,
			quantiles$last.e$spread, quantiles$last.e_s$spread,
			quantiles.sn$one.month$spread.sn,
			quantiles.sn$last.e$spread.sn, quantiles.sn$last.e_s$spread.sn)	
	dev.off()		
	
	png(filename = 'plot2.png', width = 800, height = 600, units = 'px', pointsize = 12, bg = 'white')		
		plotbt.strategy.sidebyside(quantiles$one.month$spread,
			quantiles$last.e$spread, quantiles$last.e_s$spread,
			quantiles.sn$one.month$spread.sn,
			quantiles.sn$last.e$spread.sn, quantiles.sn$last.e_s$spread.sn)	
	dev.off()	

	png(filename = 'plot3.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')		
		plotbt.custom.report.part1(	quantiles.sn$one.month )
	dev.off()				
	
	png(filename = 'plot4.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')		
		plotbt.custom.report.part1(	quantiles.sn$last.e_s )
	dev.off()		
	
	
	
	#*****************************************************************
	# Create Report - bt.one.month.test
	#****************************************************************** 	
	png(filename = 'plot1a.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')					
		plotbt.custom.report.part1(c(models,quantiles$one.month))
	dev.off()	

	png(filename = 'plot2a.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')					
		plotbt.custom.report.part1(c(models,quantiles$one.month$spread))
	dev.off()	

	#*****************************************************************
	# Create Report - bt.fa.one.month.test
	#****************************************************************** 						
	png(filename = 'plot1b.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')		
		plotbt.custom.report.part1(quantiles$one.month$spread,quantiles$last.e$spread,quantiles$last.e_s$spread)
	dev.off()		
	
	png(filename = 'plot2b.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')		
		plotbt.strategy.sidebyside(quantiles$one.month$spread,quantiles$last.e$spread,quantiles$last.e_s$spread)
	dev.off()	


	png(filename = 'plot3b.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')		
		plotbt.custom.report.part1(	quantiles$last.e )
	dev.off()		
		
	png(filename = 'plot4b.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')		
		plotbt.custom.report.part1(	quantiles$last.e_s )
	dev.off()		
	
}




#position.score = factors[[1]]
#period.ends	= index
#n.quantiles = 5
#start.t =  1+36
#prefix = ''	

# create sector quintiles		
bt.make.quintiles.sector <- function(
	sector,			# sector data
	position.score,	# position.score is a factor to form Quintiles sampled at the period.ends
	data,			# back-test object
	period.ends,	
	n.quantiles = 5,
	start.t = 2,	# first index at which to form Quintiles
	prefix = ''	
) 
{
	#*****************************************************************
	# Re-organize sectors into matrix, assume that sectors are constant in time
	#****************************************************************** 
	temp = factor(sector)
	sector.names = levels(temp)	
		n.sectors = len(sector.names)
	sectors = matrix(unclass(temp),nr=nrow(position.score),nc=ncol(position.score),byrow=T)
	
	#*****************************************************************
	# Create Quantiles
	#****************************************************************** 
	position.score = coredata(position.score)
	quantiles = weights = position.score * NA			
	
	for( s in 1:n.sectors) {
		for( t in start.t:nrow(weights) ) {
			index = sectors[t,] == s
			n = sum(index)
			
			# require at least 3 companies in each quantile
			if(n > 3*n.quantiles) {			
				factor = as.vector(position.score[t, index])
				ranking = ceiling(n.quantiles * rank(factor, na.last = 'keep','first') / count(factor))
			
				quantiles[t, index] = ranking
				weights[t, index] = 1/tapply(rep(1,n), ranking, sum)[ranking]			
			}
		}
	}	
	
	quantiles = ifna(quantiles,0)
	
	#*****************************************************************
	# Create Q1-QN spread for each Sector
	#****************************************************************** 
	long = weights * NA
	short = weights * NA
	models = list()
	
	for( s in 1:n.sectors) {
		long[] = 0
		long[quantiles == 1 & sectors == s] = weights[quantiles == 1 & sectors == s]
		long = long / rowSums(long,na.rm=T)
		
		short[] = 0
		short[quantiles == n.quantiles & sectors == s] = weights[quantiles == n.quantiles & sectors == s]
		short = short / rowSums(short,na.rm=T)
		
		data$weight[] = NA
			data$weight[period.ends,] = long - short
		models[[ paste(prefix,'spread.',sector.names[s], sep='') ]]	= bt.run(data, silent = T)	
	}

if(F) {	
	#*****************************************************************
	# Create Basic momentum strategy
	#****************************************************************** 			
	load.packages('abind')
	model.prices = abind(lapply(models, function(m) m$equity), along=2)	
		#model.prices = make.xts(model.prices, index(data$prices)
		model.prices = model.prices[period.ends,]
		model.returns = model.prices / mlag(model.prices)-1
		model.score = bt.apply.matrix(model.returns, SMA, 6) 
		model.vol = bt.apply.matrix(model.returns, runSD, 6) 

	# select top 3 sectors based on the 6 month momentum, risk weighted	
	top = ntop(model.score, 3)
		top = top / model.vol
	top = top / rowSums(top, na.rm=T)
	top = ifna(top,0)
	
	n = ncol(position.score)
	nperiods = nrow(position.score)

	long[] = 0
	short[] = 0
	for( s in 1:n.sectors) {
		score = matrix(top[,s], nr = nperiods, n)
		long[quantiles == 1 & sectors == s] = (weights * score)[quantiles == 1 & sectors == s]
		short[quantiles == n.quantiles & sectors == s] = (weights * score)[quantiles == n.quantiles & sectors == s]		
	}
	long = long / rowSums(long,na.rm=T)
	short = short / rowSums(short,na.rm=T)
	
	data$weight[] = NA
		data$weight[period.ends,] = long - short
	models$spread.sn.top3 = bt.run(data, silent = T)	

	
#plotbt.custom.report.part1(models)
#plotbt.strategy.sidebyside(models)
}	

	
	#*****************************************************************
	# Create Sector - Neutral Q1-QN spread
	#****************************************************************** 		
	long[] = 0
	long[quantiles == 1] = weights[quantiles == 1]
	long = long / rowSums(long,na.rm=T)
	
	short[] = 0
	short[quantiles == n.quantiles] = weights[quantiles == n.quantiles]
	short = short / rowSums(short,na.rm=T)
		
	data$weight[] = NA
		data$weight[period.ends,] = long - short
	models$spread.sn = bt.run(data, silent = T)	

	return(models)
}



###############################################################################
# Yet Another Forecast Dashboard
###############################################################################
# extract forecast info
forecast.helper <- function(fit, h=10, level = c(80,95)) {
	out = try( forecast(fit, h=h, level=level), silent=TRUE)
	if (class(out)[1] != 'try-error') {
		out = data.frame(out)
	} else {
		temp = data.frame(predict(fit, n.ahead=h, doplot=F))
			pred = temp[,1]
			se = temp[,2]
		qq = qnorm(0.5 * (1 + level/100))
		out = matrix(NA, nr=h, nc=1+2*len(qq))
			out[,1] = pred
		for(i in 1:len(qq))
			out[,(2*i):(2*i+1)] = c(pred - qq[i] * se, pred + qq[i] * se)
		colnames(out) = c('Point.Forecast', matrix(c(paste('Lo', level, sep='.'), paste('Hi', level, sep='.')), nr=2, byrow=T))
		out = data.frame(out)
	}	
	return(out)
}

# compute future dates for the forecast
forecast2xts <- function(data, forecast) {
	# length of the forecast
	h = nrow(forecast)
	dates = as.Date(index(data))
 		
	new.dates = seq(last(dates)+1, last(dates) + 2*365, by='day')
	rm.index = date.dayofweek(new.dates) == 6 | date.dayofweek(new.dates) == 0
 	new.dates = new.dates[!rm.index]
 		
 	new.dates = new.dates[1:h] 	
 	return(make.xts(forecast, new.dates))
}

# create forecast plot
forecast.plot <- function(data, forecast, ...) {
	out = forecast2xts(data, forecast)

 	# create plot
 	plota(c(data, out[,1]*NA), type='l', 
 			ylim = range(data,out,na.rm=T), ...) 		
 	
 	# highligh sections
	new.dates = index(out)
		temp = coredata(out)

	n = (ncol(out) %/% 2)
	for(i in n : 1) {
		polygon(c(new.dates,rev(new.dates)), 
			c(temp[,(2*i)], rev(temp[,(2*i+1)])), 
		border=NA, col=col.add.alpha(i+2,150))
	}
	
	plota.lines(out[,1], col='red')
	
	labels = c('Data,Forecast', paste(gsub('Lo.', '', colnames(out)[2*(1:n)]), '%', sep=''))
	plota.legend(labels, fill = c('black,red',col.add.alpha((1:n)+2, 150)))			
}


bt.forecast.dashboard <- function() {
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load.packages('quantmod')	
	tickers = spl('SPY')

	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '1990-01-01', env = data, auto.assign = T)
	bt.prep(data, align='remove.na')
	
	#*****************************************************************
	# Create models
	#****************************************************************** 
	load.packages('forecast,fGarch,fArma')
	
	sample = last(data$prices$SPY, 200)	
	ts.sample = ts(sample, frequency = 12)
	
	
	
	models = list(
		# fGarch		
		garch = garchFit(~arma(1,15)+garch(1,1), data=sample, trace=F),
		# fArma
		arima = armaFit(~ arima(1, 1, 15), data=ts.sample),	
		
		# forecast
		arma = Arima(ts.sample, c(1,0,1)),
		arfima = arfima(ts.sample),
		auto.arima = auto.arima(ts.sample),
	
		bats = bats(ts.sample),
		HoltWinters = HoltWinters(ts.sample),
		naive = Arima(ts.sample, c(0,1,0))
	)
	
	
	#*****************************************************************
	# Create Report
	#****************************************************************** 						
	png(filename = 'plot1.png', width = 800, height = 800, units = 'px', pointsize = 12, bg = 'white')		
		layout(matrix(1:9,nr=3))
		for(i in 1:len(models)) {
			out = forecast.helper(models[[i]], 30, level = c(80,95))	 	
			forecast.plot(sample, out, main = names(models)[i]) 	
		}	
	dev.off()		
	
	png(filename = 'plot2.png', width = 800, height = 800, units = 'px', pointsize = 12, bg = 'white')		
		layout(matrix(1:9,nr=3))
		for(i in 1:len(models)) {
			out = forecast.helper(models[[i]], 30, level = c(75,85,95,97,99))	 	
			forecast.plot(sample, out, main = names(models)[i]) 	
		}	
	dev.off()		
	
}
		


###############################################################################
# New 60/40
# http://gestaltu.blogspot.ca/2012/07/youre-looking-at-wrong-number.html	
###############################################################################
bt.new.60.40.test <- function() 
{
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load.packages('quantmod')	
	tickers = spl('SHY,IEF,TLT,SPY')

	data.all <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '1990-01-01', env = data.all, auto.assign = T)	
	for(i in ls(data.all)) data.all[[i]] = adjustOHLC(data.all[[i]], use.Adjusted=T)		
	bt.prep(data.all, align='remove.na')
	
	prices = data.all$prices
		n = ncol(prices)
		nperiods = nrow(prices)
	prices = prices/ matrix(first(prices), nr=nperiods, nc=n, byrow=T)
	
png(filename = 'plot1.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')			
	plota.matplot(prices)
dev.off()		
	
		
	#*****************************************************************
	# Load historical data
	#****************************************************************** 		
	data <- new.env()
		data$stock = data.all$SPY
		data$bond = data.all$TLT	
	bt.prep(data, align='remove.na')


	#*****************************************************************
	# Code Strategies
	#****************************************************************** 
	# all bonds began trading at 2002-07-31
	prices = data$prices
		n = ncol(prices)
		nperiods = nrow(prices)
	
	models = list()
	
	period.ends = endpoints(prices, 'months')
		period.ends = period.ends[period.ends > 0]

	
	#*****************************************************************
	# Traditional, Dollar Weighted 40% Bonds & 60% Stock
	#****************************************************************** 			
	weight.dollar = matrix(c(0.4, 0.6), nr=nperiods, nc=n, byrow=T)
	
	data$weight[] = NA
		data$weight[period.ends,] = weight.dollar[period.ends,]
	models$dollar.w.60.40 = bt.run.share(data, clean.signal=F)

	
	#*****************************************************************
	# Risk Weighted 40% Bonds & 60% Stock
	#****************************************************************** 				
	ret.log = bt.apply.matrix(prices, ROC, type='continuous')
	hist.vol = sqrt(252) * bt.apply.matrix(ret.log, runSD, n = 21)	
	weight.risk = weight.dollar / hist.vol
		weight.risk = weight.risk / rowSums(weight.risk)
		
	data$weight[] = NA
		data$weight[period.ends,] = weight.risk[period.ends,]
	models$risk.w.60.40 = bt.run.share(data, clean.signal=F)
				
	#*****************************************************************
	# Scale Risk Weighted 40% Bonds & 60% Stock strategy to have 6% volatility
	#****************************************************************** 				
	data$weight[] = NA
		data$weight[period.ends,] = target.vol.strategy(models$risk.w.60.40,
						weight.risk, 6/100, 21, 100/100)[period.ends,]
	models$risk.w.60.40.target6 = bt.run.share(data, clean.signal=T)
	
	#*****************************************************************
	# Same, plus invest cash into SHY
	#****************************************************************** 					
	weight = target.vol.strategy(models$risk.w.60.40,
						weight.risk, 6/100, 21, 100/100)
	data.all$weight[] = NA
		data.all$weight$SPY[period.ends,] = weight$stock[period.ends,]
		data.all$weight$TLT[period.ends,] = weight$bond[period.ends,]
		
		cash = 1-rowSums(weight)
		data.all$weight$SHY[period.ends,] = cash[period.ends]
	models$risk.w.60.40.target6.cash = bt.run.share(data.all, clean.signal=T)

	#*****************************************************************
	# Create Report
	#****************************************************************** 	

png(filename = 'plot2.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')			
	plotbt.strategy.sidebyside(models)
dev.off()		
	
png(filename = 'plot3.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')			
	plotbt.custom.report.part1(models)
dev.off()		
		
png(filename = 'plot4.png', width = 1200, height = 800, units = 'px', pointsize = 12, bg = 'white')			
	plotbt.custom.report.part2(models$risk.w.60.40.target6)		
dev.off()		
	
png(filename = 'plot5.png', width = 1200, height = 800, units = 'px', pointsize = 12, bg = 'white')			
	plotbt.custom.report.part2(models$risk.w.60.40.target6.cash)		
dev.off()		
	
	
}		





###############################################################################
# Adaptive Asset Allocation
# http://www.macquarieprivatewealth.ca/dafiles/Internet/mgl/ca/en/advice/specialist/darwin/documents/darwin-adaptive-asset-allocation.pdf
# http://cssanalytics.wordpress.com/2012/07/17/adaptive-asset-allocation-combining-momentum-with-minimum-variance/
###############################################################################



bt.aaa.combo <- function
(
	data,
	period.ends,
	n.top = 5,		# number of momentum positions
	n.mom = 6*22,	# length of momentum look back
	n.vol = 1*22 	# length of volatility look back
) 
{
    #*****************************************************************
    # Combo: weight positions in the Momentum Portfolio according to Volatliliy
    #*****************************************************************
    prices = coredata(data$prices)  
    ret.log = bt.apply.matrix(prices, ROC, type='continuous')
    hist.vol = bt.apply.matrix(ret.log, runSD, n = n.vol)   
    	adj.vol = 1/hist.vol[period.ends,]
    
    momentum = prices / mlag(prices, n.mom)
    
    weight = ntop(momentum[period.ends,], n.top) * adj.vol
		n.skip = max(n.mom, n.vol)
   
    data$weight[] = NA
        data$weight[period.ends,] = weight / rowSums(weight, na.rm=T)   
        data$weight[1 : n.skip,] = NA 
    bt.run.share(data, clean.signal=F, silent=T)
}

bt.aaa.minrisk <- function
(
	data,
	period.ends,
	n.top = 5,		# number of momentum positions
	n.mom = 6*22,	# length of momentum look back
	n.vol = 1*22 	# length of volatility look back
) 
{
    #*****************************************************************   
    # Adaptive Asset Allocation (AAA)
    # weight positions in the Momentum Portfolio according to 
    # the minimum variance algorithm
    #*****************************************************************   
    prices = coredata(data$prices)  
    ret.log = bt.apply.matrix(prices, ROC, type='continuous')
    
    momentum = prices / mlag(prices, n.mom)
    
    weight = NA * prices
        weight[period.ends,] = ntop(momentum[period.ends,], n.top)
	n.skip = max(n.mom, n.vol)
        
    for( i in period.ends[period.ends >= n.skip] ) {
    	hist = ret.log[ (i - n.vol + 1):i, ]
    	
		# require all assets to have full price history
		include.index = count(hist)== n.vol      

		# also only consider assets in the Momentum Portfolio
        index = ( weight[i,] > 0 ) & include.index
        n = sum(index)
        
		if(n > 0) {					
			hist = hist[ , index]
        
	        # create historical input assumptions
	        ia = create.historical.ia(hist, 252)
	            s0 = apply(coredata(hist),2,sd)       
	            ia$cov = cor(coredata(hist), use='complete.obs',method='pearson') * (s0 %*% t(s0))
	       
			# create constraints: 0<=x<=1, sum(x) = 1
			constraints = new.constraints(n, lb = 0, ub = 1)
			constraints = add.constraints(rep(1, n), 1, type = '=', constraints)       
			
			# compute minimum variance weights				            
	        weight[i,] = 0        
	        weight[i,index] = min.risk.portfolio(ia, constraints)
        }
    }

    # Adaptive Asset Allocation (AAA)
    data$weight[] = NA
        data$weight[period.ends,] = weight[period.ends,]   
    bt.run.share(data, clean.signal=F, silent=T)
}


# Sensitivity Analysis based on the bt.improving.trend.following.test()
bt.aaa.sensitivity.test <- function() 
{
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load.packages('quantmod')
	
	tickers = spl('SPY,EFA,EWJ,EEM,IYR,RWX,IEF,TLT,DBC,GLD')

	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)							
	bt.prep(data, align='keep.all', dates='2004:12::')
 
   
    #*****************************************************************
    # Code Strategies
    #******************************************************************
    prices = data$prices  
    n = ncol(prices)
   
    models = list()
   
    # find period ends
    period.ends = endpoints(prices, 'months')
        period.ends = period.ends[period.ends > 0]

        
	#*****************************************************************
	# Test
	#****************************************************************** 
	models = list()
	
	models$combo = bt.aaa.combo(data, period.ends, n.top = 5,
					n.mom = 180, n.vol = 20)
					
					
	models$aaa = bt.aaa.minrisk(data, period.ends, n.top = 5,
					n.mom = 180, n.vol = 20)
					
	plotbt.custom.report.part1(models) 

	        
        
	#*****************************************************************
	# Sensitivity Analysis: bt.aaa.combo / bt.aaa.minrisk
	#****************************************************************** 
	# length of momentum look back
	mom.lens = ( 1 : 12 ) * 20
	# length of volatility look back
	vol.lens = ( 1 : 12 ) * 20

	
	models = list()
	
	# evaluate strategies
	for(n.mom in mom.lens) {
		cat('MOM =', n.mom, '\n')
		
		for(n.vol in vol.lens) {
			cat('\tVOL =', n.vol, '\n')

			models[[ paste('M', n.mom, 'V', n.vol) ]] = 
				bt.aaa.combo(data, period.ends, n.top = 5,
					n.mom = n.mom, n.vol = n.vol)
		}
	}
	
	out = plotbt.strategy.sidebyside(models, return.table=T, make.plot = F)
	
	#*****************************************************************
	# Create Report
	#****************************************************************** 
	# allocate matrixe to store backtest results
	dummy = matrix('', len(vol.lens), len(mom.lens))
		colnames(dummy) = paste('M', mom.lens)
		rownames(dummy) = paste('V', vol.lens)
		
	names = spl('Sharpe,Cagr,DVR,MaxDD')

png(filename = 'plot1.png', width = 1000, height = 1000, units = 'px', pointsize = 12, bg = 'white')										
		
	layout(matrix(1:4,nrow=2))	
	for(i in names) {
		dummy[] = ''
		
		for(n.mom in mom.lens)
			for(n.vol in vol.lens)
				dummy[paste('V', n.vol), paste('M', n.mom)] =
					out[i, paste('M', n.mom, 'V', n.vol) ]
					
		plot.table(dummy, smain = i, highlight = T, colorbar = F)

	}	
		
dev.off()	
    
	#*****************************************************************
	# Sensitivity Analysis
	#****************************************************************** 	
	# evaluate strategies
	for(n.mom in mom.lens) {
		cat('MOM =', n.mom, '\n')
		
		for(n.vol in vol.lens) {
			cat('\tVOL =', n.vol, '\n')

			models[[ paste('M', n.mom, 'V', n.vol) ]] = 
				bt.aaa.minrisk(data, period.ends, n.top = 5,
					n.mom = n.mom, n.vol = n.vol)
		}
	}
	
	out = plotbt.strategy.sidebyside(models, return.table=T, make.plot = F)
	
	#*****************************************************************
	# Create Report
	#****************************************************************** 
png(filename = 'plot2.png', width = 1000, height = 1000, units = 'px', pointsize = 12, bg = 'white')										
		
	layout(matrix(1:4,nrow=2))	
	for(i in names) {
		dummy[] = ''
		
		for(n.mom in mom.lens)
			for(n.vol in vol.lens)
				dummy[paste('V', n.vol), paste('M', n.mom)] =
					out[i, paste('M', n.mom, 'V', n.vol) ]
					
		plot.table(dummy, smain = i, highlight = T, colorbar = F)

	}	
dev.off()	
    

    
}


bt.aaa.test <- function() 
{
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load.packages('quantmod')
	
	tickers = spl('SPY,EFA,EWJ,EEM,IYR,RWX,IEF,TLT,DBC,GLD')

	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
	
	
	# contruct another back-test enviroment with split-adjusted prices, do not include dividends
	# http://www.fintools.com/wp-content/uploads/2012/02/DividendAdjustedStockPrices.pdf
	# http://www.pstat.ucsb.edu/research/papers/momentum.pdf
	data.price <- new.env()
		for(i in ls(data)) data.price[[i]] = adjustOHLC(data[[i]], symbol.name=i, adjust='split', use.Adjusted=F)
	bt.prep(data.price, align='keep.all', dates='2004:12::')	
	
	
	# create split and dividend adjusted prices
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)							
	bt.prep(data, align='keep.all', dates='2004:12::')
 
	
	# flag to indicate whether to use Total(split and dividend adjusted) or Price(split adjusted) prices
	use.total = FALSE
	
	
	#*****************************************************************
    # Sample Plot of Total and Price only time series
    #******************************************************************	 
    if(F) {
		y = data$prices$TLT
		y.price = data.price$prices$TLT
			y = y / as.double(y[1])
			y.price = y.price / as.double(y.price[1])
			
		plota(y, type='l', ylim=range(y, y.price, na.rm=T))
			plota.lines(y.price, col='red')
		plota.legend('Total,Price', 'black,red')
	}			
		
   
    #*****************************************************************
    # Code Strategies
    #******************************************************************
    prices = data$prices      
    n = ncol(prices)
    
	prices4mom = iif(use.total, data$prices, data.price$prices)
	prices4vol = iif(use.total, data$prices, data.price$prices)    
   
    models = list()
   
    # find period ends
    period.ends = endpoints(prices, 'months')
        period.ends = period.ends[period.ends > 0]

	# Adaptive Asset Allocation parameters
	n.top = 5		# number of momentum positions
	n.mom = 6*22	# length of momentum look back
	n.vol = 1*22 	# length of volatility look back
        
    #*****************************************************************
    # Equal Weight
    #******************************************************************
    data$weight[] = NA
        data$weight[period.ends,] = ntop(prices[period.ends,], n)   
    models$equal.weight = bt.run.share(data, clean.signal=F)

    #*****************************************************************
    # Volatliliy Position Sizing
    #******************************************************************
    ret.log = bt.apply.matrix(prices4vol, ROC, type='continuous')
    hist.vol = bt.apply.matrix(ret.log, runSD, n = n.vol)
   
    adj.vol = 1/hist.vol[period.ends,]
           
    data$weight[] = NA
        data$weight[period.ends,] = adj.vol / rowSums(adj.vol, na.rm=T)    
    models$volatility.weighted = bt.run.share(data, clean.signal=F)
   
    #*****************************************************************
    # Momentum Portfolio
    #*****************************************************************
    momentum = prices4mom / mlag(prices4mom, n.mom)
   
    data$weight[] = NA
        data$weight[period.ends,] = ntop(momentum[period.ends,], n.top)   
    models$momentum = bt.run.share(data, clean.signal=F)
       
    #*****************************************************************
    # Combo: weight positions in the Momentum Portfolio according to Volatliliy
    #*****************************************************************
    weight = ntop(momentum[period.ends,], n.top) * adj.vol
   
    data$weight[] = NA
        data$weight[period.ends,] = weight / rowSums(weight, na.rm=T)   
    models$combo = bt.run.share(data, clean.signal=F,trade.summary = TRUE)

    #*****************************************************************   
    # Adaptive Asset Allocation (AAA)
    # weight positions in the Momentum Portfolio according to 
    # the minimum variance algorithm
    #*****************************************************************   
    weight = NA * prices
        weight[period.ends,] = ntop(momentum[period.ends,], n.top)
       
    for( i in period.ends[period.ends >= n.mom] ) {
    	hist = ret.log[ (i - n.vol + 1):i, ]
    	
		# require all assets to have full price history
		include.index = count(hist)== n.vol      

		# also only consider assets in the Momentum Portfolio
        index = ( weight[i,] > 0 ) & include.index
        n = sum(index)
        
		if(n > 0) {					
			hist = hist[ , index]
        
	        # create historical input assumptions
	        ia = create.historical.ia(hist, 252)
	            s0 = apply(coredata(hist),2,sd)       
	            ia$cov = cor(coredata(hist), use='complete.obs',method='pearson') * (s0 %*% t(s0))
	       
			# create constraints: 0<=x<=1, sum(x) = 1
			constraints = new.constraints(n, lb = 0, ub = 1)
			constraints = add.constraints(rep(1, n), 1, type = '=', constraints)       
			
			# compute minimum variance weights				            
	        weight[i,] = 0        
	        weight[i,index] = min.risk.portfolio(ia, constraints)
        }
    }

    # Adaptive Asset Allocation (AAA)
    data$weight[] = NA
        data$weight[period.ends,] = weight[period.ends,]   
    models$aaa = bt.run.share(data, clean.signal=F,trade.summary = TRUE)
       
    
    #*****************************************************************
    # Create Report
    #******************************************************************    
    #pdf(file = 'report.pdf', width=8.5, height=11)
   
    models = rev(models)
   
png(filename = 'plot1.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')		    
    plotbt.custom.report.part1(models)       
dev.off()		

png(filename = 'plot2.png', width = 1200, height = 800, units = 'px', pointsize = 12, bg = 'white')		               
    plotbt.custom.report.part2(models)       
dev.off()		

png(filename = 'plot3.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')		    
	plotbt.custom.report.part3(models$combo, trade.summary = TRUE)       
dev.off()		
       
png(filename = 'plot4.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')		    
    plotbt.custom.report.part3(models$aaa, trade.summary = TRUE)       
dev.off()               


}
	

bt.aaa.test.new <- function() 
{
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load.packages('quantmod')
	
	tickers = spl('SPY,EFA,EWJ,EEM,IYR,RWX,IEF,TLT,DBC,GLD')

	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)							
	bt.prep(data, align='keep.all', dates='2004:12::')
 
   
    #*****************************************************************
    # Code Strategies
    #******************************************************************
    prices = data$prices  
    n = ncol(prices)
   
    models = list()
   
 	#*****************************************************************
    # Code Strategies
    #******************************************************************
    # find period ends
    period.ends = endpoints(prices, 'months')
    #period.ends = endpoints(prices, 'weeks')
        period.ends = period.ends[period.ends > 0]

        
n.mom = 180
n.vol = 60
n.top = 4
        
	momentum = prices / mlag(prices, n.mom)         
        
	models$combo = bt.aaa.combo(data, period.ends, n.top = n.top,
					n.mom = n.mom, n.vol = n.vol)
					
	# bt.aaa.minrisk is equivalent to MV=min.var.portfolio below
	models$aaa = bt.aaa.minrisk(data, period.ends, n.top = n.top,
					n.mom = n.mom, n.vol = n.vol)
			
					
	obj = portfolio.allocation.helper(data$prices, period.ends=period.ends,
		lookback.len = n.vol, universe = ntop(momentum[period.ends,], n.top) > 0,
		min.risk.fns = list(EW=equal.weight.portfolio,
						RP=risk.parity.portfolio,
						MV=min.var.portfolio,
						MD=max.div.portfolio,
						MC=min.corr.portfolio,
						MC2=min.corr2.portfolio,
						MS=max.sharpe.portfolio())
	) 
	
	models = c(models, create.strategies(obj, data)$models)
					
    #*****************************************************************
    # Create Report
    #******************************************************************    
    # put all reports into one pdf file
	pdf(file = 'filename.pdf', width=8.5, height=11)

		strategy.performance.snapshoot(models)
		
	# close pdf file
    dev.off()	
}


###############################################################################
# Merging Current Stock Quotes with Historical Prices
###############################################################################
bt.current.quote.test <- function() 
{
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load.packages('quantmod')
	
	tickers = spl('VTI,EFA,SHY')	

	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)							
	bt.prep(data)
 
   
    # look at the data
	last(data$prices, 2)

	
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load.packages('quantmod')
	
	tickers = spl('VTI,EFA,SHY')	

	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)							
		
		# current quotes logic
		quotes = getQuote(tickers)
		for(i in ls(data))
			if( last(index(data[[i]])) < as.Date(quotes[i, 'Trade Time']) ) {
				data[[i]] = rbind( data[[i]], make.xts(quotes[i, spl('Open,High,Low,Last,Volume,Last')],
					as.Date(quotes[i, 'Trade Time'])))
			}

	bt.prep(data)
 
	
    # look at the data
	last(data$prices, 2)
}


###############################################################################
# Extending Commodity  time series
# with CRB Commodities Index 
# http://www.jefferies.com/cositemgr.pl/html/ProductsServices/SalesTrading/Commodities/ReutersJefferiesCRB/IndexData/index.shtml
###############################################################################
bt.extend.DBC.test <- function() 
{
	#*****************************************************************
	# Load historical data
	#****************************************************************** 	
	load.packages('quantmod')		
	CRB = get.CRB()
		
	tickers = spl('GSG,DBC')		
	getSymbols(tickers, src = 'yahoo', from = '1970-01-01')
	
	#*****************************************************************
	# Compare different indexes
	#****************************************************************** 	
	out = na.omit(merge(Ad(CRB), Ad(GSG), Ad(DBC)))
		colnames(out) = spl('CRB,GSG,DBC')
	temp = out / t(repmat(as.vector(out[1,]),1,nrow(out)))
		
png(filename = 'plot1.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')		    	
	# Plot side by side
	layout(1:2, heights=c(4,1))
	plota(temp, ylim=range(temp))
		plota.lines(temp[,1],col=1)
		plota.lines(temp[,2],col=2)
		plota.lines(temp[,3],col=3)
	plota.legend(colnames(temp),1:3)
			
	# Plot correlation table	
	temp = compute.cor(temp / mlag(temp)- 1, 'pearson')
			temp[] = plota.format(100 * temp, 0, '', '%')
	plot.table(temp)	
dev.off()		
	
	
	#*****************************************************************
	# Create simple equal weight back-test
	#****************************************************************** 
	tickers = spl('GLD,DBC,TLT')

	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
		
		# extend Gold and Commodity time series
		data$GLD = extend.GLD(data$GLD)	
		data$DBC = extend.data(data$DBC, get.CRB(), scale=T)
			
	bt.prep(data, align='remove.na')
 
    #*****************************************************************
    # Code Strategies
    #******************************************************************
    prices = data$prices      
    n = ncol(prices)
  
    # find period ends
    period.ends = endpoints(prices, 'months')
        period.ends = period.ends[period.ends > 0]
        
    models = list()
   
    #*****************************************************************
    # Equal Weight
    #******************************************************************
    data$weight[] = NA
        data$weight[period.ends,] = ntop(prices[period.ends,], n)   
    models$equal.weight = bt.run.share(data, clean.signal=F)

    
    #*****************************************************************
    # Create Report
    #******************************************************************       
png(filename = 'plot2.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')		    
    plotbt.custom.report.part1(models)       
dev.off()		

png(filename = 'plot3.png', width = 1200, height = 800, units = 'px', pointsize = 12, bg = 'white')		               
    plotbt.custom.report.part2(models)       
dev.off()	
}

###############################################################################
# Extending Gold time series
# http://wikiposit.org/w?filter=Finance/Commodities/
# http://www.hardassetsinvestor.com/interviews/2091-golds-paper-price.html
###############################################################################
bt.extend.GLD.test <- function() 
{
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load.packages('quantmod')	
	GLD = getSymbols('GLD', src = 'yahoo', from = '1970-01-01', auto.assign = F)			
		GLD = adjustOHLC(GLD, use.Adjusted=T)
		
	# get Gold.PM - London Gold afternoon fixing prices
	temp = read.csv('http://wikiposit.org/w?action=dl&dltypes=comma%20separated&sp=daily&uid=KITCO',skip=4,header=TRUE, stringsAsFactors=F)
	Gold.PM = make.xts(as.double(temp$Gold.PM) / 10, as.Date(temp$Date, '%d-%b-%Y'))
		Gold.PM = Gold.PM[ !is.na(Gold.PM), ]

	# merge GLD and Gold.PM
	data <- new.env()
		data$GLD = GLD
		data$Gold.PM = Gold.PM
	bt.prep(data, align='remove.na')
	

	#*****************************************************************
	# Plot GLD and Gold.PM
	#****************************************************************** 
png(filename = 'plot1.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')		    
	
	layout(1:2)
	plota(data$GLD, type='l', col='black', plotX=F)	
		plota.lines(data$Gold.PM, col='blue')	
	plota.legend('GLD,Gold.PM', 'black,blue', list(data$GLD, data$Gold.PM))
		
	# plot GLD and London afternoon gold fix spread
	spread = 100 * (Cl(data$GLD) - data$Gold.PM) / data$Gold.PM
	plota(spread , type='l', col='black')	
		plota.legend('GLD vs Gold.PM % spread', 'black', spread)
	
dev.off()		
	
		
	#*****************************************************************
	# Look at Silver
	#****************************************************************** 
	SLV = getSymbols('SLV', src = 'yahoo', from = '1970-01-01', auto.assign = F)
		SLV = adjustOHLC(SLV, use.Adjusted=T)
	Silver = make.xts(as.double(temp$Silver), as.Date(temp$Date, '%d-%b-%Y'))
		Silver = Silver[ !is.na(Silver), ]

	data <- new.env()
		data$SLV = SLV
		data$Silver = Silver
	bt.prep(data, align='remove.na')
		
		
png(filename = 'plot2.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')		    
	
	layout(1:2)
	plota(data$SLV, type='l', col='black')	
		plota.lines(data$Silver, col='blue')	
	plota.legend('SLV,Silver', 'black,blue', list(data$SLV, data$Silver))

	spread = 100*(Cl(data$SLV) - data$Silver) / data$Silver
	plota(spread , type='l', col='black')	
		plota.legend('SLV vs Silver % spread', 'black', spread)
	
dev.off()
		
	#*****************************************************************
	# Create simple equal weight back-test
	#****************************************************************** 
	tickers = spl('GLD,TLT')

	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
		
		# extend GLD with Gold.PM - London Gold afternoon fixing prices
		data$GLD = extend.GLD(data$GLD)
		
	bt.prep(data, align='remove.na')
 
    #*****************************************************************
    # Code Strategies
    #******************************************************************
    prices = data$prices      
    n = ncol(prices)
  
    # find period ends
    period.ends = endpoints(prices, 'months')
        period.ends = period.ends[period.ends > 0]
        
    models = list()
   
    #*****************************************************************
    # Equal Weight
    #******************************************************************
    data$weight[] = NA
        data$weight[period.ends,] = ntop(prices[period.ends,], n)   
    models$equal.weight = bt.run.share(data, clean.signal=F)

    
    #*****************************************************************
    # Create Report
    #******************************************************************       
png(filename = 'plot3.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')		    
    plotbt.custom.report.part1(models)       
dev.off()		

png(filename = 'plot4.png', width = 1200, height = 800, units = 'px', pointsize = 12, bg = 'white')		               
    plotbt.custom.report.part2(models)       
dev.off()		

				
}




###############################################################################
# Permanent Portfolio
# http://catallacticanalysis.com/permanent-portfolio/
# http://systematicinvestor.wordpress.com/2011/12/16/backtesting-rebalancing-methods/
# http://en.wikipedia.org/wiki/Fail-Safe_Investing#The_Permanent_Portfolio
###############################################################################
bt.permanent.portfolio.test <- function() 
{
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load.packages('quantmod')	
	tickers = spl('SPY,TLT,GLD,SHY')
		
	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
		
		# extend GLD with Gold.PM - London Gold afternoon fixing prices
		data$GLD = extend.GLD(data$GLD)
	
	bt.prep(data, align='remove.na')

	#*****************************************************************
	# Setup
	#****************************************************************** 		
	prices = data$prices   
		n = ncol(prices)
		nperiods = nrow(prices)

	# annual
	period.ends = endpoints(prices, 'years')
		period.ends = period.ends[period.ends > 0]		
		period.ends.y = c(1, period.ends)

	# quarterly
	period.ends = endpoints(prices, 'quarters')
		period.ends = period.ends[period.ends > 0]		
		period.ends.q = c(1, period.ends)
					

	models = list()
	
	#*****************************************************************
	# Code Strategies
	#****************************************************************** 	
	target.allocation = matrix(rep(1/n,n), nrow=1)
	
	# Buy & Hold	
	data$weight[] = NA	
		data$weight[period.ends.y[1],] = target.allocation
	models$buy.hold = bt.run.share(data, clean.signal=F)

		
	# Equal Weight
	data$weight[] = NA
		data$weight[period.ends.y,] = ntop(prices[period.ends.y,], n)
	models$equal.weight.y = bt.run.share(data, clean.signal=F)

	# Rebalance only when threshold is broken
	models$threshold.y = bt.max.deviation.rebalancing(data, models$buy.hold, target.allocation, 10/100, 0, period.ends = period.ends.y) 

	#*****************************************************************
	# Quarterly
	#****************************************************************** 	
	# Equal Weight
	data$weight[] = NA
		data$weight[period.ends.q,] = ntop(prices[period.ends.q,], n)
	models$equal.weight.q = bt.run.share(data, clean.signal=F)

	# Rebalance only when threshold is broken
	models$threshold.q = bt.max.deviation.rebalancing(data, models$buy.hold, target.allocation, 10/100, 0, period.ends = period.ends.q) 
	
		
    #*****************************************************************
    # Create Report
    #******************************************************************       
png(filename = 'plot1.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')		    
    plotbt.custom.report.part1(models)       
dev.off()					
	
png(filename = 'plot2.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')		    
    plotbt.strategy.sidebyside(models)
dev.off()			
	
	
png(filename = 'plot3.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')		    
	# Plot Portfolio Turnover for each Rebalancing method
	layout(1:2)
	barplot.with.labels(sapply(models, compute.turnover, data), 'Average Annual Portfolio Turnover', F)
	barplot.with.labels(sapply(models, compute.max.deviation, target.allocation), 'Maximum Deviation from Target Mix')
dev.off()			
	
}
	



###############################################################################
# Additional example for Permanent Portfolio
# that employs:
# * risk allocation
# * volatility targeting
# * makret filter (10 month SMA)
# to improve strategy perfromance
###############################################################################
bt.permanent.portfolio2.test <- function() 
{
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load.packages('quantmod')	
	tickers = spl('SPY,TLT,GLD,SHY')
		
	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
		
		# extend GLD with Gold.PM - London Gold afternoon fixing prices
		data$GLD = extend.GLD(data$GLD)
	
	bt.prep(data, align='remove.na')

	#*****************************************************************
	# Setup
	#****************************************************************** 		
	prices = data$prices   
		n = ncol(prices)

	period.ends = endpoints(prices, 'quarters')
		period.ends = period.ends[period.ends > 0]		
		period.ends = c(1, period.ends)
					

	models = list()
	
	
	#*****************************************************************
	# Dollar Weighted
	#****************************************************************** 			
	target.allocation = matrix(rep(1/n,n), nrow=1)
	weight.dollar = ntop(prices, n)
	
	data$weight[] = NA
		data$weight[period.ends,] = weight.dollar[period.ends,]
	models$dollar = bt.run.share(data, clean.signal=F)
				
	#*****************************************************************
	# Dollar Weighted + 7% target volatility
	#****************************************************************** 				
	data$weight[] = NA
		data$weight[period.ends,] = target.vol.strategy(models$dollar,
						weight.dollar, 7/100, 21, 100/100)[period.ends,]
	models$dollar.target7 = bt.run.share(data, clean.signal=F)
	
	#*****************************************************************
	# Risk Weighted
	#****************************************************************** 				
	ret.log = bt.apply.matrix(prices, ROC, type='continuous')
	hist.vol = sqrt(252) * bt.apply.matrix(ret.log, runSD, n = 21)	
	weight.risk = weight.dollar / hist.vol
		weight.risk = weight.risk / rowSums(weight.risk)
		
	data$weight[] = NA
		data$weight[period.ends,] = weight.risk[period.ends,]
	models$risk = bt.run.share(data, clean.signal=F)

	if(F) {
		# risk weighted + 7% target volatility
		data$weight[] = NA
			data$weight[period.ends,] = target.vol.strategy(models$risk,
							weight.risk, 7/100, 21, 100/100)[period.ends,]
		models$risk.target7 = bt.run.share(data, clean.signal=F)
	
		# risk weighted + 5% target volatility
		data$weight[] = NA
			data$weight[period.ends,] = target.vol.strategy(models$risk,
							weight.risk, 5/100, 21, 100/100)[period.ends,]
		models$risk.target5 = bt.run.share(data, clean.signal=F)
	}	
	#*****************************************************************
	# Market Filter (tactical): 10 month moving average
	#****************************************************************** 				
	period.ends = endpoints(prices, 'months')
		period.ends = period.ends[period.ends > 0]		
		period.ends = c(1, period.ends)

	sma = bt.apply.matrix(prices, SMA, 200)
	weight.dollar.tactical = weight.dollar * (prices > sma)	
	
	data$weight[] = NA
		data$weight[period.ends,] = weight.dollar.tactical[period.ends,]
	models$dollar.tactical = bt.run.share(data, clean.signal=F)

	#*****************************************************************
	# Tactical + 7% target volatility
	#****************************************************************** 				
	data$weight[] = NA
		data$weight[period.ends,] = target.vol.strategy(models$dollar.tactical,
						weight.dollar.tactical, 7/100, 21, 100/100)[period.ends,]
	models$dollar.tactical.target7 = bt.run.share(data, clean.signal=F)
		
			
    #*****************************************************************
    # Create Report
    #******************************************************************       
png(filename = 'plot1.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')		    
    plotbt.custom.report.part1(models)       
dev.off()					
	
png(filename = 'plot2.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')		    
    plotbt.strategy.sidebyside(models)
dev.off()	    
	
}




###############################################################################
# Additional example for Permanent Portfolio
# add transaction cost and 
# RR - remove SHY from basket
###############################################################################
bt.permanent.portfolio3.test <- function() 
{
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load.packages('quantmod')	
	tickers = spl('SPY,TLT,GLD,SHY')
		
	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
		
		# extend GLD with Gold.PM - London Gold afternoon fixing prices
		data$GLD = extend.GLD(data$GLD)
	
	bt.prep(data, align='remove.na')

	#*****************************************************************
	# Setup
	#****************************************************************** 		
	prices = data$prices   
		n = ncol(prices)

	period.ends = endpoints(prices, 'months')
		period.ends = period.ends[period.ends > 0]		
		period.ends = c(1, period.ends)
					

	models = list()
	commission = 0.1
	
	#*****************************************************************
	# Dollar Weighted
	#****************************************************************** 			
	target.allocation = matrix(rep(1/n,n), nrow=1)
	weight.dollar = ntop(prices, n)
	
	data$weight[] = NA
		data$weight[period.ends,] = weight.dollar[period.ends,]
	models$dollar = bt.run.share(data, commission=commission, clean.signal=F)
				
	#*****************************************************************
	# Dollar Weighted + 7% target volatility
	#****************************************************************** 				
	data$weight[] = NA
		data$weight[period.ends,] = target.vol.strategy(models$dollar,
						weight.dollar, 7/100, 21, 100/100)[period.ends,]
	models$dollar.target7 = bt.run.share(data, commission=commission, clean.signal=F)
	
	#*****************************************************************
	# Risk Weighted
	#****************************************************************** 				
	ret.log = bt.apply.matrix(prices, ROC, type='continuous')
	hist.vol = sqrt(252) * bt.apply.matrix(ret.log, runSD, n = 21)	
	weight.risk = weight.dollar / hist.vol
		weight.risk$SHY = 0 
		weight.risk = weight.risk / rowSums(weight.risk)
		
	data$weight[] = NA
		data$weight[period.ends,] = weight.risk[period.ends,]
	models$risk = bt.run.share(data, commission=commission, clean.signal=F)

	#*****************************************************************
	# Risk Weighted + 7% target volatility
	#****************************************************************** 				
	data$weight[] = NA
		data$weight[period.ends,] = target.vol.strategy(models$risk,
						weight.risk, 7/100, 21, 100/100)[period.ends,]
	models$risk.target7 = bt.run.share(data, commission=commission, clean.signal=F)

	#*****************************************************************
	# Risk Weighted + 7% target volatility + SHY
	#****************************************************************** 				
	data$weight[] = NA
		data$weight[period.ends,] = target.vol.strategy(models$risk,
						weight.risk, 7/100, 21, 100/100)[period.ends,]
						
  		cash = 1-rowSums(data$weight)
	    data$weight$SHY[period.ends,] = cash[period.ends]
	models$risk.target7.shy = bt.run.share(data, commission=commission, clean.signal=F)
	
	
	

    #*****************************************************************
    # Create Report
    #******************************************************************       
png(filename = 'plot1.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')		    
    plotbt.custom.report.part1(models)       
dev.off()					
	
png(filename = 'plot2.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')		    
    plotbt.strategy.sidebyside(models)
dev.off()	    

png(filename = 'plot3.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')		    	
	# Plot Portfolio Turnover for each strategy
	layout(1)
	barplot.with.labels(sapply(models, compute.turnover, data), 'Average Annual Portfolio Turnover')
dev.off()	   	
	
	
	
	
	
	
	#*****************************************************************
	# Market Filter (tactical): 10 month moving average
	#****************************************************************** 				
	sma = bt.apply.matrix(prices, SMA, 200)
	weight.dollar.tactical = weight.dollar * (prices > sma)	
	
	data$weight[] = NA
		data$weight[period.ends,] = weight.dollar.tactical[period.ends,]
	models$dollar.tactical = bt.run.share(data, commission=commission, clean.signal=F)

	#*****************************************************************
	# Tactical + 7% target volatility
	#****************************************************************** 				
	data$weight[] = NA
		data$weight[period.ends,] = target.vol.strategy(models$dollar.tactical,
						weight.dollar.tactical, 7/100, 21, 100/100)[period.ends,]
	models$dollar.tactical.target7 = bt.run.share(data, commission=commission, clean.signal=F)
		
		
	
	
	#*****************************************************************
	# Risk Weighted + Tactical 
	#****************************************************************** 				
	weight.risk.tactical = weight.risk * (prices > sma)	
	
	data$weight[] = NA
		data$weight[period.ends,] = weight.risk.tactical[period.ends,]
	models$risk.tactical = bt.run.share(data, commission=commission, clean.signal=F)
	
	#*****************************************************************
	# Risk Weighted + Tactical + 7% target volatility + SHY
	#****************************************************************** 				
	data$weight[] = NA
		data$weight[period.ends,] = target.vol.strategy(models$risk.tactical,
						weight.risk.tactical, 7/100, 21, 100/100)[period.ends,]
  		cash = 1-rowSums(data$weight)
	    data$weight$SHY[period.ends,] = cash[period.ends]						
	models$risk.tactical.target7.shy = bt.run.share(data, commission=commission, clean.signal=F)
	
	
}




###############################################################################
# Minimum Correlation Algorithm Example
###############################################################################
bt.mca.test <- function() 
{

	#*****************************************************************
	# Load historical data for ETFs
	#****************************************************************** 
	load.packages('quantmod,quadprog')
	tickers = spl('SPY,QQQ,EEM,IWM,EFA,TLT,IYR,GLD')

	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)							
	bt.prep(data, align='keep.all', dates='2002:08::')
	
	#write.xts(data$prices, 'data.csv')
	
	#*****************************************************************
	# Code Strategies
	#****************************************************************** 	
	
	obj = portfolio.allocation.helper(data$prices, periodicity = 'weeks',
		min.risk.fns = list(EW=equal.weight.portfolio,
						RP=risk.parity.portfolio,
						MV=min.var.portfolio,
						MD=max.div.portfolio,
						MC=min.corr.portfolio,
						MC2=min.corr2.portfolio),
		custom.stats.fn = 'portfolio.allocation.custom.stats'
	) 
	
	models = create.strategies(obj, data)$models
	
    #*****************************************************************
    # Create Report
    #******************************************************************       
png(filename = 'plot1.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')   
	layout(1:2)
	plotbt(models, plotX = T, log = 'y', LeftMargin = 3)	    	
		mtext('Cumulative Performance', side = 2, line = 1)
		
	out = plotbt.strategy.sidebyside(models, return.table=T)
dev.off()

png(filename = 'plot2.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')   	
	# Plot time series of components of Composite Diversification Indicator
	cdi = custom.composite.diversification.indicator(obj,plot.table = F)	
		out = rbind(colMeans(cdi, na.rm=T), out)
		rownames(out)[1] = 'Composite Diversification Indicator(CDI)'
dev.off()		
				
	# Portfolio Turnover for each strategy
	y = 100 * sapply(models, compute.turnover, data)
		out = rbind(y, out)
		rownames(out)[1] = 'Portfolio Turnover'		

png(filename = 'plot3.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')   				
	performance.barchart.helper(out, 'Sharpe,Cagr,DVR,MaxDD', c(T,T,T,T))
dev.off()	
	
png(filename = 'plot4.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')   		
	performance.barchart.helper(out, 'Volatility,Portfolio Turnover,Composite Diversification Indicator(CDI)', c(F,F,T))
dev.off()	

	
png(filename = 'plot5.png', width = 600, height = 1000, units = 'px', pointsize = 12, bg = 'white')   			
	# Plot transition maps
	layout(1:len(models))
	for(m in names(models)) {
		plotbt.transition.map(models[[m]]$weight, name=m)
			legend('topright', legend = m, bty = 'n')
	}
dev.off()	
	
png(filename = 'plot6.png', width = 600, height = 1000, units = 'px', pointsize = 12, bg = 'white')   					
	# Plot transition maps for Risk Contributions
	dates = index(data$prices)[obj$period.ends]
	layout(1:len(models))
	for(m in names(models)) {
		plotbt.transition.map(make.xts(obj$risk.contributions[[m]], dates), 
		name=paste('Risk Contributions',m))
			legend('topright', legend = m, bty = 'n')
	}
dev.off()	
	
	
}	

	
###############################################################################
# Minimum Correlation Algorithm Speed test
###############################################################################
bt.mca.speed.test <- function() 
{
	#*****************************************************************
	# Setup
	#*****************************************************************
	load.packages('quadprog,corpcor')
	
	n = 100
	hist = matrix(rnorm(1000*n), nc=n)
	
	# 0 <= x.i <= 1
	constraints = new.constraints(n, lb = 0, ub = 1)
		constraints = add.constraints(diag(n), type='>=', b=0, constraints)
		constraints = add.constraints(diag(n), type='<=', b=1, constraints)

	# SUM x.i = 1
	constraints = add.constraints(rep(1, n), 1, type = '=', constraints)		
						
	# create historical input assumptions
	ia = list()
		ia$n = n
		ia$risk = apply(hist, 2, sd)
		ia$correlation = cor(hist, use='complete.obs', method='pearson')
		ia$cov = ia$correlation * (ia$risk %*% t(ia$risk))
				
		ia$cov = make.positive.definite(ia$cov, 0.000000001)
		ia$correlation = make.positive.definite(ia$correlation, 0.000000001)
		
	#*****************************************************************
	# Time
	#*****************************************************************				
	load.packages('rbenchmark')			

	benchmark(
		min.var.portfolio(ia, constraints),
		min.corr.portfolio(ia, constraints),
		min.corr2.portfolio(ia, constraints),
		
		
	columns=c("test", "replications", "elapsed", "relative"),
	order="relative",
	replications=100
	)
	
	
	#*****************************************************************
	# Check the bottle neck
	#*****************************************************************				
	Rprof()
	for(i in 1:10)
		min.corr.portfolio(ia, constraints)
	Rprof(NULL)
	summaryRprof()

	
				
	#ia$cov = make.positive.definite.fast(ia$cov)
	#ia$correlation = make.positive.definite.fast(ia$correlation)
		
}	


###############################################################################
# Testing Universal Portfolios - Constant Rebalanced portfolio
# http://optimallog.blogspot.ca/2012/06/universal-portfolio-part-3.html
# http://optimallog.blogspot.ca/2012/06/universal-portfolio-part-4.html
# to call internal function in logopt use logopt:::crp_bh(x) or logopt:::roll.bcrp
###############################################################################
bt.crp.test <- function() 
{
	#*****************************************************************
	# Example from http://optimallog.blogspot.ca/2012/06/universal-portfolio-part-3.html
	#****************************************************************** 
	load.packages('FNN')
	load.packages('logopt', 'http://R-Forge.R-project.org')
	
	load.packages('quantmod')

	data(nyse.cover.1962.1984)
	x = nyse.cover.1962.1984
	x = x[,spl('iroqu,kinar')]
	
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	data <- new.env()
		for(i in names(x)) {
			data[[i]] = cumprod(x[,i])
			colnames(data[[i]]) = 'Close'
			}
	bt.prep(data, align='remove.na')
	
	
    #*****************************************************************
    # Code Strategies
    #******************************************************************
    prices = data$prices  
	    n = ncol(prices)
	
    #*****************************************************************
    # Plot 1
    #******************************************************************
	plota(prices$iroqu, col='blue', type='l',   ylim=range(prices), main = '"iroqu" and "kinar"', ylab='')
		plota.lines(prices$kinar, col='red')
		grid()
	plota.legend('iroqu,kinar', 'blue,red')	
	
    #*****************************************************************
    # Compute Universal Portfolio
    #******************************************************************	
	universal = prices[,1] * 0	    
	alphas = seq(0,1,by=0.05)
	crps = alphas
	for (i in 1:length(crps)) {
		data$weight[] = NA
			data$weight[] = c(alphas[i], 1-alphas[i])
		equity = bt.run(data, silent=T)$equity
		
		universal = universal + equity
		crps[i] = last(equity)
	}	    
	universal = universal/length(alphas)
	    
    #*****************************************************************
    # Plot 2
    #******************************************************************
	plot(alphas, crps, col="blue", type="l", ylab="",
		main='20 Year Return vs. mix of "iroqu" and "kinar"',
		xlab='Fraction of "iroqu" in Portfolio')
	points(alphas, crps, pch=19, cex=0.5, col="red")
	abline(h=mean(crps), col="green")
	text(0.5,mean(crps)*1.05,labels="Return from Universal Portfolio")
		grid()	

    #*****************************************************************
    # Plot 3
    #******************************************************************
	plota(prices$iroqu, col='blue', type='l',   ylim=range(prices, universal), 
			main = 'Universal Portfolios with "iroqu" and "kinar"', ylab="")
		plota.lines(prices$kinar, col='red')
		plota.lines(universal, col='green')
		grid()
	plota.legend('iroqu,kinar,universal', 'blue,red,green')	
    
	
	
	


	# Constant Rebalanced portfolio
	crp.portfolio <- function
	(
		ia,				# input assumptions
		constraints		# constraints
	)
	{
		bcrp.optim(1 + ia$hist.returns, fast.only = TRUE )
	}

	#*****************************************************************
	# Load historical data for ETFs
	#****************************************************************** 
	load.packages('quantmod,quadprog')
	tickers = spl('SPY,QQQ,EEM,IWM,EFA,TLT,IYR,GLD')

	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)							
	bt.prep(data, align='keep.all', dates='2002:08::')
	
	#*****************************************************************
	# Code Strategies
	#****************************************************************** 	

	obj = portfolio.allocation.helper(data$prices, periodicity = 'weeks', lookback.len = 460, 
		min.risk.fns = list(CRP=crp.portfolio)
	) 
	
	models = create.strategies(obj, data)$models
				
    #*****************************************************************
    # Create Report
    #******************************************************************       
    # quite volatlie for a short lookback.len
    plotbt.custom.report.part2( models$CRP )
    
    
	#*****************************************************************
	# Code Strategies
	#****************************************************************** 	
	
	obj = portfolio.allocation.helper(data$prices, periodicity = 'weeks',
		min.risk.fns = list(EW=equal.weight.portfolio,
						RP=risk.parity.portfolio,
						MC=min.corr.portfolio,
						MC2=min.corr2.portfolio)
	) 
	
	models = c(models, create.strategies(obj, data)$models)
	
    #*****************************************************************
    # Create Report
    #******************************************************************       
	# performance is inferior to other algos
    layout(1:2)
	plotbt(models, plotX = T, log = 'y', LeftMargin = 3)	    	
		mtext('Cumulative Performance', side = 2, line = 1)
		
	out = plotbt.strategy.sidebyside(models, return.table=T)
	
}



###############################################################################
# Interesting that Sep/Nov perfromance changes over different time frames
# http://www.marketwatch.com/story/an-early-halloween-for-gold-traders-2012-09-26
# An early Halloween for gold traders
# Commentary: October is worst month of calendar for gold bullion
# By Mark Hulbert
# Watch out, gold traders: Halloween is likely to come early. 
###############################################################################
bt.october.gold.test <- function() 
{
    #*****************************************************************
    # Load historical data
    #****************************************************************** 
    load.packages('quantmod')
    ticker = 'GLD'
    
    data = getSymbols(ticker, src = 'yahoo', from = '1970-01-01', auto.assign = F)
        data = adjustOHLC(data, use.Adjusted=T)
        
    #*****************************************************************
    # Look at the Month of the Year Seasonality
    #****************************************************************** 
png(filename = 'plot1.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')   
	month.year.seasonality(data, ticker)
dev.off()    
    
    
    
    #*****************************************************************
    # Load long series of gold prices from Bundes Bank
    #****************************************************************** 
    data = bundes.bank.data.gold()

    #*****************************************************************
    # Look at the Month of the Year Seasonality
    #****************************************************************** 
png(filename = 'plot2.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')   
	month.year.seasonality(data, 'GOLD', lookback.len = nrow(data))
dev.off()    


    #*****************************************************************
    # Create file for Seasonality Tool
    #******************************************************************     
    GLD = getSymbols(ticker, src = 'yahoo', from = '1970-01-01', auto.assign = F)
        GLD = adjustOHLC(GLD, use.Adjusted=T)
        

	write.xts(extend.data(GLD, data / 10), 'GOLD.csv')



}    




###############################################################################
# Couch Potato strategy
# http://www.moneysense.ca/2006/04/05/couch-potato-portfolio-introduction/
###############################################################################
	# helper function to model Couch Potato strategy - a fixed allocation strategy
	couch.potato.strategy <- function
	(
		data.all,
		tickers = 'XIC.TO,XSP.TO,XBB.TO',
		weights = c( 1/3, 1/3, 1/3 ), 		
		periodicity = 'years',
		dates = '1900::',
		commission = 0.1
	) 
	{ 
		#*****************************************************************
		# Load historical data 
		#****************************************************************** 
		tickers = spl(tickers)
		names(weights) = tickers
		
		data <- new.env()
		for(s in tickers) data[[ s ]] = data.all[[ s ]]
		
		bt.prep(data, align='remove.na', dates=dates)
	
		#*****************************************************************
		# Code Strategies
		#******************************************************************
		prices = data$prices   
			n = ncol(prices)
			nperiods = nrow(prices)
	
		# find period ends
		period.ends = endpoints(data$prices, periodicity)
			period.ends = c(1, period.ends[period.ends > 0])
	
		#*****************************************************************
		# Code Strategies
		#******************************************************************
		data$weight[] = NA
			for(s in tickers) data$weight[period.ends, s] = weights[s]
		model = bt.run.share(data, clean.signal=F, commission=commission)
		
		return(model)
	} 	

bt.couch.potato.test <- function() 
{
	#*****************************************************************
	# Canadian  Version	
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load.packages('quantmod')	
	map = list()
		map$can.eq = 'XIC.TO'
		map$can.div = 'XDV.TO'		
		map$us.eq = 'XSP.TO'
		map$us.div = 'DVY'			
		map$int.eq = 'XIN.TO'		
		map$can.bond = 'XBB.TO'
		map$can.real.bond = 'XRB.TO'
		map$can.re = 'XRE.TO'		
		map$can.it = 'XTR.TO'
		map$can.gold = 'XGD.TO'
			
	data <- new.env()
	for(s in names(map)) {
		data[[ s ]] = getSymbols(map[[ s ]], src = 'yahoo', from = '1995-01-01', env = data, auto.assign = F)
		data[[ s ]] = adjustOHLC(data[[ s ]], use.Adjusted=T)	
	}
		
	#*****************************************************************
	# Code Strategies
	#****************************************************************** 
	models = list()
		periodicity = 'years'
		dates = '2006::'
	
	models$classic = couch.potato.strategy(data, 'can.eq,us.eq,can.bond', rep(1/3,3), periodicity, dates)
	models$global = couch.potato.strategy(data, 'can.eq,us.eq,int.eq,can.bond', c(0.2, 0.2, 0.2, 0.4), periodicity, dates)
	models$yield = couch.potato.strategy(data, 'can.div,can.it,us.div,can.bond', c(0.25, 0.25, 0.25, 0.25), periodicity, dates)
	models$growth = couch.potato.strategy(data, 'can.eq,us.eq,int.eq,can.bond', c(0.25, 0.25, 0.25, 0.25), periodicity, dates)
	
	models$complete = couch.potato.strategy(data, 'can.eq,us.eq,int.eq,can.re,can.real.bond,can.bond', c(0.2, 0.15, 0.15, 0.1, 0.1, 0.3), periodicity, dates)	
	
	models$permanent = couch.potato.strategy(data, 'can.eq,can.gold,can.bond', c(0.25,0.25,0.5), periodicity, dates)	
		
	#*****************************************************************
	# Create Report
	#****************************************************************** 
png(filename = 'plot1.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')   
	layout(1:2)
	plotbt(models, plotX = T, log = 'y', LeftMargin = 3)	    	
		mtext('Cumulative Performance', side = 2, line = 1)
		
	out = plotbt.strategy.sidebyside(models, return.table=T)
dev.off()
	
	
	
	#*****************************************************************
	# US Version	
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	tickers = spl('VIPSX,VTSMX,VGTSX,SPY,TLT,GLD,SHY')
	
	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '1995-01-01', env = data, auto.assign = T)
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)	
		
		# extend GLD with Gold.PM - London Gold afternoon fixing prices
		data$GLD = extend.GLD(data$GLD)

	#*****************************************************************
	# Code Strategies
	#****************************************************************** 
	models = list()
		periodicity = 'years'
		dates = '2003::'
	
	models$classic = couch.potato.strategy(data, 'VIPSX,VTSMX', rep(1/2,2), periodicity, dates)
	models$margarita = couch.potato.strategy(data, 'VIPSX,VTSMX,VGTSX', rep(1/3,3), periodicity, dates)
	models$permanent = couch.potato.strategy(data, 'SPY,TLT,GLD,SHY', rep(1/4,4), periodicity, dates)
		
	#*****************************************************************
	# Create Report
	#****************************************************************** 
png(filename = 'plot2.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')   
	layout(1:2)
	plotbt(models, plotX = T, log = 'y', LeftMargin = 3)	    	
		mtext('Cumulative Performance', side = 2, line = 1)
		
	out = plotbt.strategy.sidebyside(models, return.table=T)
dev.off()
   	
}



###############################################################################
# Regime Detection
# http://blogs.mathworks.com/pick/2011/02/25/markov-regime-switching-models-in-matlab/
###############################################################################
bt.regime.detection.test <- function() 
{	
	#*****************************************************************
	# Generate data as in the post
	#****************************************************************** 
	bull1 = rnorm( 100, 0.10, 0.15 )
	bear  = rnorm( 100, -0.01, 0.20 )
	bull2 = rnorm( 100, 0.10, 0.15 )
	true.states = c(rep(1,100),rep(2,100),rep(1,100))
	returns = c( bull1, bear,  bull2 )


	# find regimes
	load.packages('RHmm')

	y=returns
	ResFit = HMMFit(y, nStates=2)
	VitPath = viterbi(ResFit, y)
	# HMMGraphicDiag(VitPath, ResFit, y)
	# HMMPlotSerie(y, VitPath)

	#Forward-backward procedure, compute probabilities
	fb = forwardBackward(ResFit, y)

	# Plot probabilities and implied states
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
	layout(1:2)
	plot(VitPath$states, type='s', main='Implied States', xlab='', ylab='State')
	
	matplot(fb$Gamma, type='l', main='Smoothed Probabilities', ylab='Probability')
		legend(x='topright', c('State1','State2'),  fill=1:2, bty='n')
dev.off()	

	
		
	# http://lipas.uwasa.fi/~bepa/Markov.pdf
	# Expected duration of each regime (1/(1-pii))                
	#1/(1-diag(ResFit$HMM$transMat))


	#*****************************************************************
	# Add some data and see if the model is able to identify the regimes
	#****************************************************************** 
	bear2  = rnorm( 100, -0.01, 0.20 )
	bull3 = rnorm( 100, 0.10, 0.10 )
	bear3  = rnorm( 100, -0.01, 0.25 )
	y = c( bull1, bear,  bull2, bear2, bull3, bear3 )
	VitPath = viterbi(ResFit, y)$states

	
	# map states: sometimes HMMFit function does not assign states consistently
	# let's use following formula to rank states
	# i.e. high risk, low returns => state 2 and low risk, high returns => state 1
	map = rank(sqrt(ResFit$HMM$distribution$var) - ResFit$HMM$distribution$mean)
	VitPath = map[VitPath]

	#*****************************************************************
	# Plot regimes
	#****************************************************************** 
	load.packages('quantmod')
	data = xts(y, as.Date(1:len(y)))

png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')	
	layout(1:3)
		plota.control$col.x.highlight = col.add.alpha(true.states+1, 150)
	plota(data, type='h', plotX=F, x.highlight=T)
		plota.legend('Returns + True Regimes')
	plota(cumprod(1+data/100), type='l', plotX=F, x.highlight=T)
		plota.legend('Equity + True Regimes')
	
		plota.control$col.x.highlight = col.add.alpha(VitPath+1, 150)
	plota(data, type='h', x.highlight=T)
		plota.legend('Returns + Detected Regimes')				
dev.off()	

}


###############################################################################
# Regime Detection Pitfalls
###############################################################################
bt.regime.detection.pitfalls.test <- function() 
{	
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load.packages('quantmod')	
	data <- new.env()
	getSymbols('SPY', src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
		data$SPY = adjustOHLC(data$SPY, use.Adjusted=T)							
	bt.prep(data)
	
	#*****************************************************************
	# Setup
	#****************************************************************** 		
	nperiods = nrow(data$prices)

	models = list()
	
	rets = ROC(Ad(data$SPY))
		rets[1] = 0
	
	# use 10 years: 1993:2002 for training	
	in.sample.index = '1993::2002'
	out.sample.index = '2003::'
		
	in.sample = rets[in.sample.index]
	out.sample = rets[out.sample.index]
	out.sample.first.date = nrow(in.sample) + 1

	#*****************************************************************
	# Fit Model
	#****************************************************************** 		
	load.packages('RHmm')	
	fit = HMMFit(in.sample, nStates=2)
	
	# find states
	states.all = rets * NA
	states.all[] = viterbi(fit, rets)$states
			
	#*****************************************************************
	# Code Strategies
	#****************************************************************** 
	data$weight[] = NA
		data$weight[] = iif(states.all == 1, 0, 1)
		data$weight[in.sample.index] = NA
    models$states.all = bt.run.share(data)

png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')										    
	plotbt.custom.report.part1(models) 
dev.off()	
	
	#*****************************************************************
	# Find problem - results are too good
	#****************************************************************** 		
	# The viterbi function need to see all data to compute the most likely sequence of states
	# or forward/backward probabilities
	# http://en.wikipedia.org/wiki/Forward%E2%80%93backward_algorithm
	# http://en.wikipedia.org/wiki/Viterbi_algorithm
	
	# We can use expanding window to determine the states
	states.win1 = states.all * NA
	for(i in out.sample.first.date:nperiods) {
		states.win1[i] = last(viterbi(fit, rets[1:i])$states)
		if( i %% 100 == 0) cat(i, 'out of', nperiods, '\n')
	}
	
	# Or we can refit model over expanding window as suggested in the
	# Regime Shifts: Implications for Dynamic Strategies by M. Kritzman, S. Page, D. Turkington
	# Out-of-Sample Analysis, page 8
	initPoint = fit$HMM
	states.win2 = states.all * NA
	for(i in out.sample.first.date:nperiods) {
		fit2 = HMMFit(rets[2:i], nStates=2, control=list(init='USER', initPoint = initPoint))
			initPoint = fit2$HMM
		states.win2[i] = last(viterbi(fit2, rets[2:i])$states)
		if( i %% 100 == 0) cat(i, 'out of', nperiods, '\n')
	}

	#*****************************************************************
	# Plot States
	#****************************************************************** 			
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')										    	
	layout(1:3)
	col = col.add.alpha('white',210)
	plota(states.all[out.sample.index], type='s', plotX=F)
		plota.legend('Implied States based on all data', x='center', bty='o', bg=col, box.col=col,border=col,fill=col,cex=2)
	plota(states.win1[out.sample.index], type='s')
		plota.legend('Implied States based on rolling window', x='center', bty='o', bg=col, box.col=col,border=col,fill=col,cex=2)
	plota(states.win2[out.sample.index], type='s')
		plota.legend('Implied States based on rolling window(re-fit)', x='center', bty='o', bg=col, box.col=col,border=col,fill=col,cex=2)
dev.off()
		
	#*****************************************************************
	# Code Strategies
	#****************************************************************** 		
	data$weight[] = NA
		data$weight[] = iif(states.win1 == 1, 0, 1)
		data$weight[in.sample.index] = NA
    models$states.win1 = bt.run.share(data)
		
	data$weight[] = NA
		data$weight[] = iif(states.win2 == 1, 0, 1)
		data$weight[in.sample.index] = NA
    models$states.win2 = bt.run.share(data)

	#*****************************************************************
	# Create report
	#****************************************************************** 			
png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')										    		
	plotbt.custom.report.part1(models) 
dev.off()

}	
 	 




###############################################################################
# Financial Turbulence Index example based on the
# Skulls, Financial Turbulence, and Risk Management by M. Kritzman, Y. Li
# http://www.cfapubs.org/doi/abs/10.2469/faj.v66.n5.3
#
# Timely Portfolio series of posts:
# http://timelyportfolio.blogspot.ca/2011/04/great-faj-article-on-statistical.html
# http://timelyportfolio.blogspot.ca/2011/04/great-faj-article-on-statistical_26.html
# http://timelyportfolio.blogspot.ca/2011/04/great-faj-article-on-statistical_6197.html
###############################################################################
bt.financial.turbulence.test <- function() 
{	
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load.packages('quantmod')	

	fx = get.G10()
		nperiods = nrow(fx)

	#*****************************************************************
	# Rolling estimate of the Financial Turbulence for G10 Currencies
	#****************************************************************** 
	turbulence = fx[,1] * NA
	ret = coredata(fx / mlag(fx) - 1)
	
	look.back = 252
	
	for( i in (look.back+1) : nperiods ) {
		temp = ret[(i - look.back + 1):(i-1), ]
				
		# measures turbulence for the current observation
		turbulence[i] = mahalanobis(ret[i,], colMeans(temp), cov(temp))
		
		if( i %% 200 == 0) cat(i, 'out of', nperiods, '\n')
	}	
	
	#*****************************************************************
	# Plot 30 day average of the Financial Turbulence for G10 Currencies
	#****************************************************************** 	
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
	plota(EMA( turbulence, 30), type='l', 
		main='30 day average of the Financial Turbulence for G10 Currencies')
dev.off()	
	
	
}	
	
	

###############################################################################
# Principal component analysis (PCA)
###############################################################################		
bt.pca.test <- function()
{			
	#*****************************************************************
	# Find Sectors for each company in DOW 30
	#****************************************************************** 
	tickers = spl('XLY,XLP,XLE,XLF,XLV,XLI,XLB,XLK,XLU')
	tickers.desc = spl('ConsumerCyclicals,ConsumerStaples,Energy,Financials,HealthCare,Industrials,Materials,Technology,Utilities')
	
	sector.map = c()
	for(i in 1:len(tickers)) {
		sector.map = rbind(sector.map, 
				cbind(sector.spdr.components(tickers[i]), tickers.desc[i])
			)
	}
	colnames(sector.map) = spl('ticker,sector')

	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load.packages('quantmod')	
	tickers = dow.jones.components()
	
	sectors = factor(sector.map[ match(tickers, sector.map[,'ticker']), 'sector'])
		names(sectors) = tickers
	
	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '2000-01-01', env = data, auto.assign = T)
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)	
	
	bt.prep(data, align='keep.all', dates='2012')
	
	# re-order sectors, because bt.prep can change the order of tickers
	sectors = sectors[data$symbolnames]
	
	# save data for later examples
	save(data, tickers, sectors, file='bt.pca.test.Rdata')
	#load(file='bt.pca.test.Rdata')

	#*****************************************************************
	# Principal component analysis (PCA), for interesting discussion
	# http://machine-master.blogspot.ca/2012/08/pca-or-polluting-your-clever-analysis.html
	#****************************************************************** 
	prices = data$prices	
	ret = prices / mlag(prices) - 1
	
	p = princomp(na.omit(ret))
	
	loadings = p$loadings[]
	p.variance.explained = p$sdev^2 / sum(p$sdev^2)

	# plot percentage of variance explained for each principal component	
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')	
	barplot(100*p.variance.explained, las=2, xlab='', ylab='% Variance Explained')
dev.off()
	
	#*****************************************************************
	# 2-D Plot
	#****************************************************************** 		
	x = loadings[,1]
	y = loadings[,2]
	z = loadings[,3]
	cols = as.double(sectors)
	
	# plot all companies loadings on the first and second principal components and highlight points according to the sector they belong
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')		
	plot(x, y, type='p', pch=20, col=cols, xlab='Comp.1', ylab='Comp.2')
	text(x, y, data$symbolnames, col=cols, cex=.8, pos=4)
	
	legend('topright', cex=.8,  legend = levels(sectors), fill = 1:nlevels(sectors), merge = F, bty = 'n') 
dev.off()

	#*****************************************************************
	# 3-D Plot, for good examples of 3D plots
	# http://statmethods.wordpress.com/2012/01/30/getting-fancy-with-3-d-scatterplots/
	#****************************************************************** 				
	load.packages('scatterplot3d') 
	
	# plot all companies loadings on the first, second, and third principal components and highlight points according to the sector they belong
png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')			
	s3d = scatterplot3d(x, y, z, xlab='Comp.1', ylab='Comp.2', zlab='Comp.3', color=cols, pch = 20)
		
    s3d.coords = s3d$xyz.convert(x, y, z)
    text(s3d.coords$x, s3d.coords$y, labels=data$symbolnames, col=cols, cex=.8, pos=4)
		
    legend('topleft', cex=.8,  legend = levels(sectors), fill = 1:nlevels(sectors), merge = F, bty = 'n') 
dev.off()

	#*****************************************************************
	# Next steps          
	#*****************************************************************
    # - demonstrate clustering based on the selected Principal components
    # - using PCA for spread trading 
    # http://matlab-trading.blogspot.ca/2012/12/using-pca-for-spread-trading.html
}    
	
###############################################################################
# Link between svd and eigen
# https://stat.ethz.ch/pipermail/r-help/2001-September/014982.html
# http://r.789695.n4.nabble.com/eigen-and-svd-td2550210.html
# X is a matrix of de-mean returns, cov(X) = (t(x) %*% x) / T
# (svd) X = U D V'   ## D are the singular values of X
# (eigen) X'X = V D^2 V'  ## D^2 are the eigenvalues of X'X
# V is the same in both factorizations. 
###############################################################################


###############################################################################
# The "Absorption Ratio" as defined in the "Principal Components as a Measure of Systemic Risk" 
# by M. Kritzman,Y. Li, S. Page, R. Rigobon paper
# http://papers.ssrn.com/sol3/papers.cfm?abstract_id=1633027
#
# The "Absorption Ratio" is define as the fraction of the total variance explained or absorbed by 
# a finite set of eigenvectors. Let’s, for example, compute the "Absorption Ratio" using 
# the first 3 eigenvectors.
# sum( p$sdev[1:3]^2 ) / sum( sd(na.omit(ret))^2 )
###############################################################################




###############################################################################
# Clustering based on the selected Principal components
###############################################################################		
bt.clustering.test <- function()
{		
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load.packages('quantmod')	
	
	# load data saved in the bt.pca.test() function
	load(file='bt.pca.test.Rdata')

	#*****************************************************************
	# Principal component analysis (PCA), for interesting discussion
	# http://machine-master.blogspot.ca/2012/08/pca-or-polluting-your-clever-analysis.html
	#****************************************************************** 
	prices = data$prices	
	ret = prices / mlag(prices) - 1
	
	p = princomp(na.omit(ret))
	
	loadings = p$loadings[]
	
	x = loadings[,1]
	y = loadings[,2]
	z = loadings[,3]	
	    
	#*****************************************************************
	# Create clusters
	#****************************************************************** 		
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')				
	# create and plot clusters based on the first and second principal components
	hc = hclust(dist(cbind(x,y)), method = 'ward')
	plot(hc, axes=F,xlab='', ylab='',sub ='', main='Comp 1/2')
	rect.hclust(hc, k=3, border='red')
dev.off()

png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
	# create and plot clusters based on the first, second, and third principal components
	hc = hclust(dist(cbind(x,y,z)), method = 'ward')
	plot(hc, axes=F,xlab='', ylab='',sub ='', main='Comp 1/2/3')
	rect.hclust(hc, k=3, border='red')
dev.off()

png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')	
	# create and plot clusters based on the correlation among companies
	hc = hclust(as.dist(1-cor(na.omit(ret))), method = 'ward')
	plot(hc, axes=F,xlab='', ylab='',sub ='', main='Correlation')
	rect.hclust(hc, k=3, border='red')
dev.off()

	# cor(ret, method="pearson")
	# cor(ret, method="kendall")
	# cor(ret, method="spearman")
	

}
	
	
###############################################################################
# Using Principal component analysis (PCA) for spread trading 
# http://matlab-trading.blogspot.ca/2012/12/using-pca-for-spread-trading.html
# http://www.r-bloggers.com/cointegration-r-irish-mortgage-debt-and-property-prices/
###############################################################################		
bt.pca.trading.test <- function()
{
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load.packages('quantmod')
	# tickers = spl('XLE,USO,XES,XOP')
	tickers = dow.jones.components()

	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '2009-01-01', env = data, auto.assign = T)
	bt.prep(data, align='remove.na')

	#*****************************************************************
	# Principal component analysis (PCA), for interesting discussion
	# http://machine-master.blogspot.ca/2012/08/pca-or-polluting-your-clever-analysis.html
	#****************************************************************** 
	prices = last(data$prices, 1000)
		n = len(tickers)  		
	ret = prices / mlag(prices) - 1
	
	p = princomp(na.omit(ret[1:250,]))
	
	loadings = p$loadings[]

	# look at the first 4 principal components 	
	components = loadings[,1:4]
	
	# normalize all selected components to have total weight = 1
	components = components / repRow(colSums(abs(components)),len(tickers))
	
	# note that first component is market, and all components are orthogonal i.e. not correlated to market
	market = ret[1:250,] %*% rep(1/n,n)
	temp = cbind(market, -ret[1:250,] %*% components)
		colnames(temp)[1] = 'Market'	
		
	round(cor(temp, use='complete.obs',method='pearson'),1)

	# the variance of each component is decreasing
	round(100*sd(temp,na.rm=T),1)
	
	#*****************************************************************	
	# examples of stationarity ( mean-reversion )
	# p.value - small => stationary
	# p.value - large => not stationary
	#*****************************************************************	
	library(tseries)
	
	layout(1:2)
	temp = rnorm(100)
	plot(temp, type='b', main=adf.test(temp)$p.value)
	plot(cumsum(temp), type='b', main=adf.test(cumsum(temp))$p.value)
			
	#*****************************************************************
	# Find stationary components, Augmented Dickey-Fuller test
	# library(fUnitRoots)
	# adfTest(as.numeric(equity[,1]), type="ct")@test$p.value	
	#****************************************************************** 	
	library(tseries)
	equity = bt.apply.matrix(1 + ifna(-ret %*% components,0), cumprod)
		equity = make.xts(equity, index(prices))
	
	# test for stationarity ( mean-reversion )
	adf.test(as.numeric(equity[,1]))$p.value
	adf.test(as.numeric(equity[,2]))$p.value
	adf.test(as.numeric(equity[,3]))$p.value
	adf.test(as.numeric(equity[,4]))$p.value


			
	#*****************************************************************
	# Plot securities and components
	#*****************************************************************
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')	
	layout(1:2)
	# add Bollinger Bands
	i.comp = 4	
	bbands1 = BBands(repCol(equity[,i.comp],3), n=200, sd=1)
	bbands2 = BBands(repCol(equity[,i.comp],3), n=200, sd=2)
	temp = cbind(equity[,i.comp], bbands1[,'up'], bbands1[,'dn'], bbands1[,'mavg'],
				bbands2[,'up'], bbands2[,'dn'])
		colnames(temp) = spl('Comp. 4,1SD Up,1SD Down,200 SMA,2SD Up,2SD Down')
	
	plota.matplot(temp, main=paste(i.comp, 'Principal component'))
	
	barplot.with.labels(sort(components[,i.comp]), 'weights')
dev.off()
	

	
    # http://www.wekaleamstudios.co.uk/posts/seasonal-trend-decomposition-in-r/
    ts.sample = ts(as.numeric(equity[,i.comp]), frequency = 252)
    fit.stl = stl(ts.sample, s.window="periodic")
    plot(fit.stl) 
    
    

png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')		
	layout(1:2)
	plota.matplot(prices, plotX=F)
	plota.matplot(equity)
dev.off()
			
}



	
###############################################################################
# Details for the Visual of Current Major Market Clusters post by David Varadi
# http://cssanalytics.wordpress.com/2013/01/10/a-visual-of-current-major-market-clusters/
###############################################################################		
bt.cluster.visual.test <- function()
{
    #*****************************************************************
	# Load historical data for ETFs
	#****************************************************************** 
	load.packages('quantmod')

	tickers = spl('GLD,UUP,SPY,QQQ,IWM,EEM,EFA,IYR,USO,TLT')

	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '1900-01-01', env = data, auto.assign = T)
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
		
	bt.prep(data, align='remove.na')

    #*****************************************************************
	# Create Clusters
	#****************************************************************** 
	# compute returns
	ret = data$prices / mlag(data$prices) - 1
		ret = na.omit(ret)		

	# setup period and method to compute correlations
	dates = '2012::2012'
	method = 'pearson'	# kendall, spearman
	
	correlation = cor(ret[dates], method = method)    
        dissimilarity = 1 - (correlation)
        distance = as.dist(dissimilarity)
        	
	# find 4 clusters      
	xy = cmdscale(distance)
	fit = kmeans(xy, 4, iter.max=100, nstart=100)
	
	fit$cluster
	
    #*****************************************************************
	# Create Plot
	#****************************************************************** 	
	load.packages('cluster')
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')			
	clusplot(xy, fit$cluster, color=TRUE, shade=TRUE, labels=3, lines=0, plotchar=F, 
		main = paste('Major Market Clusters over', dates), sub='')
dev.off()	


png(filename = 'plot2.png', width = 800, height = 800, units = 'px', pointsize = 12, bg = 'white')			
	layout(matrix(1:8,nc=2))
	par( mar = c(2, 2, 2, 2) )

	for(icluster in 2:8)
	clusplot(xy, kmeans(xy, icluster, iter.max=100, nstart=100)$cluster, color=TRUE, shade=F,   
		labels=3, lines=0, plotchar=F, main=icluster, sub='')
dev.off()	

}



###############################################################################
# Optimal number of clusters
# http://en.wikipedia.org/wiki/Determining_the_number_of_clusters_in_a_data_set
#
# R and Data Mining: Examples and Case Studies by Y. Zhao, Chapter 6, Clustering
# http://cran.r-project.org/doc/contrib/Zhao_R_and_data_mining.pdf
#
# http://blog.echen.me/2011/03/19/counting-clusters/
#
# Clustergram: visualization and diagnostics for cluster analysis (R code)
# http://www.r-statistics.com/tag/parallel-coordinates/
#
# http://tr8dr.wordpress.com/2009/12/30/equity-clusters/
# http://www.starklab.org/members/kazmar/2012/01/09/Optimal-number-of-clusters/
#
# Morphometrics with R  By Julien Claude
# http://books.google.ca/books?id=hA9ANHMPm14C&pg=PA123&lpg=PA123&dq=optimal+number+of+clusters+elbow+method&source=bl&ots=7P2bnNf5VL&sig=GEgiSL7CfOEU8gsalSsWHbDhGVc&hl=en&sa=X&ei=wmfwUIOtM_Ls2AW2k4FY&ved=0CFQQ6AEwBg#v=onepage&q=optimal%20number%20of%20clusters%20elbow%20method&f=false
#
# Choosing the number of clusters
# http://geomblog.blogspot.ca/2010/03/this-is-part-of-occasional-series-of.html
# http://geomblog.blogspot.ca/2010/03/choosing-number-of-clusters-ii.html
###############################################################################		
bt.cluster.optimal.number.test <- function()
{
    #*****************************************************************
	# Load historical data for ETFs
	#****************************************************************** 
	load.packages('quantmod')

	tickers = spl('GLD,UUP,SPY,QQQ,IWM,EEM,EFA,IYR,USO,TLT')

	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '1900-01-01', env = data, auto.assign = T)
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
		
	bt.prep(data, align='remove.na')

    #*****************************************************************
	# Create Clusters
	#****************************************************************** 
	# compute returns
	ret = data$prices / mlag(data$prices) - 1
		ret = na.omit(ret)		

	# setup period and method to compute correlations
	dates = '2012::2012'
	method = 'pearson'	# kendall, spearman
	
	correlation = cor(ret[dates], method = method)    
        dissimilarity = 1 - (correlation)
        distance = as.dist(dissimilarity)
        	
	# get first 2 pricipal componenets
	xy = cmdscale(distance)
	
    #*****************************************************************
	# Determine number of clusters
	#****************************************************************** 
	n = ncol(data$prices)
		n1 = ceiling(n*2/3)

	# percentage of variance explained by clusters
	p.exp = rep(0,n1)
		
	# minimum correlation among all components in each cluster	
	min.cor = matrix(1,n1,n1)  
	
	for (i in 2:n1) {
		fit = kmeans(xy, centers=i, iter.max=100, nstart=100)
		p.exp[i] = 1- fit$tot.withinss / fit$totss
		
		for (j in 1:i) {
			index = fit$cluster == j
			min.cor[i,j] = min(correlation[index,index])
		}
	}
	
	# minimum number of clusters that explain at least 90% of variance
	min(which(p.exp > 0.9))
			
	# minimum number of clusters such that correlation among all components in each cluster is at least 40%
	# will not always work
	min(which(apply(min.cor[-1,],1,min,na.rm=T) > 0.4)) + 1

	# number of clusters based on elbow method
	find.maximum.distance.point(p.exp[-1]) + 1

		
    #*****************************************************************
	# Create Plot
	#****************************************************************** 	
	load.packages('cluster')
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')			
	fit = kmeans(xy, 4, iter.max=100, nstart=100)
	clusplot(xy, fit$cluster, color=TRUE, shade=TRUE, labels=3, lines=0, plotchar=F, 
		main = paste('Major Market Clusters over', dates, ', 4 Clusters'), sub='')
dev.off()	

png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')			
	fit = kmeans(xy, 5, iter.max=100, nstart=100)
	clusplot(xy, fit$cluster, color=TRUE, shade=TRUE, labels=3, lines=0, plotchar=F, 
		main = paste('Major Market Clusters over', dates, ', 5 Clusters'), sub='')
dev.off()	


}		
	
