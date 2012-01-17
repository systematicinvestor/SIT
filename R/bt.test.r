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
		capital = 100000
		data$weight[] = (capital / prices) * data$weight
	equal.weight = bt.run(data, type='share', capital=capital)
		
			
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
	bt.prep(data, align='keep.all', dates='1995::2011')
	
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
		position.score[!buy.rule,] = NA
			
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
		position.score[!buy.rule,] = NA
	
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

		# Close next day if Today’s Close > Today’s Open
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
					
	bt.prep(data, align='remove.na', dates='1990::2011')
	bt.prep(data.weekly, align='remove.na', dates='1990::2011')

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
	min.var.weekly = bt.run(data, type='share', capital=capital)
	
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
# Investigate Rebalancing methods:
# 1. Periodic Rebalancing: rebalance to the target mix every month, quarter, year.
# 2. Maximum Deviation Rebalancing: rebalance to the target mix when asset weights deviate more than a given percentage from the target mix.
# 3. Same as 2, but rebalance half-way to target
###############################################################################

# helper function to create barplot with labels
barplot.with.labels <- function(data, main, plotX = TRUE) {
	par(mar=c( iif(plotX, 6, 2), 4, 2, 2))
	x = barplot(100 * data, main = main, las = 2, names.arg = iif(plotX, names(data), ''))
	# http://r.789695.n4.nabble.com/Highliting-a-text-in-a-plot-td828378.html
	text(x, 100 * data, round(100 * data,1), adj=c(0.5,1), xpd = TRUE)
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
	rebalancing.ratio = 0	# 0 means rebalance all-way to target.allocation
							# 0.5 means rebalance half-way to target.allocation
) 
{
	start.index = 1
	nperiods = nrow(model$weight)
	action.index = rep(F, nperiods)
	
	while(T) {	
		# find rows that violate max.deviation
		weight = model$weight
		index = which( apply(abs(weight - repmat(target.allocation, nperiods, 1)), 1, max) > max.deviation)	
	
		if( len(index) > 0 ) {
			index = index[ index > start.index ]
		
			if( len(index) > 0 ) {
				action.index[index[1]] = T
				
				data$weight[] = NA	
					data$weight[1,] = target.allocation
					
					temp = repmat(target.allocation, sum(action.index), 1)
					data$weight[action.index,] = temp + 
						rebalancing.ratio * (weight[action.index,] - temp)					
					
					capital = 100000
					data$weight[] = (capital / data$prices) * data$weight
				model = bt.run(data, type='share', capital=capital, silent=T)	
				start.index = index[1]
			} else break			
		} else break		
	}
	return(model)
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
	garch.vol = ifna.prev(garch.vol)
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
	# Setup search
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
		text(index(data)[min.index - n.query/2], reference[min.index - n.query/2], 1:n.match, 
			adj=c(1,-1), col='black',xpd=TRUE)
		plota.legend('Pattern,Match #','blue,red')
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
	plot.table(temp, smain='Match #')
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
	layout = NULL		# flag to idicate if layout is already set	
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
		
		n.query = len(query)
		n.reference = len(reference)

	#*****************************************************************
	# Compute Distance
	#****************************************************************** 		
	dist = rep(NA, n.reference)
	query.normalized = match.fun(normalize.fn)(query)	
	
	for( i in n.query : n.reference ) {
		window = reference[ (i - n.query + 1) : i]
		window.normalized = match.fun(normalize.fn)(window)	
		dist[i] = match.fun(dist.fn)(rbind(query.normalized, window.normalized))
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
	
		if(is.null(layout)) layout(1:2)		
		par(mar=c(2, 4, 2, 2))
		plot(dates, dist, type='l',col='gray', main='Top Matches', ylab='Euclidean Distance', xlab='')
			abline(h = mean(dist, na.rm=T), col='darkgray', lwd=2)
			points(dates[min.index], dist[min.index], pch=22, col='red', bg='red')
			text(dates[min.index], dist[min.index], 1:n.match, adj=c(1,1), col='black',xpd=TRUE)
			
		plota(data, type='l', col='gray')
			plota.lines(last(data,90), col='blue')
			for(i in 1:n.match) {
			plota.lines(data[(min.index[i]-n.query + 1):min.index[i]], col='red')
			}
			text(index(data)[min.index - n.query/2], reference[min.index - n.query/2], 1:n.match, 
				adj=c(1,-1), col='black',xpd=TRUE)
			plota.legend('Pattern,Match #','blue,red')	
	}
	
	return(list(min.index=min.index, dist=dist[min.index], query=query, reference=reference, dates = index(data)))
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
		
		if(is.null(layout)) layout(1)
		par(mar=c(2, 4, 2, 2))
		matplot(temp, type='l',col='gray',lwd=2, lty='dotted', xlim=c(1,2.5*n.query),
			main = paste('Pattern Prediction with', n.match, 'neighbours'),ylab='Normalized', xlab='')
			lines(temp[,(n.match+1)], col='black',lwd=4)
		
		points(rep(2*n.query,n.match), temp[2*n.query,1:n.match], pch=21, lwd=2, col='gray', bg='gray')
						
		bt.plot.dot.label(2*n.query, temp[2*n.query,1:n.match], 
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
		plot.table(temp, smain='Match #')	
	}
	
	return(temp)
}
		