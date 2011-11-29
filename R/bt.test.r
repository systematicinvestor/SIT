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
		cat('SMA =', ima, '\n')

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
	
	