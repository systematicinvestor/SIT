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
# Fundamental Analysis Tests
# Copyright (C) 2012  Michael Kapler
#
# For more information please visit my blog at www.SystematicInvestor.wordpress.com
# or drop me a line at TheSystematicInvestor at gmail
###############################################################################





###############################################################################
# Barron’s article sends Facebook stock back to the doghouse
# http://www.theglobeandmail.com/globe-investor/barrons-article-sends-facebook-stock-back-to-the-doghouse/article4565523/
# Still Too Pricey
# http://online.barrons.com/article/SB50001424053111904706204578002652028814658.html
###############################################################################
fundamental.fb.test <- function()
{
	#*****************************************************************
	# Load historical fundamental and pricing data
	#****************************************************************** 
	load.packages('quantmod') 
	tickers = spl('FB,LNKD,GRPN,AAPL,GOOG')
	tickers.temp = spl('NASDAQ:FB,NYSE:LNKD,NASDAQ:GRPN,NASDAQ:AAPL,NASDAQ:GOOG')
	
	# get fundamental data
	data.fund <- new.env()
	for(i in 1:len(tickers)) {
		if(is.null(data.fund[[tickers[i]]])) {
			cat(tickers[i],'\n')
			data.fund[[tickers[i]]] = fund.data(tickers.temp[i], 80)
		}
	}
	# sapply(data.fund, function(x) ncol(x))
	# save(data.fund, file='data.fund.fb.Rdata')
	# load(file='data.fund.Rdata')

	
	# get pricing data
	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)	

		
	#*****************************************************************
	# Combine fundamental and pricing data
	#****************************************************************** 				
	for(i in tickers) {
		fund = data.fund[[i]]
		fund.date = date.fund.data(fund)
			
		# Earnings per Share		
		EPS = 4 * get.fund.data('Diluted EPS from Total Operations', fund, fund.date)
		if(nrow(EPS) > 3)
			EPS = rbind(EPS[1:3], get.fund.data('Diluted EPS from Total Operations', fund, fund.date, is.12m.rolling=T)[-c(1:3)])
		
		# merge	
		data[[i]] = merge(data[[i]], EPS)
	}

	bt.prep(data, align='keep.all', dates='1995::')

	
	#*****************************************************************
	# Create PE
	#****************************************************************** 
	prices = data$prices
		prices = bt.apply.matrix(prices, function(x) ifna.prev(x))
		
	EPS =  bt.apply(data, function(x) ifna.prev(x[, 'EPS']))
	
	PE = ifna(prices / EPS, NA)
		PE[ abs(EPS) < 0.001 ] = NA
	
	
    #*****************************************************************
    # Create Report
    #******************************************************************       
png(filename = 'plot1.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')		    
    plota.matplot(PE)
dev.off()					

png(filename = 'plot2.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')		    
    plota.matplot(PE, type='b',pch=20, dates='2012::')
dev.off()					
		

png(filename = 'plot3.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')		    
    plota.matplot(EPS)
dev.off()					
	
png(filename = 'plot4.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')		    
    plota.matplot(prices)
dev.off()					
	
}	



###############################################################################
# DCF - Discounted Cash Flow
# http://www.independent-stock-investing.com/Discounted-Cash-Flow.html
# http://www.oldschoolvalue.com/blog/stock-analysis/apple-aapl-valuation/
# www.focusinvestor.com/DiscountedCashFlows.xls
# http://en.wikipedia.org/wiki/Discounted_cash_flow
###############################################################################
fundamental.dcf.test <- function() 
{
	#*****************************************************************
	# Load historical fundamental and pricing data
	#****************************************************************** 
	load.packages('quantmod') 
	tickers = spl('AAPL')
	tickers.temp = paste(iif( nchar(tickers) <= 3, 'NYSE:', 'NASDAQ:'), tickers, sep='')


	
	# get fundamental data
	data.fund <- new.env()
	for(i in 1:len(tickers))
		data.fund[[tickers[i]]] = fund.data(tickers.temp[i], 80, 'annual')

			
	# get pricing data
	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)			


	fund = data.fund[[tickers[1]]]
	fund.date = date.fund.data(fund)
			
	price = Cl(data[[tickers[1]]]['1995::'])
		
	#*****************************************************************
	# Extract Inputs for DCF Valuation
	#****************************************************************** 				
	# Free Cash Flows
	FCF = get.fund.data('free cash flow', fund, fund.date)
	
	# Invested Capital
	IC = get.fund.data('invested capital', fund, fund.date)
		
	# Sales
	SALE = get.fund.data('total revenue', fund, fund.date)

	# Common Equity
	CEQ = get.fund.data('total equity', fund, fund.date)

	# Common Shares Outstanding
	CSHO = get.fund.data('total common shares out', fund, fund.date)

	# Growth Rate
	CROIC = FCF/IC
	
	# Average inputs
	g = runMean(CROIC, 5)
	cash = runMean(FCF, 5)
	
	
	#*****************************************************************
	# Helper function to compute Intrinsic Value
	#****************************************************************** 				
	compute.DCF.IV <- function(cash, eqity, shares, g, R) {
		if( cash <= 0 ) return(NA)
		
		if( len(R) == 1 ) R = rep(R, len(g))
		
		value = eqity + sum(cash * cumprod(1 + g) / cumprod(1 + R))
		return( value / shares )
	}

		
	#*****************************************************************
	# Compute Intrinsic Value, assumptions:
	# Company will grow for the first 3 years at current Growth Rate
	# slowed down by 20% for the next 4 years, and slowed down by a further 20% for the next 3 years
	# and finally 3% growth for the next 10 years
	#
	# The Discount Rate is 9%
	#
	# http://www.oldschoolvalue.com/blog/stock-analysis/apple-aapl-valuation/
	#****************************************************************** 				
	dcf.price = NA * g
	i.start = which(!is.na(g))[1] 
	
	for(i in i.start : nrow(g)) {
		# Create Growth Rate scenario: 		
		g.scenario = c(rep(g[i],3), rep(g[i],4)*0.8, rep(g[i],3)*0.8*0.8, rep(3/100,10))
		
		# Compute Intrinsic Value
		dcf.price[i] = compute.DCF.IV(cash[i], CEQ[i], CSHO[i], g.scenario, 9/100)
	}
	
	#*****************************************************************
	# Create Plot
	#****************************************************************** 

png(filename = 'plot1.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')		    
	plota(price, type='l', log = 'y', col='blue', main=tickers[1],
		ylim=range(price,dcf.price,na.rm=T))
	plota.lines(dcf.price, type='s', col='red', lwd=2)
	plota.legend('Close,Intrinsic Value', 'blue,red', list(price, dcf.price))	
dev.off()	

	
png(filename = 'plot2.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')		    
	plota(g, type='b', col='blue', pch=0, main='Growth Rate')	
dev.off()	

png(filename = 'plot3.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')		    	
	plota(cash, type='b', col='blue', pch=0, main='Free Cash Flows')	
dev.off()			
	
	
			
 
}
