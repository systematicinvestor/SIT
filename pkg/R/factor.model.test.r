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
# Test cases and examples for Multiple Factor Model functions
# Copyright (C) 2012  Michael Kapler
#
# For more information please visit my blog at www.SystematicInvestor.wordpress.com
# or drop me a line at TheSystematicInvestor at gmail
###############################################################################



###############################################################################
# First Steps in building Multi Factor Model
###############################################################################		
fm.fund.data.test <- function()
{
	#*****************************************************************
	# Load historical fundamental data
	# http://advfn.com/p.php?pid=financials&symbol=NYSE:WMT&mode=quarterly_reports
	#****************************************************************** 
	
	# symbol = 'TSX:BMO' # for Canada
	symbol = 'WMT'	
	symbol = paste(iif( nchar(symbol) <= 3, 'NYSE:', 'NASDAQ:'), symbol, sep='')

	fund = fund.data(symbol, 80)
	
	# construct date
	fund.date = date.fund.data(fund)	
	
	#*****************************************************************
	# Create and Plot Earnings per share
	#****************************************************************** 
	total.capitalization = get.fund.data('total capitalization', fund, fund.date)			
		barplot(total.capitalization)
			
	EPS.Q = as.double(fund['Diluted EPS from Total Operations',])
		EPS.Q = as.xts(EPS.Q, fund.date)	
	EPS = runSum(EPS.Q, 4)

	EPS.Q = get.fund.data('Diluted EPS from Total Operations', fund, fund.date)
	EPS = get.fund.data('Diluted EPS from Total Operations', fund, fund.date, is.12m.rolling=T)
	
	
	# Plot
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')		
	layout(1:2)
	par(mar=c(2,2,2,1))
	x = barplot(EPS.Q, main='Wal-Mart Quarterly Earnings per share', border=NA)
	text(x, EPS.Q, fund['quarterly indicator',], adj=c(0.5,-0.3), cex=0.8, xpd = TRUE)

	barplot(EPS, main='Wal-Mart Rolling Annual Earnings per share', border=NA)
dev.off()
	

	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load.packages('quantmod')
	tickers = 'WMT'
		
	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
		
	data$WMT = merge(data$WMT, EPS)
		# back fill EPS
		data$WMT$EPS = ifna.prev(coredata(data$WMT$EPS))	
	
	# Plot
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')		
	y = data$WMT['1990::']
	plota(Cl(y), type = 'l', LeftMargin=3)
			
	plota2Y(y$EPS, type='l', las=1, col='red', col.axis = 'red')
								
	plota.legend('WMT(rhs),WMT.EPS(lhs)', 'blue,red', list(Cl(y),y$EPS))
dev.off()
			
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load.packages('quantmod')		
	tickers = dow.jones.components()
	
	# get fundamental data
	data.fund <- new.env()
		temp = paste(iif( nchar(tickers) <= 3, 'NYSE:', 'NASDAQ:'), tickers, sep='')
		for(i in 1:len(tickers)) data.fund[[tickers[i]]] = fund.data(temp[i], 80)
	#sapply(data.fund, function(x) ncol(x))
	#save(data.fund, file='data.fund.Rdata')
	
		
	# get pricing data
	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)	
	#save(data, file='data.Rdata')
	
	#load(file='data.fund.Rdata')
	#load(file='data.Rdata')
	
			
	# combine fundamental and pricing data
	for(i in tickers) {
		fund = data.fund[[i]]
		fund.date = date.fund.data(fund)
		
		EPS.Q = as.double(fund['Diluted EPS from Total Operations',])
			EPS.Q = as.xts(EPS.Q, fund.date)	
		EPS = runSum(EPS.Q, 4)
				
		data[[i]] = merge(data[[i]], EPS)
	}

	bt.prep(data, align='keep.all', dates='1995::2011')
		
	#*****************************************************************
	# Compute monthly factors
	#****************************************************************** 
	prices = data$prices
		prices = bt.apply.matrix(prices, function(x) ifna.prev(x))
	
	# create factors
	factors = list()

	# E/P
	EPS = bt.apply(data, function(x) ifna.prev(x[, 'EPS']))
	factors$EP = EPS / prices
			
	# VOMO - Volume x Momentum
	volume = bt.apply(data, function(x) ifna.prev(Vo(x)))
	factors$VOMO = (prices / mlag(prices,10) - 1) * bt.apply.matrix(volume, runMean, 22) / bt.apply.matrix(volume, runMean, 66)
		
	
	# find month ends
	month.ends = endpoints(prices, 'months')
	
	prices = prices[month.ends,]
	n = ncol(prices)
	nperiods = nrow(prices)
	
	ret = prices / mlag(prices) - 1
	next.month.ret = mlag(ret, -1)
	
	factors$EP = factors$EP[month.ends,]	
	factors$VOMO = factors$VOMO[month.ends,]	
		
	#*****************************************************************
	# Correlation Analysis
	#****************************************************************** 
	x = as.vector(factors$EP)
 	y = as.vector(next.month.ret)
 	
 	cor.test(x, y, use = 'complete.obs', method = 'pearson')			

 	# Plot
png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')		
	par(mar=c(4,4,2,1)) 	 	 	
 	plot(x, y, pch=20, main='Correlation Analysis for EP factor', xlab='EP', ylab='Next Month Return')
 		abline(lm(y ~ x), col='blue', lwd=2)
dev.off() 	

}		



###############################################################################
# Multiple Factor Model - Building Fundamental Factors
#http://www.accountingcoach.com/accounting-topics.html
###############################################################################		
fm.fund.factor.test <- function()
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
	load.packages('quantmod,abind')	
	tickers = dow.jones.components()
	
	sectors = factor(sector.map[ match(tickers, sector.map[,'ticker']), 'sector'])
		names(sectors) = tickers
	
	# just load historical fundamental data instead of downloading all data again	
	if(FALSE) {
		# get fundamental data
		data.fund <- new.env()
			temp = paste(iif( nchar(tickers) <= 3, 'NYSE:', 'NASDAQ:'), tickers, sep='')
			for(i in 1:len(tickers)) data.fund[[tickers[i]]] = fund.data(temp[i], 80)
		#sapply(data.fund, function(x) ncol(x))
		save(data.fund, file='data.fund.Rdata')
				
		# get pricing data
		data <- new.env()
		getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
			for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)	
		save(data, file='data.Rdata')
		
	} else {
		# get fundamental data
		load(file='data.fund.Rdata')
		
		# get pricing data
		load(file='data.Rdata')
	}
	


	#*****************************************************************
	# Combine fundamental and pricing data
	#****************************************************************** 
	#VALUING FINANCIAL SERVICE FIRMS
	#www.stern.nyu.edu/~adamodar/pdfiles/papers/finfirm.pdf
	#http://people.stern.nyu.edu/adamodar/pdfiles/papers/
	#"financial companies" "Sales" value
	
	for(i in tickers) {
		fund = data.fund[[i]]
		fund.date = date.fund.data(fund)
					
		# Earnings per Share		
		EPS = get.fund.data('Diluted EPS from Total Operations', fund, fund.date, is.12m.rolling=T)
		
		# Sales, exception not available for financial service firms
		SALE = get.fund.data('total revenue', fund, fund.date, is.12m.rolling=T)
		
		# Common Shares Outstanding
		CSHO = get.fund.data('total common shares out', fund, fund.date)

		# Common Equity
		CEQ = get.fund.data('total equity', fund, fund.date)

		# Dividends
		DV.PS = get.fund.data('dividends paid per share', fund, fund.date, is.12m.rolling=T)
				
		# Cash Flow
		CFL = get.fund.data('net cash from operating activities', fund, fund.date, cash.flow=T, is.12m.rolling=T)
				
		# merge
		data[[i]] = merge(data[[i]], EPS, SALE, CSHO, CEQ, DV.PS, CFL)
	}

	bt.prep(data, align='keep.all', dates='1995::2011')

	#*****************************************************************
	# Create Factors
	#****************************************************************** 
	prices = data$prices	
		prices = bt.apply.matrix(prices, function(x) ifna.prev(x))
		
	sectors	= sectors[colnames(prices)]	

	# create factors
	factors = list()
	
	#*****************************************************************
	# Create Traditional Value
	#****************************************************************** 
	factors$TV = list()

	# Market Value - capitalization
	CSHO =  bt.apply(data, function(x) ifna.prev(x[, 'CSHO']))
	MKVAL = prices * CSHO
	
	# Price / Earnings
	EPS = bt.apply(data, function(x) ifna.prev(x[, 'EPS']))
	factors$TV$EP = EPS / prices
	
	# Price / Trailing Sales
	SALE = bt.apply(data, function(x) ifna.prev(x[, 'SALE']))	
	factors$TV$SP = SALE / MKVAL
	
	# Price / Trailing Cash Flow
	CFL = bt.apply(data, function(x) ifna.prev(x[, 'CFL']))
	factors$TV$CFP = CFL / MKVAL
	
	# Dividend Yield
	DV.PS = bt.apply(data, function(x) ifna.prev(x[, 'DV.PS']))
	factors$TV$DY = DV.PS / prices
	
	# Price / Book Value		
	CEQ = bt.apply(data, function(x) ifna.prev(x[, 'CEQ']))
	factors$TV$BP = CEQ	/ MKVAL

	# Eliminate Price/Sales and Price/Cash Flow for financial firms
	factors$TV$SP[, sectors == 'Financials'] = NA
	factors$TV$CFP[, sectors == 'Financials'] = NA
	
		
	#*****************************************************************
	# Convert to monthly
	#****************************************************************** 
	# find month ends
	month.ends = endpoints(prices, 'months')
	
	prices = prices[month.ends,]
		n = ncol(prices)
		nperiods = nrow(prices)
	
	ret = prices / mlag(prices) - 1
		next.month.ret = mlag(ret, -1)
	
	MKVAL = MKVAL[month.ends,]
	
	for(j in 1:len(factors)) {	
		for(i in 1:len(factors[[j]])) {
			factors[[j]][[i]] = factors[[j]][[i]][month.ends,]	
		}
	}

	
	#*****************************************************************
	# Create the overall Traditional Value factor 
	#****************************************************************** 
	# check missing data for financial firms
	sapply(factors$TV, count)
	
	# plot factors correlation
	# factor.avgcor(factors$TV, next.month.ret)
	
	# normalize (convert to z scores) cross sectionaly all Traditional Value factors
	for(i in names(factors$TV)) {
		#factors$TV[[i]] = (factors$TV[[i]] - apply(factors$TV[[i]], 1, mean, na.rm=T)) / apply(factors$TV[[i]], 1, sd, na.rm=T)
		factors$TV[[i]] = (factors$TV[[i]] - cap.weighted.mean(factors$TV[[i]], MKVAL)) / 
							apply(factors$TV[[i]], 1, sd, na.rm=T)
	}

	# compute the overall Traditional Value factor 
	load.packages('abind')
	temp = abind(factors$TV, along = 3)
	factors$TV$AVG = factors$TV[[1]]
		factors$TV$AVG[] = apply(temp, c(1,2), mean, na.rm=T)
		
		
	# plot quantiles for all Traditional Value factors
	# http://www.quantequityinvesting.com/factor-time-series-chart.php
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')			
	layout(matrix(1:6,nc=2))
	sapply(1:len(factors$TV), function(i)
		compute.quantiles(factors$TV[[i]], next.month.ret, paste(names(factors$TV)[i], 'Traditional Value'))
	)
dev.off() 		
	
	#*****************************************************************
	# Backtest quantiles and quantile spread
	#****************************************************************** 
	out = compute.quantiles(factors$TV$AVG, next.month.ret, plot=F)	
		
	prices = data$prices
		prices = bt.apply.matrix(prices, function(x) ifna.prev(x))

	# create strategies that invest in each qutile
	models = list()
	
	for(i in 1:5) {
		data$weight[] = NA
			data$weight[month.ends,] = iif(out$quantiles == i, out$weights, 0)
			capital = 100000
			data$weight[] = (capital / prices) * (data$weight)	
		models[[paste('Q',i,sep='')]] = bt.run(data, type='share', capital=capital)
	}
	
	# spread
	data$weight[] = NA
		data$weight[month.ends,] = iif(out$quantiles == 5, out$weights, 
									iif(out$quantiles == 1, -out$weights, 0))
		capital = 100000
		data$weight[] = (capital / prices) * (data$weight)
	models$Q5_Q1 = bt.run(data, type='share', capital=capital)
	
	#*****************************************************************
	# Create Report
	#****************************************************************** 
	
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')										
	plotbt(models, plotX = T, log = 'y', LeftMargin = 3)	    	
		mtext('Cumulative Performance', side = 2, line = 1)
dev.off()	
	
	
		
}		


###############################################################################
# Multiple Factor Model - Building All Factors and
# Running Cross-Sectional Regression
#http://www.accountingcoach.com/accounting-topics.html
###############################################################################		
fm.all.factor.test <- function()
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
	load.packages('quantmod,abind')	
	tickers = dow.jones.components()
	
	sectors = factor(sector.map[ match(tickers, sector.map[,'ticker']), 'sector'])
		names(sectors) = tickers
	
	# just load historical fundamental data instead of downloading all data again	
	if(FALSE) {
		# get fundamental data
		data.fund <- new.env()
			temp = paste(iif( nchar(tickers) <= 3, 'NYSE:', 'NASDAQ:'), tickers, sep='')
			for(i in 1:len(tickers)) data.fund[[tickers[i]]] = fund.data(temp[i], 80)
		#sapply(data.fund, function(x) ncol(x))
		save(data.fund, file='data.fund.Rdata')
				
		# get pricing data
		data <- new.env()
		getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
			for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)	
		save(data, file='data.Rdata')
		
	} else {
		# get fundamental data
		load(file='data.fund.Rdata')
		
		# get pricing data
		load(file='data.Rdata')
	}
	
	data.clean(data, min.ratio=3)
  		tickers = ls(data)


	#*****************************************************************
	# Combine fundamental and pricing data
	#****************************************************************** 
	#VALUING FINANCIAL SERVICE FIRMS
	#www.stern.nyu.edu/~adamodar/pdfiles/papers/finfirm.pdf
	#http://people.stern.nyu.edu/adamodar/pdfiles/papers/
	#"financial companies" "Sales" value
	
	for(i in tickers) {
		fund = data.fund[[i]]
		fund.date = date.fund.data(fund)
			nperiods = ncol(fund)
			
		# D - holds all data to be merged with pricing data
		D = list()
		
		#--------------------------------------------------------------
		# Data for Traditional and Relative Value	
		#--------------------------------------------------------------
				
		# Earnings per Share		
		D$EPS = get.fund.data('Diluted EPS from Total Operations', fund, fund.date, is.12m.rolling=T)
		
		# Sales, exception not available for financial service firms
		D$SALE = get.fund.data('total revenue', fund, fund.date, is.12m.rolling=T)
		
		# Common Shares Outstanding
		D$CSHO = get.fund.data('total common shares out', fund, fund.date)

		# Common Equity
		D$CEQ = get.fund.data('total equity', fund, fund.date)

		# Dividends
		D$DV.PS = get.fund.data('dividends paid per share', fund, fund.date, is.12m.rolling=T)
				
		# Cash Flow
		D$CFL = get.fund.data('net cash from operating activities', fund, fund.date, cash.flow=T, is.12m.rolling=T)
			
		#--------------------------------------------------------------
		# Data for Historical Growth	
		#--------------------------------------------------------------
				
		# Consecutive Quarters of Positive Changes in Trailing 12 Month Cash Flow
		D$CFL.CON.CHG = consecutive.changes(D$CFL)		
			# check
			#merge(D$CFL, sign(diff(D$CFL)), consecutive.changes(D$CFL), consecutive.changes(D$CFL,F))
		
		# Consecutive Quarters of Positive Change in Quarterly Earnings
		D$EPS.CON.CHG = consecutive.changes(D$EPS)
		
		# 12 Month Change in Quarterly Cash Flow
		temp = get.fund.data('net cash from operating activities', fund, fund.date, cash.flow=T)
		D$CFL.CHG = temp / mlag(temp,4)
		
		# http://ca.advfn.com/Help/eps-growth-rate-102.html
		# 3 Year Average Annual Sales Growth
		D$SALE.3YR.GR = D$SALE
		if(!all(is.na(D$SALE))) D$SALE.3YR.GR = SMA(ifna(D$SALE / mlag(D$SALE,4) - 1,NA), 3*4)

		# 3 Year Average Annual Earnings Growth
		D$EPS.3YR.GR = SMA(D$EPS / mlag(D$EPS,4) - 1, 3*4)
		
		# 12 Quarter Trendline in Trailing 12 Month Earnings		
		D$EPS.TREND = D$EPS * NA
			D$EPS.TREND[12:nperiods] = sapply(12:nperiods, function(i) beta.degree(ols(cbind(1,1:12), D$EPS[(i-12+1):i])$coefficients[2]))
			
		# Slope of Trend Line Through Last 4 Quarters of Trailing 12M Cash Flows		
		D$CFL.TREND = D$CFL * NA
			D$CFL.TREND[4:nperiods] = sapply(4:nperiods, function(i) beta.degree(ols(cbind(1,1:4), D$CFL[(i-4+1):i])$coefficients[2]))

		#--------------------------------------------------------------
		# Data for Profit Trends	
		#--------------------------------------------------------------
		RECT = get.fund.data('receivables', fund, fund.date)
		INVT = get.fund.data('inventories', fund, fund.date)
		
		D$AT = get.fund.data('total assets', fund, fund.date)
		XSGA = get.fund.data('Selling, General & Administrative (SG&A) Expense', fund, fund.date, is.12m.rolling=T)
		
		# Consecutive Quarters of Declines in (Receivables+Inventories) / Sales
		D$RS.CON.CHG = consecutive.changes((RECT + INVT) / D$SALE, F)
				
		# Consecutive Qtrs of Positive Change in Trailing 12M Cash Flow / Sales
		D$CS.CON.CHG = consecutive.changes(D$CFL/D$SALE)
		
		# Overhead = sales, general and administrative costs
		# http://moneyterms.co.uk/overhead_cost_ratio/
		# Consecutive Quarters of Declines in Trailing 12 Month Overhead / Sales
		D$OS.CON.CHG = consecutive.changes(XSGA/D$SALE, F)
				
		# (Industry Relative) Trailing 12 Month (Receivables+Inventories) / Sales
		D$RS = (RECT + INVT) / D$SALE
		
		# (Industry Relative) Trailing 12 Month Sales / Assets
		D$SA = D$SALE / D$AT
		
		# Trailing 12 Month Overhead / Sales
		D$OS = XSGA / D$SALE
		
		# Trailing 12 Month Earnings / Sales
		D$ES = D$EPS / D$SALE						
							
		#--------------------------------------------------------------
		# Merge Fundamental and Pricing data
		#--------------------------------------------------------------
		
		# merge	
		temp = abind(D,along=2)
    		colnames(temp) = names(D)
    	data[[i]] = merge(adjustOHLC(data[[i]], use.Adjusted=T), as.xts(temp, fund.date))
	}
	
	bt.prep(data, align='keep.all', fill.gaps = T, dates='1995::')


	#*****************************************************************
	# Create Factors
	#****************************************************************** 
	prices = data$prices	
		prices = bt.apply.matrix(prices, function(x) ifna.prev(x))
		
	# re-map sectors
	sectors	= sectors[colnames(prices)]	

	# create factors
	factors = list()
	factor.names = list()
	
	#*****************************************************************
	# Create Traditional Value
	#****************************************************************** 
	factors$TV = list()
	factor.names$TV = 'Traditional Value'

	# Market Value - capitalization
	CSHO =  bt.apply(data, function(x) ifna.prev(x[, 'CSHO']))
	MKVAL = prices * CSHO
	
	# Price / Earnings
	EPS = bt.apply(data, function(x) ifna.prev(x[, 'EPS']))
	factors$TV$EP = EPS / prices
	
	# Price / Trailing Sales
	SALE = bt.apply(data, function(x) ifna.prev(x[, 'SALE']))	
	factors$TV$SP = SALE / MKVAL
	
	# Price / Trailing Cash Flow
	CFL = bt.apply(data, function(x) ifna.prev(x[, 'CFL']))
	factors$TV$CFP = CFL / MKVAL
	
	# Dividend Yield
	DV.PS = bt.apply(data, function(x) ifna.prev(x[, 'DV.PS']))
	factors$TV$DY = DV.PS / prices
	
	# Price / Book Value		
	CEQ = bt.apply(data, function(x) ifna.prev(x[, 'CEQ']))
	factors$TV$BP = CEQ	/ MKVAL

	# Eliminate Price/Sales and Price/Cash Flow for financial firms
	factors$TV$SP[, sectors == 'Financials'] = NA
	factors$TV$CFP[, sectors == 'Financials'] = NA
	
	#*****************************************************************
	# Create Historical Growth
	#****************************************************************** 
	factors$HG = list()
	factor.names$HG = 'Historical Growth'

	for(i in spl('CFL.CON.CHG,EPS.CON.CHG,CFL.CHG,SALE.3YR.GR,EPS.3YR.GR,EPS.TREND,CFL.TREND')) {
		factors$HG[[i]] = bt.apply(data, function(x) ifna.prev(x[, i]))
	}

	#*****************************************************************
	# Create Profit Trends
	#****************************************************************** 
	factors$PT = list()		
	factor.names$PT = 'Profit Trends'	
	
	for(i in spl('RS.CON.CHG,CS.CON.CHG,OS.CON.CHG,RS,SA,OS,ES')) {
		factors$PT[[i]] = bt.apply(data, function(x) ifna.prev(x[, i]))
	}
	
	#*****************************************************************
	# Create Price Momentum
	#****************************************************************** 
	factors$PM = list()
	factor.names$PM = 'Price Momentum'	
	
	# find week ends
	week.ends = endpoints(prices, 'weeks')
		week.prices = prices[week.ends,]
		week.nperiods = nrow(week.prices)
	
	#Slope of 52 Week Trend Line (20 Day Lag)
	factors$PM$S52W.TREND = bt.apply.matrix(week.prices, function(x) {
		c(rep(NA,51),
		sapply(52:week.nperiods, function(i) beta.degree(ols(cbind(1,1:52), x[(i - 52 + 1):i])$coefficients[2]))
		)})
	
	# http://stockcharts.com/school/doku.php?id=chart_school:technical_indicators:price_oscillators
	#4/52 Week Price Oscillator (20 Day Lag)
	factors$PM$PP04.52W = bt.apply.matrix(week.prices, EMA, 4) / bt.apply.matrix(week.prices, EMA, 52)
					
	#39 Week Return (20 Day Lag)
	factors$PM$R39W = week.prices / mlag(week.prices,39)
			
	# http://www.tradesignalonline.com/lexicon/view.aspx?id=Volume+Price+Trend+%28VPT%29
	#51 Week Volume Price Trend (20 Day Lag)	
	temp = bt.apply(data, function(x) cumsum(ifna(Vo(x),0)))
	temp = temp[week.ends,]
		week.volume = temp - mlag(temp)		
	temp = (week.prices - mlag(week.prices)) * week.volume
	factors$PM$VPT51W = bt.apply.matrix(temp, runSum, 51)
			
	# Convert weekly to daily
	for(i in 1:len(factors$PM)) {
		temp = prices * NA
		temp[week.ends,] = factors$PM[[i]]
		factors$PM[[i]] = bt.apply.matrix(temp, function(x) ifna.prev(x))
	}
	
	#Percent Above 260 Day Low (20 Day Lag)
	factors$PM$P260LOW = prices / bt.apply.matrix(prices, runMin, 260)
	
	# Flip sign
	for(i in names(factors$PM)) factors$PM[[i]] = -factors$PM[[i]]
	
	#*****************************************************************
	# Create Price Reversal
	#****************************************************************** 
	factors$PR = list()
	factor.names$PR = 'Price Reversal'	
		
	#5 Day Industry Relative Return
	factors$PR$r5DR = prices/mlag(prices,5)
		factors$PR$r5DR = factors$PR$r5DR / sector.mean(factors$PR$r5DR, sectors)
	
	#5 Day Money Flow / Volume
	factors$PR$MFV = bt.apply(data, function(x) {
		MFI(cbind(ifna.prev(Hi(x)),ifna.prev(Lo(x)),ifna.prev(Cl(x))),5) / ifna.prev(Vo(x))
	})
			
	#10 Day MACD - Signal Line
	factors$PR$MACD = bt.apply.matrix(prices, function(x) {
		temp=MACD(x,10)
		temp[,'macd'] - temp[,'signal']
		})		
	
	#14 Day RSI (Relative Strength Indicator)
	factors$PR$RSI = bt.apply.matrix(prices, RSI, 14)
			
	#14 Day Stochastic
	factors$PR$STOCH = bt.apply(data, function(x) {
		stoch(cbind(ifna.prev(Hi(x)),ifna.prev(Lo(x)),ifna.prev(Cl(x))),14)[,'slowD']
	})
		
	#4 Week Industry Relative Return
	factors$PR$rR4W = week.prices / mlag(week.prices,4)
		factors$PR$rR4W = factors$PR$rR4W / sector.mean(factors$PR$rR4W, sectors)
		
		# Convert weekly to daily
		temp = prices * NA
		temp[week.ends,] = factors$PR$rR4W
		factors$PR$rR4W = bt.apply.matrix(temp, function(x) ifna.prev(x))
	
	
	# VOMO - Volume x Momentum
	volume = bt.apply(data, function(x) ifna.prev(Vo(x)))
	factors$PR$VOMO = (prices / mlag(prices,10) - 1) * bt.apply.matrix(volume, runMean, 22) / bt.apply.matrix(volume, runMean, 66)
	
	# Flip sign
	for(i in names(factors$PR)) factors$PR[[i]] = -factors$PR[[i]]
	
	#*****************************************************************
	# Create Small Size
	#****************************************************************** 		
	factors$SS = list()
	factor.names$SS = 'Small Size'		
	
	#Log of Market Capitalization
	factors$SS$MC = log(MKVAL)
	
	#Log of Market Capitalization Cubed
	factors$SS$MC3 = log(MKVAL)^3
	
	#Log of Stock Price
	factors$SS$P = log(prices)
	
	#Log of Total Assets
	factors$SS$AT = log(bt.apply(data, function(x) ifna.prev(x[, 'AT'])))
	
	#Log of Trailing-12-Month Sales
	factors$SS$SALE = log(bt.apply(data, function(x) ifna.prev(x[, 'SALE'])))

	# Flip sign
	for(i in names(factors$SS)) factors$SS[[i]] = -factors$SS[[i]]
	
	#*****************************************************************
	# Convert to monthly
	#****************************************************************** 
	# find month ends
	month.ends = endpoints(prices, 'months')
	
	prices = prices[month.ends,]
		n = ncol(prices)
		nperiods = nrow(prices)
	
	ret = prices / mlag(prices) - 1
		next.month.ret = mlag(ret, -1)
	
	MKVAL = MKVAL[month.ends,]
	
	for(j in 1:len(factors)) {	
		for(i in 1:len(factors[[j]])) {
			factors[[j]][[i]] = factors[[j]][[i]][month.ends,]	
			factors[[j]][[i]][] = ifna(factors[[j]][[i]],NA)
		}
	}

	#*****************************************************************
	# Create Relative Value
	#****************************************************************** 
	factors$RV = list()
	factor.names$RV = 'Relative Value'		
	
	# relative 
	for(i in spl('EP,SP,CFP')) {
		factors$RV[[paste('r',i,sep='')]] = factors$TV[[i]] / sector.mean(factors$TV[[i]], sectors) 		
	}
		
	# spreads, 5 Year Avg = 60 months
	for(i in spl('rEP,rSP,rCFP')) {
		factors$RV[[paste('s',i,sep='')]] = factors$RV[[i]] - 
		bt.apply.matrix(factors$RV[[i]], function(x) if(all(is.na(x))) x else SMA(x,60)[1:len(x)])
	}
	
	#*****************************************************************
	# Profit Trends (Relative)
	#****************************************************************** 
	
	# relative 
	for(i in spl('RS,SA')) {
		factors$PT[[paste('r',i,sep='')]] = factors$PT[[i]] / sector.mean(factors$PT[[i]], sectors)
	}		
	
	for(j in 1:len(factors)) {	
		for(i in 1:len(factors[[j]])) {
			factors[[j]][[i]][] = ifna(factors[[j]][[i]],NA)
		}
	}
	
	#*****************************************************************
	# Normalize and add Average factor
	#****************************************************************** 
	for(j in names(factors)) {
		factors[[j]] = normalize.normal(factors[[j]])
		factors[[j]] = add.avg.factor(factors[[j]])
	}

	#*****************************************************************
	# Create Composite Average factor
	#****************************************************************** 	
	factors.avg = list()
	for(j in names(factors)) factors.avg[[j]] = factors[[j]]$AVG
	
	factors.avg = add.avg.factor(factors.avg)

	#*****************************************************************
	# Backtest qutiles and qutile spread
	#****************************************************************** 
png(filename = 'plot1.png', width = 600, height = 800, units = 'px', pointsize = 12, bg = 'white')		
	plot.quantiles(factors.avg, next.month.ret, 'Average')
dev.off() 	

png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')		
	plot.bt.quantiles(factors.avg$AVG, next.month.ret, 'Composite Average', data)
dev.off() 	




	#*****************************************************************
	# Save CSFB factors to be used later to create multiple factor Risk Model
	#****************************************************************** 
	save(data, sectors, factors.avg, next.month.ret, file='data.factors.Rdata')
		# remove Composite Average factor
		factors.avg = factors.avg[which(names(factors.avg) != 'AVG')]
	
	
	#*****************************************************************
	# Run cross sectional regression and create Alpha model
	#****************************************************************** 
	nperiods = nrow(next.month.ret)
	n = ncol(next.month.ret)
			
	# create matrix for each factor
	factors.matrix = abind(factors.avg, along = 3)	
	all.data = factors.matrix
	
	# betas
	beta = all.data[,1,] * NA
	
	# append next.month.ret to all.data			
	all.data = abind(next.month.ret, all.data, along = 3)
		dimnames(all.data)[[3]][1] = 'Ret'
		
	# estimate betas (factor returns)
	for(t in 12:(nperiods-1)) {
		temp = all.data[t:t,,]
		x = temp[,-1]
		y = temp[,1]
		beta[(t+1),] = lm(y~x-1)$coefficients
	}
	
	# create Alpha return forecasts
	alpha = next.month.ret * NA
		
	for(t in 18:(nperiods-1)) {
		coef = colMeans(beta[(t-5):t,],na.rm=T)
		
		coef = iif(coef > 0, coef, 0)
		
		alpha[t,] = rowSums(all.data[t,,-1] * t(repmat(coef, 1,n)), na.rm=T)	
	}
	

	
	
	#*****************************************************************
	# Backtest qutiles and qutile spread
	#****************************************************************** 
png(filename = 'plot4.png', width = 600, height = 800, units = 'px', pointsize = 12, bg = 'white')		

	layout(1:2)
	temp = compute.quantiles(alpha, next.month.ret, plot=T)
	plot.bt.quantiles(alpha, next.month.ret, 'Alpha', data)
	
	
dev.off() 	

	
}		


###############################################################################
# Multiple Factor Model - Building Risk Model
#
# The risk factor model: MSCI Barra United States Equity Multi-Factor Model, page 101
# http://www.alacra.com/alacra/help/barra_handbook_US.pdf
###############################################################################		
fm.risk.model.test <- function()
{	
	#*****************************************************************
	# Load factor data that we saved at the end of the fm.all.factor.test function
	#****************************************************************** 
	load.packages('quantmod,abind')	
		
	load(file='data.factors.Rdata')
		# remove Composite Average factor
		factors.avg = factors.avg[which(names(factors.avg) != 'AVG')]	
		
	#*****************************************************************
	# Run cross sectional regression to estimate factor returns
	#****************************************************************** 
	nperiods = nrow(next.month.ret)
	n = ncol(next.month.ret)
		
	# create sector dummy variables: binary 0/1 values for each sector
	nsectors = len(levels(sectors))	
	sectors.matrix = array(double(), c(nperiods, n, nsectors))
		dimnames(sectors.matrix)[[3]] = levels(sectors)		
	for(j in levels(sectors)) {
		sectors.matrix[,,j] = matrix(sectors == j,  nr=nperiods, nc=n, byrow=T)
	}
	
	# create matrix for each factor
	factors.matrix = abind(factors.avg, along = 3)		
	
	# combine sector dummies and all factors
	all.data = abind(sectors.matrix, factors.matrix)		
	
	# create betas and specific.return
	beta = all.data[,1,] * NA
	specific.return = next.month.ret * NA
		nfactors = ncol(beta)
		
	# append next.month.ret to all.data			
	all.data = abind(next.month.ret, all.data, along = 3)
		dimnames(all.data)[[3]][1] = 'Ret'
			
	# estimate betas (factor returns)
	for(t in 12:(nperiods-1)) {		
		temp = all.data[t:t,,]
		x = temp[,-c(1:2)]
		y = temp[,1]
		b = lm(y~x)$coefficients
		
		b[2:nsectors] = b[1] + b[2:nsectors]
		beta[(t+1),] = b		
		
		specific.return[(t+1),] = y - rowSums(temp[,-1] * matrix(b, n, nfactors, byrow=T), na.rm=T)	
	}
	
	#*****************************************************************
	# Stock specific return - return not captured by common factors
	# specific.return = company.return - exposures %*% beta 
	#
	# http://www.business.uconn.edu/finance/seminars/papers/Chanatip.Kitwiwattanachai_JMP.pdf
	# page 17, risk model "industry dummy" "linearly dependent"	
	#
	# Northfield Fundamental Risk Model
	# www.northinfo.com/documents/8.pdf
	#
	# There are a few alternative ways of running above regression in robust way
	# load.packages('MASS')
	# temp = rlm(y~x)$coefficients
	#
	# load.packages('quantreg')
	# temp = rq(y ~ x, tau = 0.5)$coefficients
	#*****************************************************************

	

	#*****************************************************************
	# helper function
	#****************************************************************** 	
	fm.hist.plot <- function(temp, smain=NULL) {			
		ntemp = ncol(temp)		
		cols = plota.colors(ntemp)	
		plota(temp, ylim = range(temp), log='y', main=smain)
		for(i in 1:ntemp) plota.lines(temp[,i], col=cols[i])
		plota.legend(colnames(temp), cols, as.list(temp))
	}

	#*****************************************************************
	# Examine historical cumulative factor returns
	#****************************************************************** 	
	temp = make.xts(beta, index(next.month.ret))
		temp = temp['2000::',]
		temp[] = apply(coredata(temp), 2, function(x) cumprod(1 + ifna(x,0)))
	
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')		
	fm.hist.plot(temp[,-c(1:nsectors)], 'Factor Returns')
dev.off()	

	#*****************************************************************
	# Compute factor covariance using shrinkage estimator
	# use var.shrink.eqcor function in BurStFin package to estimate a 
	# variance matrix using Ledoit-Wolf shrinkage towards equal correlation
	# http://www.portfolioprobe.com/wp-content/uploads/2010/08/pprobe_R_risk.txt
	#
	# There are a few alternative ways of estimating covariance
	# load.packages('tawny')
	# tawny::cov.shrink(beta)	
	#
	# Exponentially weighted
	# lam = 0.9
	# i = 0:(nrow(beta)-1)
	# wt = lam^i
	# wt = wt/sum(wt)
	# cov.wt(beta, wt=rev(wt)) 	
	#
	# load.packages('corpcor')
	# corpcor::cov.shrink(beta, w=rev(wt)) 		
	#****************************************************************** 
	load.packages('BurStFin')	
	#var.shrink.eqcor(returns) 
	#factor.model.stat(returns)
	
	
	factor.covariance = array(double(), c(nperiods, nfactors, nfactors))
		dimnames(factor.covariance)[[2]] = colnames(beta)
		dimnames(factor.covariance)[[3]] = colnames(beta)

	# estimate factor covariance
	for(t in 36:nperiods) {
		factor.covariance[t,,] = var.shrink.eqcor(beta[(t-23):t,])
	}
	
	#*****************************************************************
	# Compute stocks specific variance foreasts using GARCH(1,1)
	# Rely on code in the Volatility Forecasting using Garch(1,1) post
	#
	# Stock specific return - return not captured by common factors
	# specific.return = company.return - exposures %*% beta 
	#****************************************************************** 		
	load.packages('tseries,fGarch')
		
	specific.variance = next.month.ret * NA
	
	for(i in 1:n) specific.variance[,i] = bt.forecast.garch.volatility(specific.return[,i], 24) 

	# compute historical specific.variance
	hist.specific.variance = next.month.ret * NA
	for(i in 1:n) hist.specific.variance[,i] = runSD(specific.return[,i], 24)	
	
	# if specific.variance is missing use historical specific.variance	
	specific.variance[] = ifna(coredata(specific.variance), coredata(hist.specific.variance))	
			

	#*****************************************************************
	# Save multiple factor risk model to be used later during portfolio construction
	#****************************************************************** 
	save(all.data, factor.covariance, specific.variance, file='risk.model.Rdata')
			
	
	#*****************************************************************
	# Compute portfolio risk
	#****************************************************************** 
	portfolio = rep(1/n, n)
		portfolio = matrix(portfolio, n, nfactors)
	
	portfolio.risk = next.month.ret[,1] * NA
	for(t in 36:(nperiods-1)) {
	
		#exposure = portfolio %*% exposures		
		portfolio.exposure = colSums(portfolio * all.data[t,,-1], na.rm=T)
		
		#risk = sqrt(exposure %*% factor.covariance %*% t(exposure) + specific.variance %*% portfolio^2)
		portfolio.risk[t] = sqrt(
			portfolio.exposure %*% factor.covariance[t,,] %*% (portfolio.exposure) + 
			sum(specific.variance[t,]^2 * portfolio[,1]^2, na.rm=T)
			)
	}
	
			
	#*****************************************************************
	# Compute historical portfolio risk
	#****************************************************************** 
	portfolio = rep(1/n, n)
		portfolio = t(matrix(portfolio, n, nperiods))
	
	portfolio.returns = next.month.ret[,1] * NA
		portfolio.returns[] = rowSums(mlag(next.month.ret) * portfolio, na.rm=T)
	
	hist.portfolio.risk = runSD(portfolio.returns, 24)
	
	#*****************************************************************
	# Plot risks
	#****************************************************************** 			

png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')		

	plota(portfolio.risk['2000::',], type='l')
		plota.lines(hist.portfolio.risk, col='blue')
		plota.legend('Risk,Historical Risk', 'black,blue')
		
dev.off()	
	
	
}



###############################################################################
# Why do we need a risk model
#
# Portfolio Optimization with Factors, Scenarios, and Realistic Short Positions
# by B. Jacobs, K. Levy, H. Markowitz
###############################################################################		
fm.risk.model.optimization.test <- function()
{	
	#*****************************************************************
	# Load data
	#****************************************************************** 
	load.packages('quantmod')	

	# Load factor data that we saved at the end of the fm.all.factor.test function
	load(file='data.factors.Rdata')
		nperiods = nrow(next.month.ret)
		n = ncol(next.month.ret)
		tickers = colnames(next.month.ret)
		
	# Load multiple factor risk model data that we saved at the end of the fm.risk.model.test function	
	load(file='risk.model.Rdata')
		
		
	#*****************************************************************
	# Construct minimum variance portfolio using the sample covariance matrix
	#****************************************************************** 
	load.packages('quadprog,corpcor')
	
	#--------------------------------------------------------------------------
	# Create Covariance matrix
	#--------------------------------------------------------------------------
	temp = last(mlag(next.month.ret),24)
	cov.temp = cov(temp, use='complete.obs', method='pearson')	
	hist.cov = cov.temp
	
	#--------------------------------------------------------------------------
	# Adjust Covariance matrix
	#--------------------------------------------------------------------------
	if(!is.positive.definite(cov.temp)) {
		cov.temp <- make.positive.definite(cov.temp, 0.000000001)
	}	
		
	#--------------------------------------------------------------------------
	# Create constraints
	#--------------------------------------------------------------------------
	# set min/max wgts for individual stocks: 0 =< x <= 1
	constraints = new.constraints(n, lb = 0, ub = 1)
	
	# wgts must sum to 1 (fully invested)
	constraints = add.constraints(rep(1,n), 1, type = '=', constraints)
	
		
	#--------------------------------------------------------------------------
	# Solve QP problem
	#--------------------------------------------------------------------------		
	sol = solve.QP.bounds(Dmat = cov.temp, dvec = rep(0, nrow(cov.temp)) , 
		Amat=constraints$A, bvec=constraints$b, constraints$meq,
		lb = constraints$lb, ub = constraints$ub)

	print(sqrt(sol$value))
	#--------------------------------------------------------------------------
	# Plot Portfolio weights
	#--------------------------------------------------------------------------				
	x = round(sol$solution,4)
		x = iif(x < 0, 0, x)
		names(x) = colnames(next.month.ret)
	hist.min.var.portfolio = x

png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')		
		
	barplot(100*x,las = 2, 
		main = 'Minimum variance portfolio weights (sample covariance matrix)')
		
dev.off()	


		
	#*****************************************************************
	# Construct minimum variance portfolio using the multiple factor risk model
	#****************************************************************** 
	t = nperiods	
	factor.exposures = all.data[t,,-1]	
		nfactors = ncol(factor.exposures)
		
	#--------------------------------------------------------------------------
	# Create constraints
	#--------------------------------------------------------------------------
	# set min/max wgts for individual stocks: 0 =< x <= 1
	constraints = new.constraints(n, lb = 0, ub = 1)
	
	# wgts must sum to 1 (fully invested)
	constraints = add.constraints(rep(1,n), 1, type = '=', constraints)
		
	# adjust prior constraints, add factor exposures
	constraints = add.variables(nfactors, constraints)
	
	# BX - X1 = 0
	constraints = add.constraints(rbind(factor.exposures, -diag(nfactors)), rep(0, nfactors), type = '=', constraints)

	#--------------------------------------------------------------------------
	# Create Covariance matrix
	# [Qu  0]
	# [ 0 Qf]
	#--------------------------------------------------------------------------
	temp = diag(n)
		diag(temp) = specific.variance[t,]^2
	cov.temp = diag(n + nfactors)
		cov.temp[1:n,1:n] = temp
	cov.temp[(n+1):(n+nfactors),(n+1):(n+nfactors)] = factor.covariance[t,,]
	
	#--------------------------------------------------------------------------
	# Solve QP problem
	#--------------------------------------------------------------------------
	load.packages('quadprog')
	
	sol = solve.QP.bounds(Dmat = cov.temp, dvec = rep(0, nrow(cov.temp)) , 
		Amat=constraints$A, bvec=constraints$b, constraints$meq,
		lb = constraints$lb, ub = constraints$ub)

	print(sqrt(sol$value))
	#--------------------------------------------------------------------------
	# Plot Portfolio weights
	#--------------------------------------------------------------------------				
	x = round(sol$solution,4)[1:n]
		x = iif(x < 0, 0, x)
		names(x) = colnames(next.month.ret)
	risk.model.min.var.portfolio = x

png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')		
		
	barplot(100*x,las = 2, 
		main = 'Minimum variance portfolio weights (multiple factor risk model)')
		
dev.off()	


	#--------------------------------------------------------------------------
	# Comare portfolio risk under historical covariance and risk model covariance
	#--------------------------------------------------------------------------					
	
	fm.risk.cov(hist.min.var.portfolio, hist.cov)
	fm.risk.cov(risk.model.min.var.portfolio, hist.cov)

	fm.risk.model(hist.min.var.portfolio, factor.exposures, factor.covariance[t,,], specific.variance[t,])
	fm.risk.model(risk.model.min.var.portfolio, factor.exposures, factor.covariance[t,,], specific.variance[t,])


}

# compute portfolio risk using sample covariance matrix
fm.risk.cov <- function(portfolio, cov) { 
	sqrt(portfolio %*% cov %*% portfolio) 
}
	
# compute portfolio risk using multiple factor risk model
fm.risk.model <- function
(
	portfolio, 
	factor.exposures, 
	factor.covariance, 
	specific.variance
) 
{ 
	factor.exposures = ifna(factor.exposures, 0)
	specific.variance = ifna(specific.variance, mean(coredata(specific.variance), na.rm=T))


	portfolio.exposure = portfolio %*% factor.exposures
	
	sqrt(
		portfolio.exposure %*% factor.covariance %*% t(portfolio.exposure) + 
		sum(specific.variance^2 * portfolio^2)
		)[1]
}



###############################################################################
# 130/30: The New Long-Only (2008) by A. Lo, P. Patel
# Suggested by Nico
###############################################################################		
fm.long.short.test <- function()
{	
	#*****************************************************************
	# Load data
	#****************************************************************** 
	load.packages('quantmod')	

	# Load factor data that we saved at the end of the fm.all.factor.test function
	load(file='data.factors.Rdata')
		nperiods = nrow(next.month.ret)
		tickers = colnames(next.month.ret)
		n = len(tickers)
		
	# Load multiple factor risk model data that we saved at the end of the fm.risk.model.test function	
	load(file='risk.model.Rdata')
		factor.exposures = all.data[,,-1]	
		factor.names = dimnames(factor.exposures)[[3]]
		nfactors = len(factor.names)
			
	#*****************************************************************
	# Compute Betas: b = cov(r,m) / var(m)
	# The betas are measured on a two-year rolling window
	# http://en.wikipedia.org/wiki/Beta_(finance)
	#****************************************************************** 
	ret = mlag(next.month.ret)
	beta = ret * NA

	# 1/n benchmark portfolio
	benchmark = ntop(ret, n)	
	benchmark.ret = rowSums(benchmark * ret, na.rm=T)
	
	# estimate betas
	for(t in 24:nperiods) {
		t.index = (t-23):t
		benchmark.var = var( benchmark.ret[t.index], na.rm=T )
		
		t.count = count(ret[t.index, ])
		t.cov = cov( ifna(ret[t.index,], 0), benchmark.ret[t.index], use='complete.obs' )
		
		beta[t,] = iif(t.count > 20, t.cov/benchmark.var, NA)
	}
		
			
	#*****************************************************************
	# Construct LONG ONLY portfolio using the multiple factor risk model
	#****************************************************************** 
	load.packages('quadprog,corpcor,kernlab')
	
	weight = NA * next.month.ret
	weights = list()
		weights$benchmark = ntop(beta, n)		
		weights$long.alpha = weight
	
	for(t in 36:nperiods) {	
		#--------------------------------------------------------------------------
		# Create constraints
		#--------------------------------------------------------------------------
		# set min/max wgts for individual stocks: 0 =< x <= 10/100
		constraints = new.constraints(n, lb = 0, ub = 10/100)
		
		# wgts must sum to 1 (fully invested)
		constraints = add.constraints(rep(1,n), type = '=', b = 1, constraints)
			
		#--------------------------------------------------------------------------
		# beta of portfolio is the weighted average of the individual asset betas		
		# http://www.duke.edu/~charvey/Classes/ba350/riskman/riskman.htm
		#--------------------------------------------------------------------------
		constraints = add.constraints(ifna(as.vector(beta[t,]),0), type = '=', b = 1, constraints)
			
		#--------------------------------------------------------------------------
		# Create factor exposures constraints
		#--------------------------------------------------------------------------	
		# adjust prior constraints, add factor exposures
		constraints = add.variables(nfactors, constraints)
		
		# BX - X1 = 0
		constraints = add.constraints(rbind(ifna(factor.exposures[t,,], 0), -diag(nfactors)), rep(0, nfactors), type = '=', constraints)
	
		#--------------------------------------------------------------------------
		# Create Covariance matrix
		# [Qu  0]
		# [ 0 Qf]
		#--------------------------------------------------------------------------
		temp = diag(n)
			diag(temp) = ifna(specific.variance[t,], mean(coredata(specific.variance[t,]), na.rm=T))^2
		cov.temp = diag(n + nfactors)
			cov.temp[1:n,1:n] = temp
		cov.temp[(n+1):(n+nfactors),(n+1):(n+nfactors)] = factor.covariance[t,,]
				
		#--------------------------------------------------------------------------
		# page 9, Risk: We use the Barra default setting, risk aversion value of 0.0075, and
		# AS-CF risk aversion ratio of 1.
		#
		# The Effects of Risk Aversion on Optimization (2010) by S. Liu, R. Xu
		# page 4/5
		#--------------------------------------------------------------------------
		risk.aversion = 0.0075
	
		# set expected return
		alpha = factors.avg$AVG[t,] / 5
		expected.return = c(ifna(coredata(alpha),0), rep(0, nfactors))
			
		# remove companies that have no beta from optimization
		index = which(is.na(beta[t,]))
		if( len(index) > 0) {
			constraints$ub[index] = 0
			constraints$lb[index] = 0
		}
	
		# find solution
		sol = solve.QP.bounds(Dmat = 2* risk.aversion * cov.temp, dvec = expected.return, 
					Amat = constraints$A, bvec = constraints$b, 
					meq = constraints$meq, lb = constraints$lb, ub = constraints$ub)
					
		weights$long.alpha[t,] = sol$solution[1:n]
		
		cat(t, '\n')
	}
	
		
	#*****************************************************************
	# Construct Long/Short 130:30 portfolio using the multiple factor risk model
	# based on the examples in the aa.long.short.test functions
	#****************************************************************** 
	weights$long.short.alpha = weight
				
	for(t in 36:nperiods) {	
		#--------------------------------------------------------------------------
		# Create constraints
		#--------------------------------------------------------------------------
		# set min/max wgts for individual stocks: -10/100 =< x <= 10/100
		constraints = new.constraints(n, lb = -10/100, ub = 10/100)
		
		# wgts must sum to 1 (fully invested)
		constraints = add.constraints(rep(1,n), type = '=', b = 1, constraints)
			
		#--------------------------------------------------------------------------
		# beta of portfolio is the weighted average of the individual asset betas		
		# http://www.duke.edu/~charvey/Classes/ba350/riskman/riskman.htm
		#--------------------------------------------------------------------------
		constraints = add.constraints(ifna(as.vector(beta[t,]),0), type = '=', b = 1, constraints)
			
		#--------------------------------------------------------------------------
		# Create factor exposures constraints
		#--------------------------------------------------------------------------	
		# adjust prior constraints, add factor exposures
		constraints = add.variables(nfactors, constraints)
		
		# BX - X1 = 0
		constraints = add.constraints(rbind(ifna(factor.exposures[t,,], 0), -diag(nfactors)), rep(0, nfactors), type = '=', constraints)
			
		#--------------------------------------------------------------------------
		# Create 130:30
		# -v.i <= x.i <= v.i, v.i>0, SUM(v.i) = 1.6
		#--------------------------------------------------------------------------
		
		# adjust prior constraints, add v.i
		constraints = add.variables(n, constraints)
	
		# -v.i <= x.i <= v.i
		#   x.i + v.i >= 0
		constraints = add.constraints(rbind(diag(n), matrix(0,nfactors,n)  ,diag(n)), rep(0, n), type = '>=', constraints)
		#   x.i - v.i <= 0
		constraints = add.constraints(rbind(diag(n), matrix(0,nfactors,n), -diag(n)), rep(0, n), type = '<=', constraints)
		
		# SUM(v.i) = 1.6
		constraints = add.constraints(c(rep(0, n), rep(0, nfactors), rep(1, n)), 1.6, type = '=', constraints)
		
		#--------------------------------------------------------------------------
		# Create Covariance matrix
		# [Qu  0]
		# [ 0 Qf]
		#--------------------------------------------------------------------------
		temp = diag(n)
			diag(temp) = ifna(specific.variance[t,], mean(coredata(specific.variance[t,]), na.rm=T))^2
		cov.temp = 0*diag(n + nfactors + n)
			cov.temp[1:n,1:n] = temp
		cov.temp[(n+1):(n+nfactors),(n+1):(n+nfactors)] = factor.covariance[t,,]
			
		#--------------------------------------------------------------------------
		# page 9, Risk: We use the Barra default setting, risk aversion value of 0.0075, and
		# AS-CF risk aversion ratio of 1.
		#
		# The Effects of Risk Aversion on Optimization (2010) by S. Liu, R. Xu
		# page 4/5
		#--------------------------------------------------------------------------
		risk.aversion = 0.0075
	
		# set expected return
		alpha = factors.avg$AVG[t,] / 5
		expected.return = c(ifna(coredata(alpha),0), rep(0, nfactors), rep(0, n))
			
		# remove companies that have no beta from optimization
		index = which(is.na(beta[t,]))
		if( len(index) > 0) {
			constraints$ub[index] = 0
			constraints$lb[index] = 0
		}
	
		# find solution
		sol = solve.QP.bounds(Dmat = 2* risk.aversion * cov.temp, dvec = expected.return, 
					Amat = constraints$A, bvec = constraints$b, 
					meq = constraints$meq, lb = constraints$lb, ub = constraints$ub)
			
		weights$long.short.alpha[t,] = sol$solution[1:n]
		
		cat(t, '\n')
	}

		
	#*****************************************************************
	# Construct LONG ONLY minimum variance portfolio using the multiple factor risk model
	#****************************************************************** 
	weights$long.min.var.alpha = weight
				
	for(t in 36:nperiods) {	
		#--------------------------------------------------------------------------
		# Create constraints
		#--------------------------------------------------------------------------
		# set min/max wgts for individual stocks: 0 =< x <= 10/100
		constraints = new.constraints(n, lb = 0, ub = 10/100)
		
		# wgts must sum to 1 (fully invested)
		constraints = add.constraints(rep(1,n), type = '=', b = 1, constraints)
			
		#--------------------------------------------------------------------------
		# Create factor exposures constraints
		#--------------------------------------------------------------------------	
		# adjust prior constraints, add factor exposures
		constraints = add.variables(nfactors, constraints)
		
		# BX - X1 = 0
		constraints = add.constraints(rbind(ifna(factor.exposures[t,,], 0), -diag(nfactors)), rep(0, nfactors), type = '=', constraints)
	
		#--------------------------------------------------------------------------
		# Create Covariance matrix
		# [Qu  0]
		# [ 0 Qf]
		#--------------------------------------------------------------------------
		temp = diag(n)
			diag(temp) = ifna(specific.variance[t,], mean(coredata(specific.variance[t,]), na.rm=T))^2
		cov.temp = diag(n + nfactors)
			cov.temp[1:n,1:n] = temp
		cov.temp[(n+1):(n+nfactors),(n+1):(n+nfactors)] = factor.covariance[t,,]
				
		#--------------------------------------------------------------------------
		# Setup optimizations
		#--------------------------------------------------------------------------	
		# set expected return
		alpha = factors.avg$AVG[t,] / 5
		expected.return = c(ifna(coredata(alpha),0), rep(0, nfactors))
			
		# remove companies that have no beta from optimization
		index = which(is.na(beta[t,]))
		if( len(index) > 0) {
			constraints$ub[index] = 0
			constraints$lb[index] = 0
		}
	
		# find solution
		sol = solve.QP.bounds(Dmat = cov.temp, dvec = 0 * expected.return, 
					Amat = constraints$A, bvec = constraints$b, 
					meq = constraints$meq, lb = constraints$lb, ub = constraints$ub)
					
		weights$long.min.var.alpha[t,] = sol$solution[1:n]
		
		cat(t, '\n')
	}

			
	#*****************************************************************
	# Construct Market-Neutral portfolio 100:100 with beta=0 using the multiple factor risk model
	# based on the examples in the aa.long.short.test functions
	#****************************************************************** 
	weights$market.neutral.alpha = weight

	for(t in 36:nperiods) {	
		#--------------------------------------------------------------------------
		# Split x into x.long and x.short, x_long and x_short >= 0
		# SUM(x.long) - SUM(x.short) = 0
		#--------------------------------------------------------------------------			
		# x.long and x.short >= 0
		# x.long <= 0.1 
		# x.short <= 0.1 
		constraints = new.constraints(2*n, lb = 0, ub = c(rep(0.1,n), rep(0.1,n)))
		
		# SUM (x.long - x.short) = 0
		constraints = add.constraints(c(rep(1,n), -rep(1,n)), 0, type = '=', constraints)		
	
		# SUM (x.long + x.short) = 2
		constraints = add.constraints(c(rep(1,n), rep(1,n)), 2, type = '=', constraints)		
				
		#--------------------------------------------------------------------------
		# beta of portfolio is the weighted average of the individual asset betas		
		# http://www.duke.edu/~charvey/Classes/ba350/riskman/riskman.htm
		#--------------------------------------------------------------------------
		temp = ifna(as.vector(beta[t,]),0)
		constraints = add.constraints(c(temp, -temp), type = '=', b = 0, constraints)
			
		#--------------------------------------------------------------------------
		# Create factor exposures constraints
		#--------------------------------------------------------------------------	
		# adjust prior constraints, add factor exposures
		constraints = add.variables(nfactors, constraints)
		
		# BX - X1 = 0
		temp = ifna(factor.exposures[t,,], 0)
		constraints = add.constraints(rbind(temp, -temp, -diag(nfactors)), rep(0, nfactors), type = '=', constraints)
			
		#--------------------------------------------------------------------------
		# Add binary constraints	
		#--------------------------------------------------------------------------	
		# adjust prior constraints: add b.i
		constraints = add.variables(n, constraints)
	
		# index of binary variables b.i
		constraints$binary.index = (2*n + nfactors + 1):(3*n + nfactors)
	
		# binary variable b.i : x.long < b, x.short < (1 - b)
		# x.long < b
		constraints = add.constraints(rbind(diag(n), 0*diag(n), matrix(0,nfactors,n), -diag(n)), rep(0, n), type = '<=', constraints)

		# x.short < (1 - b)
		constraints = add.constraints(rbind(0*diag(n), diag(n), matrix(0,nfactors,n), diag(n)), rep(1, n), type = '<=', constraints)

		#--------------------------------------------------------------------------
		# set expected return
		#--------------------------------------------------------------------------	
		# set expected return
		alpha = factors.avg$AVG[t,] / 5
		temp = ifna(coredata(alpha),0)
		expected.return = c(temp, -temp, rep(0, nfactors), rep(0, n))

		#--------------------------------------------------------------------------
		# Create Covariance matrix
		# [Qu  0]
		# [ 0 Qf]
		#--------------------------------------------------------------------------
		temp = diag(n)
			diag(temp) = ifna(specific.variance[t,], mean(coredata(specific.variance[t,]), na.rm=T))^2
			
		# | cov -cov |
		# |-cov  cov |		
		temp = cbind( rbind(temp, -temp), rbind(-temp, temp) )
		
		cov.temp = 0*diag(2*n + nfactors + n)
			cov.temp[1:(2*n),1:(2*n)] = temp
		cov.temp[(2*n+1):(2*n+nfactors),(2*n+1):(2*n+nfactors)] = factor.covariance[t,,]
	
		#--------------------------------------------------------------------------
		# Adjust Covariance matrix
		#--------------------------------------------------------------------------
		if(!is.positive.definite(cov.temp)) {
			cov.temp <- make.positive.definite(cov.temp, 0.000000001)
		}	
		
		#--------------------------------------------------------------------------
		# page 9, Risk: We use the Barra default setting, risk aversion value of 0.0075, and
		# AS-CF risk aversion ratio of 1.
		#
		# The Effects of Risk Aversion on Optimization (2010) by S. Liu, R. Xu
		# page 4/5
		#--------------------------------------------------------------------------
		risk.aversion = 0.0075
			
		# remove companies that have no beta from optimization
		index = which(is.na(beta[t,]))
		if( len(index) > 0) {
			constraints$ub[index] = 0
			constraints$lb[index] = 0
			constraints$ub[2*index] = 0
			constraints$lb[2*index] = 0			
		}
	
		# find solution
		sol = solve.QP.bounds(Dmat = 2* risk.aversion * cov.temp, dvec = expected.return, 
					Amat = constraints$A, bvec = constraints$b, 
					meq = constraints$meq, lb = constraints$lb, ub = constraints$ub,
					binary.vec = constraints$binary.index)					
					
		if(constraints$binary.index[1] != 0) cat(sol$counter,'QP calls made to solve problem with', len(constraints$binary.index), 'binary variables using Branch&Bound', '\n')		
	
		x = sol$solution[1:n] - sol$solution[(n+1):(2*n)]
		weights$market.neutral.alpha[t,] = x
		
		cat(t, '\n')
	}
		
	#save(weights, file='fm.long.short.test.Rdata')
		
	#*****************************************************************
	# Plot Transition Maps
	#****************************************************************** 		
png(filename = 'plot1.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')										
	layout(1:5)
	for(i in names(weights)) plotbt.transition.map(weights[[i]], i)	
dev.off()	
	
		
	#*****************************************************************
	# Create strategies
	#****************************************************************** 	
	prices = data$prices
		prices = bt.apply.matrix(prices, function(x) ifna.prev(x))
		
	# find month ends
	month.ends = endpoints(prices, 'months')

	# create strategies that invest in each qutile
	models = list()
	
	for(i in names(weights)) {
		data$weight[] = NA
			data$weight[month.ends,] = weights[[i]]
			capital = 100000
			data$weight[] = (capital / prices) * (data$weight)	
		models[[i]] = bt.run(data, type='share', capital=capital)
	}
			
	#*****************************************************************
	# Create Report
	#****************************************************************** 	
	models = rev(models)
	
	
png(filename = 'plot2.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')										
	plotbt.custom.report.part1(models, dates='1998::')
dev.off()	


png(filename = 'plot3.png', width = 1200, height = 800, units = 'px', pointsize = 12, bg = 'white')	
	plotbt.custom.report.part2(models)
dev.off()	
	
	
png(filename = 'plot4.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')		
	# Plot Portfolio Turnover for each strategy
	layout(1)
	barplot.with.labels(sapply(models, compute.turnover, data), 'Average Annual Portfolio Turnover')
dev.off()	

}


###############################################################################
# Dow Jones Historical Components
# http://en.wikipedia.org/wiki/Historical_components_of_the_Dow_Jones_Industrial_Average
#' @export 
###############################################################################
fm.hist.dow.jones.components <- function()
{
	tickers = spl('MMM	AA	MO	AXP	T	BAC	CAT	CVX	CSCO	C	KO	DD	XOM	GE	HPQ	HD	HON	INTC	IBM	IP	JNJ	JPM	KFT	MCD	MRK	MSFT	PFE	PG	BA	TRV	UTX	VZ	WMT	DIS', '\t')
	
	data = 
'8-Jun-09	1	1	0	1	1	1	1	1	1	0	1	1	1	1	1	1	0	1	1	0	1	1	1	1	1	1	1	1	1	1	1	1	1	1
22-Sep-08	1	1	0	1	1	1	1	1	0	1	1	1	1	1	1	1	0	1	1	0	1	1	1	1	1	1	1	1	1	0	1	1	1	1
19-Feb-08	1	1	0	1	1	1	1	1	0	1	1	1	1	1	1	1	0	1	1	0	1	1	0	1	1	1	1	1	1	0	1	1	1	1
21-Nov-05	1	1	1	1	1	0	1	0	0	1	1	1	1	1	1	1	1	1	1	0	1	1	0	1	1	1	1	1	1	0	1	1	1	1
8-Apr-04	1	1	1	1	1	0	1	0	0	1	1	1	1	1	1	1	1	1	1	0	1	1	0	1	1	1	1	1	1	0	1	1	1	1
27-Jan-03	1	1	1	1	1	0	1	0	0	1	1	1	1	1	1	1	1	1	1	1	1	1	0	1	1	1	0	1	1	0	1	0	1	1
1-Nov-99	1	1	1	1	1	0	1	0	0	1	1	1	1	1	1	1	1	0	1	1	1	1	0	1	1	1	0	1	1	0	1	0	1	1
17-Mar-97	1	1	1	1	1	0	1	1	0	1	1	1	1	1	1	0	0	0	1	1	1	1	0	1	1	0	0	1	1	0	1	0	1	1
6-May-91	1	1	1	1	1	0	1	1	0	1	1	1	1	1	1	0	0	0	1	1	1	1	0	1	1	0	0	1	1	0	1	0	1	1'


	hist = matrix( spl( gsub('\n', '\t', data), '\t'), nrow = len(spl(data, '\n')), byrow=TRUE)			
	hist = as.xts( matrix(as.double(hist[,-1]),nr=nrow(hist)), as.Date(hist[,1], '%d-%b-%y'))
		colnames(hist) = tickers
	return(hist)
}
										