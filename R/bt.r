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
# Backtest Functions
# Copyright (C) 2011  Michael Kapler
#
# For more information please visit my blog at www.SystematicInvestor.wordpress.com
# or drop me a line at TheSystematicInvestor at gmail
###############################################################################


###############################################################################
# Align dates, faster version of merge function
###############################################################################
bt.merge <- function
(
	b,				# enviroment with symbols time series
	align = c('keep.all', 'remove.na'),	# alignment type
	dates = NULL	# subset of dates
) 
{
	align = align[1]
	symbolnames = b$symbolnames
	nsymbols = len(symbolnames) 
	
	# merge
	temp = matrix(NA, nsymbols, 2)
	for( i in 1:nsymbols ) {
		idate = attr(b[[ symbolnames[i] ]], 'index')
		temp[i,1] = idate[ 1 ]
		temp[i,2] = idate[ len(idate) ]
	}
	class(temp)='POSIXct' 
	temp = as.Date(range(temp))
	
	if(!is.null(dates)) { 
		idate = seq(temp[1], temp[2], by=1)
		temp = make.xts(1:len(idate),idate) 
		
		temp = temp[dates] 
		temp = range( attr(temp, 'index') )
		class(temp) = 'POSIXct'
		temp = as.Date(range(temp))
	}
	
	# find business days
	all.dates = seq(temp[1], temp[2], by=1)
#		week.dates = date.dayofweek(all.dates)
#		all.dates = all.dates[!(week.dates == 0 | week.dates == 6)]
	
	# date map
	date.map = matrix(NA, nr = len(all.dates), nsymbols)
	for( i in 1:nsymbols ) {
		idate = attr(b[[ symbolnames[i] ]], 'index')
			class(idate) = 'POSIXct'
			idate = as.Date(idate)
		
		index = match(idate, all.dates)
		sub.index = which(!is.na(index))
		date.map[ index[sub.index], i] = sub.index
	}
 
	if( align == 'remove.na' ) { 
		index = which(count(date.map, side=1) < nsymbols )
	} else {
		index = which(count(date.map, side=1) < max(1, 0.1 * nsymbols) )
	}
	
	if(len(index) > 0) { 
		date.map = date.map[-index,, drop = FALSE]
		all.dates = all.dates[-index] 
	}
	
	return( list(all.dates = all.dates, date.map = date.map))
}



###############################################################################
# Prepare backtest data
###############################################################################
bt.prep <- function
(
	b,				# enviroment with symbols time series
	align = c('keep.all', 'remove.na'),	# alignment type
	dates = NULL	# subset of dates
) 
{    
	# setup
	if( !exists('symbolnames', b, inherits = F) ) b$symbolnames = ls(b)
	symbolnames = b$symbolnames
	nsymbols = len(symbolnames) 
	
	if( nsymbols > 1 ) {
		# merge
		out = bt.merge(b, align, dates)
		for( i in 1:nsymbols ) {
			b[[ symbolnames[i] ]] = 
				make.xts( coredata( b[[ symbolnames[i] ]] )[ out$date.map[,i],, drop = FALSE], out$all.dates)
		}	
	} else {
		if(!is.null(dates)) b[[ symbolnames[1] ]] = b[[ symbolnames[1] ]][dates,]	
		out = list(all.dates = index(b[[ symbolnames[1] ]]) )
	}

	# dates
	b$dates = out$all.dates
		   
	# empty matrix		
	dummy = matrix(double(), len(out$all.dates), nsymbols)
		colnames(dummy) = symbolnames
		dummy = make.xts(dummy, out$all.dates)
		
	# weight matrix holds signal and weight information		
	b$weight = dummy
	
	# execution price, if null use Close	
	b$execution.price = dummy
		
	# populate prices matrix
	for( i in 1:nsymbols ) {
		if( has.Cl( b[[ symbolnames[i] ]] ) ) {
			dummy[,i] = Cl( b[[ symbolnames[i] ]] );
		}
	}
	b$prices = dummy	
}

# matrix form
bt.prep.matrix <- function
(
	b,				# enviroment with symbols time series
	align = c('keep.all', 'remove.na'),	# alignment type
	dates = NULL	# subset of dates
)
{    
	align = align[1]
	nsymbols = len(b$symbolnames)
	
	# merge
	if(!is.null(dates)) { 	
		temp = make.xts(1:len(b$dates), b$dates)
		temp = temp[dates] 
		index = as.vector(temp)
		
		for(i in b$fields) b[[ i ]] = b[[ i ]][index,, drop = FALSE]
		
		b$dates = b$dates[index]
	}
 
	if( r_align == 'remove.na' ) { 
		index = which(count(b$Cl, side=1) < nsymbols )
	} else {
		index = which(count(b$Cl,side=1) < max(1,0.1 * nsymbols) )
	}
	
	if(len(index) > 0) { 
		for(i in b$fields) b[[ i ]] = b[[ i ]][-index,, drop = FALSE]
		
		b$dates = b$dates[-index]
	}
	
	# empty matrix		
	dummy = make.xts(b$Cl, b$dates)
		
	# weight matrix holds signal and weight information		
	b$weight = NA * dummy
	
	b$execution.price = NA * dummy
	
	b$prices = dummy
}


###############################################################################
# Remove symbols that has less than minHistory
###############################################################################
bt.prep.remove.symbols <- function
(
	b, 					# enviroment with symbols time series
	min.history = 1000	# minmum number of observations
) 
{
	index = which( count(b$prices, side=2) < min.history )
	if( len(index) > 0 ) {
		b$prices = b$prices[, -index]
		b$weight = b$weight[, -index]
		b$execution.price = b$execution.price[, -index]
		
		rm(list = b$symbolnames[index], envir = b)		
		b$symbolnames = b$symbolnames[ -index]
	}
}


###############################################################################
# Run backtest
###############################################################################
# some operators do not work well on xts
# weight[] = apply(coredata(weight), 2, ifna_prev)
###############################################################################
bt.run <- function
(
	b,					# enviroment with symbols time series
	trade.summary = F, 	# flag to create trade summary
	do.lag = 1, 		# lag signal
	do.CarryLastObservationForwardIfNA = TRUE, 
	type = c('weight', 'share'),
	silent = F,
	capital = 100000
) 
{
	# setup
	type = type[1]

	# print last signal / weight observation
	if( !silent ) {
		cat('Latest weights :\n')
			print( last(b$weight) )
		cat('\n')
	}
		
    # create signal
    weight = b$weight
    weight[] = ifna(weight, NA)
    
    # lag
    if(do.lag > 0) {
		weight = mlag(weight, do.lag) # Note k=1 implies a move *forward*  
	}

	# backfill
	if(do.CarryLastObservationForwardIfNA) {			
		weight[] = apply(coredata(weight), 2, ifna.prev)
    }
	weight[is.na(weight)] = 0

	
	# find trades
	weight1 = mlag(weight, -1)
	tstart = weight != weight1 & weight1 != 0
	tend = weight != 0 & weight != weight1
		trade = ifna(tstart | tend, FALSE)
	
	# prices
	prices = b$prices
	
	# execution.price logic
	if( sum(trade) > 0 ) {
		execution.price = coredata(b$execution.price)
		prices1 = coredata(b$prices)
		
		prices1[trade] = iif( is.na(execution.price[trade]), prices1[trade], execution.price[trade] )
		prices[] = prices1
	}
		
	# type of backtest
	if( type == 'weight') {
		ret = prices / mlag(prices) - 1
		ret[] = ifna(ret, NA)
		ret[is.na(ret)] = 0			
	} else { # shares, hence provide prices
		ret = prices
	}
	
	weight = make.xts(weight, b$dates)

	# prepare output
	bt = list()
		bt = bt.summary(weight, ret, type, capital)

	if( trade.summary ) bt$trade.summary = bt.trade.summary(b, bt)

	if( !silent ) {
		cat('Performance summary :\n')
		cat('', spl('CAGR,Best,Worst'), '\n', sep = '\t')  
    	cat('', sapply(cbind(bt$cagr, bt$best, bt$worst), function(x) round(100*x,1)), '\n', sep = '\t')  
		cat('\n')    
	}
	    
	return(bt)
}


###############################################################################
# Backtest summary
###############################################################################
bt.summary <- function
(
	weight, 	# signal / weights matrix
	ret, 		# returns for type='weight' and prices for type='share'
	type = c('weight', 'share'),
	capital = 100000
) 
{
	type = type[1]
    n = nrow(ret)
	     	
    bt = list()
    	bt$weight = weight
    	bt$type = type
    	
	if( type == 'weight') {    	    	
    	bt$ret = make.xts(rowSums(ret * weight), index(ret))
    	
    } else {
    	bt$share = weight
    	bt$capital = capital
    	prices = ret
    		
    	# backfill prices
		prices1 = coredata(prices)
		prices1[is.na(prices1)] = ifna(mlag(prices1), NA)[is.na(prices1)]				
		prices[] = prices1
		
		# new logic
		cash = capital - rowSums(bt$share * mlag(prices), na.rm=T)
		
			# find trade dates
			share.nextday = mlag(bt$share, -1)
			tstart = bt$share != share.nextday & share.nextday != 0
			
			index = mlag(apply(tstart, 1, any))
				index = ifna(index, FALSE)
								
			totalcash = NA * cash
				totalcash[index] = cash[index]
			totalcash = ifna.prev(totalcash)
		
		portfolio.ret = (totalcash + rowSums(bt$share * prices, na.rm=T) ) / (totalcash + rowSums(bt$share * mlag(prices), na.rm=T) ) - 1		
		bt$weight = bt$share * mlag(prices) / (totalcash + rowSums(bt$share * mlag(prices), na.rm=T) )
		

		
		bt$weight[is.na(bt$weight)] = 0		
		bt$ret = make.xts(ifna(portfolio.ret,0), index(ret))
    }
    	
    bt$best = max(bt$ret)
    bt$worst = min(bt$ret)
    bt$equity = cumprod(1 + bt$ret)
    bt$cagr = compute.cagr(bt$equity)
    	
    return(bt)    
}

###############################################################################
# Portfolio turnover	
# http://wiki.fool.com/Portfolio_turnover
# sales or purchases and dividing it by the average monthly value of the fund's assets
###############################################################################
compute.turnover <- function
(	
	bt,		# backtest object
	b 		# enviroment with symbols time series
) 
{ 
	year.ends =  unique(c(endpoints(index(bt$weight), 'years'), nrow(bt$weight)))	
		year.ends = year.ends[year.ends>0]	
		nr = len(year.ends)
	period.index = c(1, year.ends)

	
	if( bt$type == 'weight') {    	    	
		portfolio.value = rowSums(abs(bt$weight), na.rm=T)
		portfolio.turnover = rowSums( abs(bt$weight - mlag(bt$weight)) )
	} else {
		# logic from bt.summary function				
		cash = bt$capital - rowSums(bt$share * mlag(b$prices), na.rm=T)
		
			# find trade dates
			share.nextday = mlag(bt$share, -1)
			tstart = bt$share != share.nextday & share.nextday != 0
			
			index = mlag(apply(tstart, 1, any))
				index = ifna(index, FALSE)
								
			totalcash = NA * cash
				totalcash[index] = cash[index]
			totalcash = ifna.prev(totalcash)
		
		portfolio.value = totalcash + rowSums(bt$share * mlag(b$prices), na.rm=T)		
		
		portfolio.turnover = rowSums( mlag(b$prices) * abs(bt$share - mlag(bt$share)) )				
	}
	
	portfolio.turnover[1:2] = 0
	temp = NA * period.index			
	for(iyear in 2:len(period.index)) {
		temp[iyear] = sum( portfolio.turnover[ period.index[(iyear-1)] : period.index[iyear] ], na.rm=T) / 
						mean( portfolio.value[ period.index[(iyear-1)] : period.index[iyear] ], na.rm=T)			
	}
	return( mean(temp, na.rm=T) )			
}


###############################################################################
# Compute Portfolio Maximum Deviation
###############################################################################
compute.max.deviation <- function
(
	bt,
	target.allocation
)
{
	weight = bt$weight[-1,]
	max(abs(weight - repmat(target.allocation, nrow(weight), 1)))
}


###############################################################################
# Backtest Trade summary
###############################################################################
bt.trade.summary <- function
(
	b, 		# enviroment with symbols time series
	bt		# backtest object
)
{    
	if( bt$type == 'weight') weight = bt$weight else weight = bt$share
	
	out = NULL
	
	# find trades
	weight1 = mlag(weight, -1)
	tstart = weight != weight1 & weight1 != 0
	tend = weight != 0 & weight != weight1
		trade = ifna(tstart | tend, FALSE)
	
	# prices
	prices = b$prices
	
	# execution price logic
	if( sum(trade) > 0 ) {
		execution.price = coredata(b$execution.price)
		prices1 = coredata(b$prices)
		
		prices1[trade] = iif( is.na(execution.price[trade]), prices1[trade], execution.price[trade] )
		
		# backfill pricess
		prices1[is.na(prices1)] = ifna(mlag(prices1), NA)[is.na(prices1)]				
		prices[] = prices1
			
		# get actual weights
		weight = bt$weight
	
		# extract trades
		symbolnames = b$symbolnames
		nsymbols = len(symbolnames) 	

		trades = c()
		for( i in 1:nsymbols ) {	
			tstarti = which(tstart[,i])
			tendi = which(tend[,i])
			
			if( len(tstarti) > 0 ) {
				if( len(tendi) < len(tstarti) ) tendi = c(tendi, nrow(weight))
				
				trades = rbind(trades, 
								cbind(i, weight[(tstarti+1), i], 
								tstarti, tendi, 
								as.vector(prices[tstarti, i]), as.vector(prices[tendi,i])
								)
							)
			}
		}
		colnames(trades) = spl('symbol,weight,entry.date,exit.date,entry.price,exit.price')

		# prepare output		
		out = list()
		out$stats = cbind(
			bt.trade.summary.helper(trades),
			bt.trade.summary.helper(trades[trades[, 'weight'] >= 0, ]),
			bt.trade.summary.helper(trades[trades[, 'weight'] <0, ])
		)
		colnames(out$stats) = spl('All,Long,Short')
		
		trades = data.frame(coredata(trades))
			trades$symbol = symbolnames[trades$symbol]
			trades$entry.date = index(weight)[trades$entry.date]
			trades$exit.date = index(weight)[trades$exit.date]
			trades$return = round(100*(trades$weight) * (trades$exit.price/trades$entry.price - 1),2)			
			trades$entry.price = round(trades$entry.price, 2)
			trades$exit.price = round(trades$exit.price, 2)			
			trades$weight = round(100*(trades$weight),1)		

		out$trades = as.matrix(trades)		
	}
	
	return(out)
}

# helper function
bt.trade.summary.helper <- function(trades) 
{		
	if(nrow(trades) <= 0) return(NA)
	
	out = list()
		tpnl = trades[, 'weight'] * (trades[, 'exit.price'] / trades[,'entry.price'] - 1)
		tlen = trades[, 'exit.date'] - trades[, 'entry.date']
		
	out$ntrades = nrow(trades)
	out$avg.pnl = mean(tpnl)
	out$len = mean(tlen)
		
	out$win.prob = len(which( tpnl > 0 )) / out$ntrades
	out$win.avg.pnl = mean( tpnl[ tpnl > 0 ])
	out$win.len = mean( tlen[ tpnl > 0 ])
		
	out$loss.prob = 1 - out$win.prob
	out$loss.avg.pnl = mean( tpnl[ tpnl < 0 ])
	out$loss.len = mean( tlen[ tpnl < 0 ])
		
	#Van Tharp : Expectancy = (PWin * AvgWin) - (PLoss * AvgLoss)			
	out$expectancy = (out$win.prob * out$win.avg.pnl + out$loss.prob * out$loss.avg.pnl)/100
			
	# Profit Factor is computed as follows: (PWin * AvgWin) / (PLoss * AvgLoss)
	out$profitfactor = -(out$win.prob * out$win.avg.pnl) / (out$loss.prob * out$loss.avg.pnl)			
			
	return(as.matrix(unlist(out)))
}		


###############################################################################
# Apply given function to bt enviroment
###############################################################################
bt.apply <- function
(
	b,			# enviroment with symbols time series
	xfun=Cl,	# user specified function
	...			# other parameters
)
{
	out = b$weight
	out[] = NA
	
	symbolnames = b$symbolnames
	nsymbols = length(symbolnames) 
	
	for( i in 1:nsymbols ) {	
		msg = try( match.fun(xfun)( coredata(b[[ symbolnames[i] ]]),... ) , silent=TRUE)
		if (class(msg)[1] != 'try-error') {
			out[,i] = msg
		} else {
			cat(i, msg, '\n')
		}
	}
	return(out)
}

bt.apply.matrix <- function
(
	b,			# matrix
	xfun=Cl,	# user specified function
	...			# other parameters
)
{
	out = b
	out[] = NA
	nsymbols = ncol(b)
	
	for( i in 1:nsymbols ) {	
		msg = try( match.fun(xfun)( coredata(b[,i]),... ) , silent=TRUE);
		if (class(msg)[1] != 'try-error') {
			out[,i] = msg
		} else {
			cat(i, msg, '\n')
		}
	}
	return(out)
}



###############################################################################
# Remove excessive signal
###############################################################################
bt.exrem <- function(weight)
{
	bt.apply.matrix(weight, function(x) {
		temp = ifna(ifna.prev(x),0)
			itemp = which(temp != mlag(temp))
		x[] = NA 
		x[itemp] = temp[itemp]
		return(x)
	})
}	

###############################################################################
# Timed Exit: Remove excessive signal
###############################################################################
bt.exrem.time.exit <- function(signal, nlen) {
	signal[is.na(signal)] = FALSE
	signal.index = which(signal)			
	nperiods = len(signal)	
	
	for(i in signal.index) {
		if( signal[i] ) {
			signal[ (i+1) : iif(i + nlen - 1 > nperiods, nperiods, i + nlen - 1) ] = FALSE
		}
	}
	return(signal)
}




###############################################################################
# Backtest Test function
###############################################################################
bt.test <- function()
{
	load.packages('quantmod')
	
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	
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

	# MA Cross
	sma = bt.apply(data, function(x) { SMA(Cl(x), 200) } )	
	data$weight[] = NA
		data$weight[] = iif(prices >= sma, 1, 0)
	sma.cross = bt.run(data, trade.summary=T)			

	#*****************************************************************
	# Create Report
	#****************************************************************** 
		
					
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')										
	plotbt.custom.report.part1( sma.cross, buy.hold)			
dev.off()	


png(filename = 'plot2.png', width = 1200, height = 800, units = 'px', pointsize = 12, bg = 'white')	
	plotbt.custom.report.part2( sma.cross, buy.hold)			
dev.off()	
	

png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')	
	plotbt.custom.report.part3( sma.cross, buy.hold)			
dev.off()	




	# put all reports into one pdf file
	pdf(file = 'report.pdf', width=8.5, height=11)
		plotbt.custom.report(sma.cross, buy.hold, trade.summary=T)
	dev.off()	

	#*****************************************************************
	# Code Strategies
	#****************************************************************** 
	
	data$weight[] = NA
		data$weight$SPY = 1
	temp = bt.run(data)

	data$weight[] = NA
		data$weight$SPY = 2
	temp = bt.run(data)

	data$weight[] = NA
		data$weight$SPY = 1
		capital = 100000
		data$weight[] = (capital / prices) * data$weight
	temp = bt.run(data, type='share', capital=capital)

	data$weight[] = NA
		data$weight$SPY = 2
		capital = 100000
		data$weight[] = (capital / prices) * data$weight
	temp = bt.run(data, type='share', capital=capital)
	
}


###############################################################################
# Analytics Functions
###############################################################################
# CAGR - geometric return
###############################################################################
compute.cagr <- function(equity) 
{ 
	as.double( last(equity,1)^(1/compute.nyears(equity)) - 1 )
}

compute.nyears <- function(x) 
{
	as.double(diff(as.Date(range(index(x)))))/365
}

# 252 - days, 52 - weeks, 26 - biweeks, 12-months, 6,4,3,2,1
compute.annual.factor = function(x) {
	possible.values = c(252,52,26,13,12,6,4,3,2,1)
	index = which.min(abs( nrow(x) / compute.nyears(x) - possible.values ))
	round( possible.values[index] )
}

compute.sharpe <- function(x) 
{ 
	temp = compute.annual.factor(x)
	x = as.vector(coredata(x))
	return(sqrt(temp) * mean(x)/sd(x) )
}

# R2 equals the square of the correlation coefficient
compute.R2 <- function(equity) 
{
	x = as.double(index(equity))
	y = equity
	#summary(lm(y~x))
	return( cor(y,x)^2 )
}

# http://cssanalytics.wordpress.com/2009/10/15/ft-portfolio-with-dynamic-hedging/
# DVR is the Sharpe Ratio times the R-squared of the equity curve
compute.DVR <- function(bt) 
{
	return( compute.sharpe(bt$ret) * compute.R2(bt$equity) )
}

compute.risk <- function(x) 
{ 
	temp = compute.annual.factor(x)
	x = as.vector(coredata(x))
	return( sqrt(temp)*sd(x) ) 
}

compute.drawdown <- function(x) 
{ 
	return(x / cummax(x) - 1)
}

compute.max.drawdown <- function(x) 
{ 
	as.double( min(compute.drawdown(x)) )
}

compute.avg.drawdown <- function(x) 
{ 
	drawdown = c( compute.drawdown(coredata(x)), 0 )
	dstart = which( drawdown == 0 & mlag(drawdown, -1) != 0 )
	dend = which(drawdown == 0 & mlag(drawdown, 1) != 0 )
	mean(apply( cbind(dstart, dend), 1, function(x){ min(drawdown[ x[1]:x[2] ], na.rm=T) } ))
}


compute.exposure <- function(weight) 
{ 
	sum( apply(weight, 1, function(x) sum(x != 0) ) != 0 ) / nrow(weight) 
}

compute.var <- function(x, probs=0.05) 
{ 
	quantile( coredata(x), probs=probs)
}

compute.cvar <- function(x, probs=0.05) 
{ 
	x = coredata(x)
	mean( x[ x < quantile(x, probs=probs) ] )
}


compute.stats <- function(data, fns) 
{
	out = matrix(double(), len(fns), len(data))
		colnames(out) = names(data)
		rownames(out) = names(fns)
	for(c in 1:len(data)) {
		for(r in 1:len(fns)) {
			out[r,c] = match.fun(fns[[r]])( na.omit(data[[c]]) )
		}
	}
	return(out)
}


###############################################################################
# Example to illustrate a simeple backtest
###############################################################################
bt.simple <- function(data, signal) 
{
	# lag singal
	signal = Lag(signal, 1)

	# back fill
    signal = na.locf(signal, na.rm = FALSE)
	signal[is.na(signal)] = 0

	# calculate Close-to-Close returns
	ret = ROC(Cl(data))
	ret[1] = 0
	
	# compute stats	
    n = nrow(ret)
    bt <- list()
    	bt$ret = ret * signal
    	bt$best = max(bt$ret)
    	bt$worst = min(bt$ret)
    	bt$equity = cumprod(1 + bt$ret)
    	bt$cagr = bt$equity[n] ^ (1/nyears(data)) - 1
    
    # print
	cat('', spl('CAGR,Best,Worst'), '\n', sep = '\t')  
    cat('', sapply(cbind(bt$cagr, bt$best, bt$worst), function(x) round(100*x,1)), '\n', sep = '\t')  
    	    	
	return(bt)
}

bt.simple.test <- function()
{
	load.packages('quantmod')
	
	# load historical prices from Yahoo Finance
	data = getSymbols('SPY', src = 'yahoo', from = '1980-01-01', auto.assign = F)

	# Buy & Hold
	signal = rep(1, nrow(data))
    buy.hold = bt.simple(data, signal)
        
	# MA Cross
	sma = SMA(Cl(data),200)
	signal = ifelse(Cl(data) > sma, 1, 0)
    sma.cross = bt.simple(data, signal)
        
	# Create a chart showing the strategies perfromance in 2000:2009
	dates = '2000::2009'
	buy.hold.equity <- buy.hold$equity[dates] / as.double(buy.hold$equity[dates][1])
	sma.cross.equity <- sma.cross$equity[dates] / as.double(sma.cross$equity[dates][1])

	chartSeries(buy.hold.equity, TA=c(addTA(sma.cross.equity, on=1, col='red')),	
	theme ='white', yrange = range(buy.hold.equity, sma.cross.equity) )	
}



