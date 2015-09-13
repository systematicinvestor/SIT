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
# Copyright (C) 2015 Systematic Investor
#
# For more information please visit my blog at www.SystematicInvestor.wordpress.com
# or drop me a line at TheSystematicInvestor at gmail
###############################################################################


###############################################################################
#' New Share Back Test functionality to properly capture portfolio changes.
#' This is an event driven back test that updates portfolio at every trade event
#'
#' Summary:
#' * store actual positions and cash at the end of the day,
#' hence if we are rebalacing on T, the positions will be shown on T
#' * for weights, use weights from T-1, to properly reflect weights throughout day
#'
#' @export 
###############################################################################
bt.run.share.ex <- function
(
	b,					# enviroment with symbols time series
	prices = b$prices,	# prices
	clean.signal = T,	# flag to remove excessive signal	
	trade.summary = F, 	# flag to create trade summary
	silent = F,
	capital = 100000,
	commission = 0,
	weight = b$weight,
	dates = 1:nrow(b$prices),
	lot.size = c(),
	control = list(
		round.lot = default.round.lot.control()
	)	
) 
{
	# make sure that prices are available, assume that
	# weights account for missing prices i.e. no price means no allocation
	prices[] = ifna( bt.apply.matrix(coredata(prices), ifna.prev), 1)

	weight = coredata(weight)
		temp = bt.exrem(weight)
	if(clean.signal) {
		weight = temp
	} else { # always clean up 0's
		index = ifna(weight == 0, F)
		weight[index] = temp[index]
	}
	# back filled weights
	weight1 = ifna( bt.apply.matrix(weight, ifna.prev), 0)
	
		
	# find trades
	trade = !is.na(weight)
		trade.index = which(rowSums(trade) > 0)
	
	# prices
	prices = coredata(b$prices)
		n = ncol(prices)
		nperiods = nrow(prices)
	
	
	# execution.price logic
	if( sum(trade) > 0 ) {
		execution.price = coredata(b$execution.price)
		prices1 = coredata(b$prices)
		
		prices1[trade] = iif( is.na(execution.price[trade]), prices1[trade], execution.price[trade] )
		prices[] = prices1
	}

	# validate commission
	if( !is.list(commission) )
		commission = list(cps = commission, fixed = 0.0, percentage = 0.0)	
		
	# validate lot.size
	if( len(lot.size) > 0 ) 
		lot.size = ifna(iif( len(lot.size) == 1, rep(lot.size, n), lot.size), 1)
		
	
	# setup event driven back test loop
	cash.wt = cash = rep(capital, nperiods)
	com = rep(0, nperiods)
	share.wt = share = matrix(0, nperiods, n)
	last.trade = 0
	weight.last = weight1[1,]
	
	for(i in trade.index) {		
		if(last.trade > 0) {
			# copy from last trade
			index = (last.trade + 1) : i
				n.index = len(index)
			share.wt[index,] = rep.row(share[last.trade,], n.index)
			share[index,] = rep.row(share[last.trade,], n.index)
			cash.wt[index] = cash[last.trade]
			cash[index] = cash[last.trade]
			
			weight.last = weight1[i-1,]
		}
					
		# update share[i,] and cash[i]
		out = bt.run.share.ex.allocate(weight.new = weight1[i,], weight.prev = weight.last,
			weight.change.index = !is.na(weight[i,]),
			price = prices[i,], share = share[i,], cash = cash[i],
			commission, lot.size, control = control$round.lot)
			
		share[i,] = out$share
		cash[i] = out$cash
		com[i] = out$com
		last.trade = i
	}
		
	if( last.trade > 0 & last.trade < nperiods) {
		# copy from last trade
		index = (last.trade + 1) : nperiods
			n.index = len(index)
		share.wt[index,] = rep.row(share[last.trade], n.index)
		share[index,] = rep.row(share[last.trade], n.index)
		cash.wt[index] = cash[last.trade]
		cash[index] = cash[last.trade]
	}
	
	
	# setup output structure
    bt = list(type = 'share', capital = capital)
	
bt$share = share
bt$cash = cash
bt$com = com
bt$value = cash + rowSums(share * prices)
	
	bt$weight = share.wt * prices / (cash.wt + rowSums(share.wt * prices))
	value = c(capital, cash + rowSums(share * prices))
	bt$ret = (value / mlag(value) - 1)[-1]
	
		# make ret -> equity, weight xts
		bt$ret = make.xts(bt$ret, index(b$prices))
		bt$weight = make.xts(bt$weight, index(b$prices))
		
	bankrupt = which(bt$ret <= -1)
	if(len(bankrupt) > 0) bt$ret[bankrupt[1]:n] = -1
        
    bt$equity = cumprod(1 + bt$ret)
    
	
	# convert dates to dates.index
	bt$dates.index = dates2index(b$prices, dates)
	
	# prepare output
	bt = bt.run.trim.helper(bt, bt$dates.index)

	if( trade.summary ) bt$trade.summary = bt.trade.summary(b, bt)

	if( !silent ) {
		# print last signal / weight observation
		cat('Latest weights :\n')
		print(round(100*last(bt$weight),2))
		cat('\n')

		cat('Performance summary :\n')
		cat('', spl('CAGR,Best,Worst'), '\n', sep = '\t')  
    	cat('', sapply(cbind(bt$cagr, bt$best, bt$worst), function(x) round(100*x,1)), '\n', sep = '\t')  
		cat('\n')    
	}
	    
	return(bt)
}
				

###############################################################################
#' default.round.lot.control	
#' @export 
###############################################################################
default.round.lot.control = function() {
	list(
	# selects allocation that has
		# best.match -  smallest absolute deviation from target allocation
		# minimum.turnover - minimum turnover and is within diff.target abs weight from best match
		select = c('best.match', 'minimum.turnover'), 
		diff.target = 5/100 # only used if select = 'minimum.turnover'
	)
}

###############################################################################
#' Do one period re balance
#'
#' @export 
###############################################################################
bt.run.share.ex.allocate = function
(
	weight.new,
	weight.prev,
	weight.change.index,
	price,
	share,
	cash,
	commission,
	lot.size,
	silent=T,
	# control allocation if round lot is enabled
	control = default.round.lot.control()
) {
	
	# total value, as if everything is liquidated
	value = sum(price * share) + cash

	
# helper functions	
compute.commission = function(share.prev, share.new, price, commission) {	
	if(is.null(dim(share.new))) {
		share.diff = abs(share.prev - share.new)
		return(
			sum(share.diff) * commission$cps + sum(sign(share.diff)) * commission$fixed + sum(price * share.diff) * commission$percentage
		)
	}
	
	share.prev = rep.row(share.prev, nrow(share.new))
	price = rep.row(price, nrow(share.new))

	share.diff = abs(share.prev - share.new)
	rowSums(share.diff) * commission$cps + rowSums(sign(share.diff)) * commission$fixed + rowSums(price * share.diff) * commission$percentage
}

compute.cash = function(value, share, price, com) {
	if(is.null(dim(share)))
		value - sum(price * share) - com
	else {
		price = rep.row(price, nrow(share))
		value - rowSums(price * share) - com
	}
}

compute.weight.diff = function(target, share, cash) {
	if(is.null(dim(share)))
		sum(abs(
			target - c(share * price, cash) / (sum(price * share) + cash)
		))
	else {
		price = rep.row(price, nrow(share))
		target = rep.row(target, nrow(share))
		rowSums(abs(
			target - cbind(share * price, cash) / (rowSums(price * share) + cash)
		))
	}
}
	

#new.cash = (1 - sum(weight.new)) * value
allocate = function(value, share) {	
	new.total.weight = sum(abs(weight.new[weight.change.index]))
	if(new.total.weight == 0)
		share[weight.change.index] = 0
	else {
		allocate.value = value * sum(abs(weight.new)) - sum(abs(share * price)[!weight.change.index])
			
		# weight = weight * (capital / prices)
		share[weight.change.index] = 
			allocate.value * (weight.new / price)[weight.change.index] / new.total.weight
	}
	share
}

allocate.lot = function(value, share, lot.size) {
	if(len(lot.size) == 0) return(allocate(value, share))
	
	new.total.weight = sum(abs(weight.new[weight.change.index]))
	if(new.total.weight == 0)
		share[weight.change.index] = 0
	else {
		allocate.value = value * sum(abs(weight.new)) - sum(abs(share * price)[!weight.change.index])
		lot.size = lot.size[weight.change.index]
		w = weight.new[weight.change.index]/ new.total.weight
		p = price[weight.change.index]
		
		shares = rep.row(share, 3)			
		shares[2, weight.change.index] = round.lot.basic(w, p, allocate.value, lot.size)			
		shares[3, weight.change.index] = round.lot.basic.base(w, p, allocate.value, lot.size)
		share = shares
	}
	share	
}



	
	# first allocate based on total value
	new.share = allocate(value, share)
	
	# compute commisions
	com = compute.commission(share, new.share, price, commission)

	# if commisions are due, allocate second time
	# asuming commisions are paid out from total value upfront
	if( com > 0 || len(lot.size) > 0 ) {
		# might need to set aside more, due to nonlinear nature of commisions
		# i.e. fixed commision was not active during first allocation, but is active during second one
		new.share = allocate.lot(value - 2 * com, share, lot.size)

		# create list of possible portfolios and compute commisions, cash, weight diff
		share1 = rbind(share, new.share)
			com1 = compute.commission(share, share1, price, commission)
			cash1 = compute.cash(value, share1, price, com1)
		
			target = c(weight.new, 1 - sum(weight.new))
			diff1 = compute.weight.diff(target, share1, cash1)

		# select one of the portfolios
		# weight diff best match
		j = which.min(diff1)

		# minimum turnover match that is within 5% abs weight diff from best match
		if( control$select[1] == 'minimum.turnover' ) {
			j1 = which(diff1 - diff1[j] <= control$diff.target)
			j = j1[which.min(com1[j1])]
		}
		
		# select one
		new.share = share1[j,]
		com = com1[j]
	}
	
	
	# assume that total value does not change i.e.
	# value = sum(price * share) + cash = sum(price * new.share) + new.cash + com
	new.cash = value - sum(price * new.share) - com
	
if(!silent) {
	# check that value before and value after are the same factoring in commisions
	cat('Old[T,V,C]', sum(price * share) + cash, sum(price * share), cash, '\n',
		'New [T,V,C,COM]', sum(price * new.share) + new.cash + com, sum(price * new.share), new.cash, com, '\n')
		
	# check that final weights are similar to desired weights
	cat('Old Weight', weight.new, '\n',
		'New Weight', price * new.share / (sum(price * new.share) + new.cash), '\n')
}

	list(share = new.share, cash = new.cash, com = com)
}	
	




# Tests
bt.run.share.ex.allocate.test = function() {
	
	# Start with Cash 
	commission = list(cps = 0.0, fixed = 0.0, percentage = 0.0)
	commission = list(cps = 0.01, fixed = 1.0, percentage = 0.0)
	weight.prev = c(0,0) / 10
	share = c(0, 0)
	price = c(1,2)
	cash = 100
	lot.size=c()
	
	weight.new = c(10,0) / 10
	weight.change.index = c(T, T)
	a = bt.run.share.ex.allocate(weight.new,weight.prev,weight.change.index,price,share,cash,commission,lot.size,F)
	
	weight.new = c(-10,0) / 10
	weight.change.index = c(T, T)
	a = bt.run.share.ex.allocate(weight.new,weight.prev,weight.change.index,price,share,cash,commission,lot.size,F)
	
	weight.new = c(13,-3) / 10
	weight.change.index = c(T, T)
	a = bt.run.share.ex.allocate(weight.new,weight.prev,weight.change.index,price,share,cash,commission,lot.size, F)
	
	weight.new = c(2,8) / 10
	weight.change.index = c(T, T)
	a = bt.run.share.ex.allocate(weight.new,weight.prev,weight.change.index,price,share,cash,commission,lot.size)
	
	weight.new = c(0,8) / 10
	weight.change.index = c(T, T)
	a = bt.run.share.ex.allocate(weight.new,weight.prev,weight.change.index,price,share,cash,commission,lot.size)
	
	weight.new = c(0,8) / 10
	weight.change.index = c(F, T)
	a = bt.run.share.ex.allocate(weight.new,weight.prev,weight.change.index,price,share,cash,commission,lot.size)
	
	
	weight.new = c(-10,0) / 10
	weight.change.index = c(T, T)
	a = bt.run.share.ex.allocate(weight.new,weight.prev,weight.change.index,price,share,cash,commission,lot.size)
	
	weight.new = c(-10,0) / 10
	weight.change.index = c(T, F)
	a = bt.run.share.ex.allocate(weight.new,weight.prev,weight.change.index,price,share,cash,commission,lot.size)
	
	weight.new = c(13,-3) / 10
	weight.change.index = c(T, T)
	a = bt.run.share.ex.allocate(weight.new,weight.prev,weight.change.index,price,share,cash,commission,lot.size)
	
	weight.new = c(-10,10) / 10
	weight.change.index = c(T, T)
	a = bt.run.share.ex.allocate(weight.new,weight.prev,weight.change.index,price,share,cash,commission,lot.size)
	
	weight.new = c(10,10) / 10
	weight.change.index = c(T, T)
	a = bt.run.share.ex.allocate(weight.new,weight.prev,weight.change.index,price,share,cash,commission,lot.size)
	
	# Start with Allocation		
	weight.new = c(2,8) / 10
	weight.prev = c(0,8) / 10
	weight.change.index = c(T, F)
	price = c(1,2)
	share = c(0, 40)
	cash = 20
	
	a = bt.run.share.ex.allocate(weight.new,weight.prev,weight.change.index,price,share,cash,commission,lot.size)
	
	weight.new = c(2,8) / 10
	weight.prev = c(0,8) / 10
	weight.change.index = c(T, T)
	price = c(1,2)
	share = c(0, 40)
	cash = 20
	
	a = bt.run.share.ex.allocate(weight.new,weight.prev,weight.change.index,price,share,cash,commission,lot.size)
	
}



###############################################################################	
#' Round Lot
#' a helper function for round.lot.basic
#' @export 
###############################################################################
round.lot = function(weight, price, capital, lot.size) {
	weight = coredata(ifna(weight, 0))
	price = coredata(ifna(price, 1)) 	
	lot.size = ifna(iif( len(lot.size) == 1, rep(lot.size, ncol(weight)), lot.size), 1)

	round.lot.basic(weight, price, capital, lot.size)
}

###############################################################################	
#' Round Lot Basic Base
#' round lot to the nearest lot.size
#' @export 
###############################################################################
round.lot.basic.base = function(weight, price, capital, lot.size) {
	sign(weight) * floor(abs(weight * capital / price / lot.size)) * lot.size
}

###############################################################################	
#' Round Lot Basic
#' round lot to the nearest lot.size and next try to reallocate remaining cash
#' @export 
###############################################################################
round.lot.basic = function(weight, price, capital, lot.size) {
	share = abs(weight * capital) / price		
		share1 = floor(share / lot.size) * lot.size
		
	discrepancy = (share - share1) * price
		cash = sum(discrepancy)
		
	lot.cash = price * lot.size
		min.lot.cash = min(lot.cash)

	# handle 0 weights, MV seems to produce very tiny NEGATIVE weights
	# let's round up to one share and use it instead of weight
	index = (1:len(weight))[cash >= lot.cash & floor(abs(share)) != 0]
		
	for(i in order(discrepancy[index], decreasing=T)) {
		if(cash < min.lot.cash) break
		j = index[i]
		if(cash < lot.cash[j]) next
			
		share1[j] = share1[j] + lot.size[j]
		cash = cash - lot.cash[j]	
	}
	
	sign(weight) * share1
}

round.lot.basic.test = function() {
	weight = c(1, 1, 1, 1) / 4
	price = c(1.345, 2.4, 3.5, 4.6)
	capital = 100
	lot.size = c(1, 1, 1, 1)
	
	w = round.lot.basic(weight, price, capital, lot.size)
	w
	sum(abs(w * price))
	
	weight = c(1, -1, 3, -1) / 4

	w = round.lot.basic(weight, price, capital, lot.size)
	w	
	sum(abs(w * price))
	w = (weight * capital) / price
	w
	sum(abs(w * price))
}


###############################################################################	
#' Test for bt.run.share.ex functionality
#' @export 
###############################################################################
bt.run.share.ex.test = function() {
	#*****************************************************************
	# Load historical data
	#*****************************************************************
	library(SIT)
	load.packages('quantmod')
	
	tickers = 'SPY'
	tickers = 'SPY,XLY,XLP,XLE,XLF,XLV,XLI,XLB,XLK,XLU'
	
	
	data <- new.env()
	getSymbols.extra(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
		for(i in data$symbolnames) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
	bt.prep(data, align='remove.na', fill.gaps = T)
	
	#*****************************************************************
	# Setup
	#*****************************************************************
	prices = data$prices
	  n = ncol(prices)
	  nperiods = nrow(prices)
	
	period.ends = date.ends(data$prices,'months')
	  	
	models = list()
	
	commission = list(cps = 0.01, fixed = 1.0, percentage = 0.0)
	
	#*****************************************************************
	# Buy Hold
	#******************************************************************
	data$weight[] = NA
	  #data$weight[1,] = 1
	  data$weight[period.ends,] = ntop(prices[period.ends,], n)
	models$test = bt.run.share(data, clean.signal=F, silent=F, commission=commission)
	
	data$weight[] = NA
	  #data$weight[1,] = 1
	  data$weight[period.ends,] = ntop(prices[period.ends,], n)
	models$test.ex = bt.run.share.ex(data, clean.signal=F, silent=F, commission=commission)

	data$weight[] = NA
	  #data$weight[1,] = 1
	  data$weight[period.ends,] = ntop(prices[period.ends,], n)
	models$test.ex.lot = bt.run.share.ex(data, clean.signal=F, silent=F, commission=commission, lot.size=50)
	
		
	data$weight[] = NA
	  #data$weight[1,] = 1
	  data$weight[period.ends,] = ntop(prices[period.ends,], n)
	models$test.ex.lot.turnover = bt.run.share.ex(data, clean.signal=F, silent=F, commission=commission, 
		lot.size=50, control = list(round.lot = list(select = 'minimum.turnover', diff.target = 5/100))
	)	
	
	
	# check shares
	last(models$test.ex.lot$share[period.ends,], 20)
	
	
	
	#*****************************************************************
	# Report
	#******************************************************************
	strategy.performance.snapshoot(models, T)
	
	layout(1:3)
		plotbt.transition.map(models$test$weight, 'BuyHold')
		plotbt.transition.map(models$test.ex$weight, 'BuyHold.ex')
		plotbt.transition.map(models$test.ex.lot$weight, 'BuyHold.ex')
	
	layout(1:3)
		plot(models$test.ex.lot$value, type='l')	
		plot(models$test.ex.lot$cash[-c(1:20)], type='l')
		plot(models$test.ex.lot$com, type='l')
		
		
	# put all reports into one pdf file
	pdf(file = 'report1.pdf', width=8.5, height=11)
		
		strategy.performance.snapshoot(models, data=data)
		
	dev.off()
		
		
		
	#*****************************************************************
	# Another test
	#******************************************************************	
	models = list()
	
	commission = list(cps = 0.01, fixed = 10.0, percentage = 0.0)
	
	obj = portfolio.allocation.helper(data$prices, 
		period.ends = period.ends, lookback.len = 250, silent=T, 
		min.risk.fns = list(
			EW=equal.weight.portfolio,
			RP=risk.parity.portfolio(function(ia) ia$risk),
			MV=min.var.portfolio,
			Sharpe.RP=risk.parity.portfolio(function(ia) ia$risk / ia$expected.return)
		)
	)
	
	for(i in names(obj$weights)) {
		data$weight[] = NA
		  data$weight[period.ends,] = obj$weights[[i]]
		models[[paste0(i)]] = bt.run.share(data, clean.signal=F, silent=T, commission=commission)
	
		data$weight[] = NA
		  data$weight[period.ends,] = obj$weights[[i]]
		models[[paste0(i,'.ex')]] = bt.run.share.ex(data, clean.signal=F, silent=T, commission=commission)
	
		data$weight[] = NA
		  data$weight[period.ends,] = obj$weights[[i]]
		models[[paste0(i,'.ex.lot')]] = bt.run.share.ex(data, clean.signal=F, silent=T, commission=commission, lot.size=50)
		
		data$weight[] = NA
		  data$weight[period.ends,] = obj$weights[[i]]
		models[[paste0(i,'.ex.lot.turnover')]] = bt.run.share.ex(data, clean.signal=F, silent=T, commission=commission, 
			lot.size=50, control = list(round.lot = list(select = 'minimum.turnover', diff.target = 5/100)))
			
	}
	
	
	# check shares
	range(models$MV.ex.lot$share)
	
	#*****************************************************************
	# Report
	#******************************************************************
	strategy.performance.snapshoot(models, T)
	
	
	# put all reports into one pdf file
	pdf(file = 'report2.pdf', width=8.5, height=11)
		
		strategy.performance.snapshoot(models, data=data)
		
	dev.off()
  
}


