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
#' If UnAdjusted logic is enabled, each price history is expected to have Dividend and
#' Split columns, it can be populated with bt.unadjusted.add.div.split function
#'
#' events are trades and div/splits for existing positions
#' * if div => adjust cash for 
#'	 long = cash + share * div * (1-div.tax)
#'	 short = cash - share * div
#' if split => adjust share by split ratio
#'
#' @export 
###############################################################################
bt.run.share.ex <- function
(
	b,					# enviroment with symbols time series
	prices = b$prices,	# prices
	clean.signal = T,	# flag to remove excessive signal	
	trade.summary = F, # flag to create trade summary
	silent = F,
	capital = 100000,
	commission = 0,
	weight = b$weight,
	dates = 1:nrow(b$prices),
	lot.size = c(),
	control = list(
		round.lot = default.round.lot.control()
	),
	
	adjusted = T,
	dividend.foreign.withholding.tax = NULL	
	# http://canadiancouchpotato.com/2012/09/17/foreign-withholding-tax-explained/
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
		trade.index = rowSums(trade) > 0
	
	# unadjusted logic
	if(!adjusted) {
		dividends = coredata(bt.apply(b, function(x) x[,'Dividend']))
		splits = coredata(bt.apply(b, function(x) x[,'Split']))		
			trade.dividend = rowSums(mlag(weight1) != 0 & dividends > 0, na.rm=T) > 0
			trade.split = rowSums(mlag(weight1) != 0 & splits > 0, na.rm=T) > 0
			
		dividend.foreign.withholding.tax = map2vector(dividend.foreign.withholding.tax, colnames(prices), 0)

		event.index = which(trade.index | trade.dividend | trade.split)
	} else
		event.index = which(trade.index)
		
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
	lot.size = map2vector(lot.size, colnames(prices), 1)
	
	# setup event driven back test loop
	cash.wt = cash = rep(capital, nperiods)
	event.type = div = com = rep(0, nperiods)
		event.type.def = list(none=0, trade=1, split=2, dividend=3)
	share.wt = share = matrix(0, nperiods, n)
		colnames(share) = colnames(prices)
	last.trade = 0
	weight.last = weight1[1,]
	
	for(i in event.index) {
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
		
		# unadjusted logic
		if(!adjusted) {
			if( trade.dividend[i] ) {
				for(a in which(share[i,] !=0 & dividends[i,] > 0))
					cashflow = share[i,a] * dividends[i,a] * 
						iif(share[i,a] < 0, 1, 1 - dividend.foreign.withholding.tax[a])
					cash[i] = cash[i] + cashflow
					cash.wt[i] = cash.wt[i] + cashflow
					div[i] = cashflow
					event.type[i] = event.type.def$dividend
			}
			# check what happends if dividend and split are on the same day
			if( trade.split[i] ) {
				for(a in which(share[i,] !=0 & splits[i,] > 0))
					share[i,a] = share.wt[i,a] = share[i,a] / splits[i,a]
				event.type[i] = event.type.def$split
			}
		}
		
		# update share[i,] and cash[i]
		if( trade.index[i] ) {
			out = bt.run.share.ex.allocate(weight.new = weight1[i,], weight.prev = weight.last,
				weight.change.index = !is.na(weight[i,]),
				price = prices[i,], share = share[i,], cash = cash[i],
				commission, lot.size, control = control$round.lot)
			
			# only update current ones, not the ones used for weights
			share[i,] = out$share
			cash[i] = out$cash
			com[i] = out$com
			event.type[i] = event.type.def$trade
		}
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
bt$value = cash + rowSums(share * prices)

bt$com = com
bt$div = div
bt$event.type = factor(event.type, as.character(unlist(event.type.def)), names(event.type.def))


	
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
	getSymbols.extra(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T, set.symbolnames=T)
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
 
	

	#*****************************************************************
	# Example of using round lot externally
	#******************************************************************
	weight = rep(1/n, n)
	price = coredata(last(prices))
	share = rep(0, n)
	cash = 100000
	commission = list(cps = 0.01, fixed = 1.0, percentage = 0.0)
	lot.size = rep(100, n)
	
	bt.run.share.ex.allocate(weight, weight, rep(T, n),
		price, share, cash, commission, lot.size)
			
}


###############################################################################	
#' Append dividend and split columns
#' @export 
###############################################################################
bt.unadjusted.add.div.split = function(data.raw, yahoo.round.up.nearest.cents=F) {
	if( !exists('symbolnames', data.raw, inherits = F) ) 
		tickers = ls(data.raw)
	else
		tickers = data.raw$symbolnames
		
	#*****************************************************************
	# For each asset, append dividend and split columns
	#****************************************************************** 	
	for(ticker in spl(tickers)) {
		price = data.raw[[ticker]]		
		price$Dividend = price$Split = 0

		# need full history	
		dividend = getDividends(ticker, from = '1900-01-01')	
		split = getSplits(ticker, from = '1900-01-01')	
		
		
# un-adjust split, faster version of adjRatios(splits=merge(split, index(dividend)))[,1]
#split1 = split
#	split1[] = rev(cumprod(rev(coredata(split))))
#x = mlag(merge(split1, index(dividend)), -1)
#x[] = ifna(x[ifna.prevx.rev(x)],1)
##all(x == adjRatios(splits=merge(split, index(dividend)))[,1])
		
		
		# Please see quantmod:::adjustOHLC for more details
		# un-adjust dividends for splits (Yahoo already adjusts div for splits)
    	if(is.xts(split) && is.xts(dividend) && nrow(split) > 0 && nrow(dividend) > 0)
			dividend = dividend * 1/adjRatios(splits=merge(split, index(dividend)))[,1]
		
		# use unadjusted dividends to compute retruns based on Close      	
		dividend = dividend[index(price)]
		split = split[index(price)]
		
		# http://www.theglobeandmail.com/globe-investor/investor-education/four-dividend-dates-every-investor-needs-to-know/article19273251/
		if( is.xts(dividend) && nrow(dividend) > 0 )
		if( nrow(price[index(dividend)]) != nrow(dividend) ) 
			stop(paste('Missing Price date for dividend. Symbol =', ticker))
		else
			price$Dividend[index(dividend)] = dividend
			
		if( is.xts(split) && nrow(split) > 0 )
		if( nrow(price[index(split)]) != nrow(split) ) 
			stop(paste('Missing Price date for split. Symbol =', ticker))
		else
			price$Split[index(split)] = split
		
		# round up to the nearest cents - this is the only way to match IBM prices
		if(yahoo.round.up.nearest.cents) {
			map.col = unlist(find.names('Close,Open,High,Low,Adjusted', price, F))
			price[,map.col] = ceiling(100 * price[,map.col]) / 100 
		}
			
		data.raw[[ticker]] = price
	}
}

###############################################################################	
#' Test for bt.run.share.unadjusted functionality
#' @export 
###############################################################################
bt.run.share.unadjusted.test.data = function() {
	#*****************************************************************
	# Load historical data
	#*****************************************************************
	library(SIT)
	load.packages('quantmod')
	
	tickers = 'IBM'
		
	data = env()
	getSymbols.extra(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T, set.symbolnames=T)
		#bt.start.dates(data)
		
		# copy unadjusted prices
		data.raw = env(data)
	
	
	#*****************************************************************
	# For each asset, append dividend and split columns
	#****************************************************************** 	
	bt.unadjusted.add.div.split(data.raw, yahoo.round.up.nearest.cents = T)
	
		
	#*****************************************************************
	# Look at the data for IBM
	#****************************************************************** 	
	ticker = 'IBM'
	# adjusted
	adjusted = data.raw[[ticker]]$Adjusted / mlag(data.raw[[ticker]]$Adjusted) - 1
		adjusted = ifna(adjusted,0)
	prod(1 + adjusted)

	# unadjusted
	split = iif(data.raw[[ticker]]$Split > 0, 1 / data.raw[[ticker]]$Split, 1)
	unadjusted = (data.raw[[ticker]]$Close * split +  data.raw[[ticker]]$Dividend) / mlag(data.raw[[ticker]]$Close) - 1
		unadjusted = ifna(unadjusted,0)
	prod(1 + unadjusted)	
	
	# look at diffs
	index = which(round(adjusted - unadjusted,4) != 0)
	cbind(round(adjusted - unadjusted, 4), data.raw[[ticker]]$Split, data.raw[[ticker]]$Dividend)[index]

	plota.matplot(cbind(cumprod(1 + adjusted),cumprod(1 + unadjusted)))

	# look at most extreme one
	index.max = which.max(abs(adjusted - unadjusted))	
	cbind(adjusted, unadjusted, round(adjusted - unadjusted, 4), data.raw[[ticker]]$Split, data.raw[[ticker]]$Dividend)[index.max]	
	data.raw[[ticker]][(index.max-1):index.max,]
	
	# http://finance.yahoo.com/q/hp?s=IBM&a=10&b=4&c=1992&d=10&e=5&f=1992&g=d
	(65.875 + 0.3025) / 68.250 - 1
	12.04279 / 12.25577 - 1
	
	# https://www.google.com/finance/historical?cid=18241&startdate=Nov+4+1992&enddate=Nov+5+1992
	16.47 /17.06 -1
	
	# www.quantshare.com/sa-43-10-ways-to-download-historical-stock-quotes-data-for-free
	# http://www.quotemedia.com/finance/quote/?qm_page=13863&qm_symbol=IBM
	#http://app.quotemedia.com/quotetools/getHistoryDownload.csv?&webmasterId=501&startDay=02&startMonth=11&startYear=1992&endDay=02&endMonth=12&endYear=1992&isRanged=false&symbol=IBM
	8.4485 / 7.8843 - 1
	
	
	# not working anymore
	# http://moneycentral.msn.com/investor/charts/chartdl.aspx?PT=11&compsyms=&D4=1&DD=1&D5=0&DCS=2&MA0=0&MA1=0&CF=0&D7=&D6=&showtablbt=View+price+history+with+dividends%2Fsplits&symbol=IBM&nocookie=1&SZ=0
	
	yhist = read.xts(hist.quotes.url('IBM', '1992-11-01', '1992-11-30', 'yahoo'))
	ghist = read.xts(hist.quotes.url('IBM', '1992-11-01', '1992-11-30', 'google'), format='%d-%b-%y')
	qhist = read.xts(hist.quotes.url('IBM', '1992-11-01', '1992-11-30', 'quotemedia'))	

	# quantmod:::adjustOHLC 
	# TTR:::adjRatios
	dividends = getDividends('IBM', from = '1900-01-01')	
	splits = getSplits('IBM', from = '1900-01-01')	
	
dividends['1992:11::1992:11']
	
	# un-adjust dividends for splits (Yahoo already adjusts div for splits)
    if(is.xts(splits) && is.xts(dividends) && nrow(splits) > 0 && nrow(dividends) > 0)
		dividends = dividends * 1/adjRatios(splits=merge(splits, index(dividends)))[,1]
		
dividends['1992:11::1992:11']
		
	# use unadjusted dividends to compute retruns based on Close      	
    dividend = dividends[index(yhist)]

	yhist = yhist[,spl('Close,Adj_Close')]
		colnames(yhist) = spl('Yahoo.Close,Yahoo.Adjusted')		
	yhist$Dividend = 0
		yhist$Dividend[index(dividend)] = dividend
	yhist
	
	ghist = ghist[,'Close']
		colnames(ghist) = spl('Google.Adjusted')		
	
	qhist = qhist[,c('close', 'adjclose')]
		colnames(qhist) = spl('Quotemedia.Close,Quotemedia.Adjusted')		

	temp = cbind(yhist, ghist, qhist)
	temp[,spl('Yahoo.Close,Dividend,Quotemedia.Close')]
	
	to.return = function(x) round(100*(x/mlag(x)-1),3)
	
	Yahoo.Return = to.return(temp$Yahoo.Close + temp$Dividend)
	
	# round up to the nearest cents - this is the only way to match IBM prices
	Yahoo.Return1 = to.return(ceiling(100*temp$Yahoo.Close)/100 + temp$Dividend)
	
	Yahoo.Return.Adjusted = to.return(temp$Yahoo.Adjusted)
	
	Google.Return.Adjusted = to.return(temp$Google.Adjusted)
	
	Quotemedia.Return = to.return(temp$Quotemedia.Close + temp$Dividend)
	Quotemedia.Return.Adjusted = to.return(temp$Quotemedia.Adjusted)
	
	ret = cbind(Yahoo.Return, Yahoo.Return1, Yahoo.Return.Adjusted, Google.Return.Adjusted, Quotemedia.Return, Quotemedia.Return.Adjusted)
	t(apply(ret,1,range))
	t(diff(apply(ret,1,range)))
	ret['1992:11:05']    
 
 
# https://www.ibm.com/investor/financials/	
# The dividend rate per share is the actual amount paid per share. No adjustments were made for stock splits.
# Dividend number 311
# Rate per share 1.21
# Payable date 12/10/92
# Record date 11/12/92  
#
# Split
# Record date 5/10/79
# Payment date 5/31/79
# Stock dividend or split 4 for 1 Stock Split
#
# Prices
txt = '
Date 		Open High Low Close Volume
Nov-2-1992 67.00 69.00 67.00 68.88 2,322,100
Nov-3-1992 68.50 69.88 68.50 69.13 2,375,200
Nov-4-1992 69.00 69.63 68.13 68.25 2,079,800
Nov-5-1992 67.13 67.25 65.63 65.88 2,136,200
Nov-6-1992 65.38 66.50 65.00 66.25 2,642,300
Nov-9-1992 66.25 67.63 66.25 67.50 2,216,400
Nov-10-1992 67.63 68.00 65.88 65.88 2,187,100
Nov-11-1992 65.63 65.75 64.50 65.00 3,145,100
Nov-12-1992 65.13 65.38 64.13 64.13 3,133,000
Nov-13-1992 64.88 65.13 64.00 64.88 1,851,300
Nov-16-1992 64.75 65.50 64.63 64.88 1,765,100
Nov-17-1992 64.88 65.00 64.00 64.25 2,020,700
Nov-18-1992 64.13 64.38 62.75 63.13 2,707,100
Nov-19-1992 63.00 63.13 61.00 61.25 3,307,600
Nov-20-1992 61.38 62.63 60.88 62.25 3,715,200
Nov-23-1992 62.38 63.88 62.25 63.25 2,220,200
Nov-24-1992 63.75 65.50 63.50 64.88 2,847,100
Nov-25-1992 65.38 66.00 65.13 65.38 1,788,700
Nov-27-1992 65.88 66.25 65.25 66.00 1,229,500       
Nov-30-1992 67.88 68.63 67.50 68.25 3,239,000
'

IBM = read.xts(txt,sep=' ', format='%b-%d-%Y')
	IBM = IBM[,'Close']
		colnames(IBM) = spl('IBM.Close')		

	IBM$Dividend = 0
	IBM$Dividend['1992:11:05'] = 1.21
	
	IBM.Return = to.return(IBM$IBM.Close + IBM$Dividend)
	
	ret = cbind(IBM.Return, Yahoo.Return, Yahoo.Return1, Yahoo.Return.Adjusted, Google.Return.Adjusted, Quotemedia.Return, Quotemedia.Return.Adjusted)
	ret['1992:11:05']    
	
	 ret$IBM.Close - ret$Yahoo.Close.1
	
	# http://www.dividend.com/dividend-stocks/technology/diversified-computer-systems/ibm-ibm-corp/	
	
# looks like dividends are split adjusted by yahoo	
	
	# all due to dividends
	setdiff(index, which(data.raw[[ticker]]$Dividend > 0))
	
	# remove diffs
	temp.adjusted = adjusted
		temp.adjusted[index] = 0	
	prod(1 + temp.adjusted)	
	
	temp.unadjusted = unadjusted
		temp.unadjusted[index] = 0	
	prod(1 + temp.unadjusted)	
		
	plota.matplot(cbind(cumprod(1 + temp.adjusted),cumprod(1 + temp.unadjusted)))

}
	

###############################################################################	
#' Summarize bt.run.share.ex events
#' @export 
###############################################################################
bt.make.trade.event.summary.table = function(bt, to.text=F) {
	index = bt$event.type != 'none'
	
	# create summary table: event.type, date, shares, cash, com, div, value	
	out = data.frame(
		Type = bt$event.type,
		bt$share,
		Cash=bt$cash,
		Com=bt$com,
		Div=bt$div,
		Value=bt$value
	)[index,]
	rownames(out) = format(index(bt$equity)[index], '%Y-%m-%d')
	
	if(to.text) to.nice(out,0)
	else out
}

###############################################################################	
#' Test for bt.run.share.unadjusted functionality
#' @export 
###############################################################################
bt.run.share.unadjusted.test = function() {
	#*****************************************************************
	# Load historical data
	#*****************************************************************
	library(SIT)
	load.packages('quantmod')
	
	tickers = 'IBM'
		
	data = env()
	getSymbols.extra(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T, set.symbolnames=T)
		#bt.start.dates(data)
		
		# copy unadjusted prices
		data.raw = env(data)
	
		# adjusted prices
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
	# Base SIT Back-test
	#****************************************************************** 
	data$weight[] = NA
	  #data$weight[1,] = 1
	  data$weight[period.ends,] = 1		
	models$test = bt.run.share(data, clean.signal=F, silent=F, commission=commission)
	
	data$weight[] = NA
		#data$weight[1,] = 1
		data$weight[period.ends,] = 1		
	models$test.ex = bt.run.share.ex(data, clean.signal=F, silent=F, commission=commission,
		lot.size=1)
	
	
	#*****************************************************************
	# For each asset, append dividend and split columns
	#****************************************************************** 	
	bt.unadjusted.add.div.split(data.raw, yahoo.round.up.nearest.cents = T)
	
	bt.prep(data.raw, align='remove.na', fill.gaps = T)
	

	#*****************************************************************
	# Setup
	#*****************************************************************
	prices = data.raw$prices
	  n = ncol(prices)
	  nperiods = nrow(prices)
	
	period.ends = date.ends(data.raw$prices,'months')
	  
	#models = list()
	
	commission = list(cps = 0.01, fixed = 1.0, percentage = 0.0)
	

	#*****************************************************************
	# New Back-test
	#******************************************************************
	data.raw$weight[] = NA
		#data.raw$weight[1,] = 1
		data.raw$weight[period.ends,] = 1		
	models$test.unadjusted = bt.run.share.ex(data.raw, clean.signal=F, silent=F, commission=commission,
		lot.size=50, adjusted = F)
	  
	#*****************************************************************
	# Report
	#****************************************************************** 	
	strategy.performance.snapshoot(models, T)

	layout(1:2)
		plotbt.transition.map(models$test.ex$weight)
		plotbt.transition.map(models$test.unadjusted$weight)

		
	# create table
	mlast(bt.make.trade.event.summary.table(models$test.unadjusted), 20)
		
		

	#*****************************************************************
	# Load historical data
	#*****************************************************************
	library(SIT)
	load.packages('quantmod')
	
	tickers = 'SPY'
	tickers = 'SPY,XLY,XLP,XLE,XLF,XLV,XLI,XLB,XLK,XLU'
	tickers = 'AAPL,IBM,VTI,IEV,EWJ,EEM,RWX,DBC,GLD,TLT,IEF,SHY'
		
	data = env()
	getSymbols.extra(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T, set.symbolnames=T)
		#bt.start.dates(data)
		
		# copy unadjusted prices
		data.raw = env(data)
	
		# adjusted prices
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
	# Base SIT Back-test
	#****************************************************************** 
	data$weight[] = NA
	  #data$weight[1,] = 1
	  data$weight[period.ends,] = ntop(prices[period.ends,], n)
	models$test = bt.run.share(data, clean.signal=F, silent=F, commission=commission)
	
	data$weight[] = NA
	  data$weight[period.ends,] = ntop(prices[period.ends,], n)
	models$test.ex = bt.run.share.ex(data, clean.signal=F, silent=F, commission=commission,
		lot.size=50)
	
	
	#*****************************************************************
	# Report
	#****************************************************************** 	
	strategy.performance.snapshoot(models, T)

	plotbt.transition.map(models$test.ex$weight)

#*****************************************************************
# New Back-test
#****************************************************************** 	
	
	#*****************************************************************
	# For each asset, append dividend and split columns
	#****************************************************************** 	
	bt.unadjusted.add.div.split(data.raw)
	
	bt.prep(data.raw, align='remove.na', fill.gaps = T)

	#*****************************************************************
	# Setup
	#*****************************************************************
	prices = data.raw$prices
	  n = ncol(prices)
	  nperiods = nrow(prices)
	
	period.ends = date.ends(data.raw$prices,'months')
	  
	#models = list()
	
	commission = list(cps = 0.01, fixed = 1.0, percentage = 0.0)
	

	#*****************************************************************
	# New Back-test
	#******************************************************************
	data.raw$weight[] = NA
	  data.raw$weight[period.ends,] = ntop(prices[period.ends,], n)
	models$test.unadjusted = bt.run.share.ex(data.raw, clean.signal=F, silent=F, commission=commission,
		lot.size=50, adjusted = F)
	  
	data.raw$weight[] = NA
	  data.raw$weight[period.ends,] = ntop(prices[period.ends,], n)  
	models$test.unadjusted1 = bt.run.share.ex(data.raw, clean.signal=F, silent=F, commission=commission, 
		lot.size=50, 
		adjusted = F,
		control = list(round.lot = list(select = 'minimum.turnover', diff.target = 5/100)),
		dividend.foreign.withholding.tax = 30/100
	)		
	
	#*****************************************************************
	# Report
	#****************************************************************** 	
	strategy.performance.snapshoot(models, T)

	layout(1:2)
		plotbt.transition.map(models$test.unadjusted$weight)
		plotbt.transition.map(models$test.unadjusted1$weight)
		
	# create table
	mlast(bt.make.trade.event.summary.table(models$test.unadjusted), 20)
		

	
	# put all reports into one pdf file
	pdf(file = 'report.u.pdf', width=8.5, height=11)
		
		strategy.performance.snapshoot(models, data=data)
		
	dev.off()
 
	
			
	
}



