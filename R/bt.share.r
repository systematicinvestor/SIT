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
	do.lag = 1, 	   # lag logic, to be back compatible with bt.run.share

	silent = F,
	capital = 100000,
	commission = 0,
	weight = b$weight,
	dates = 1:nrow(b$prices),
	
	lot.size = 0.01,
	control = list(
		round.lot = default.round.lot.control()
	),
	
	# cashflow are treated differently based on type
	# the regular cashflows do not affect portfolio returns
	#
	# the fee.rebate cashflows affect portfolio returns
	# MER,Margin Cost, Annual Taxes, can be modeled as periodic withdrawals
	# these withdrawals should be included in return calculations 
	# because these withdrawals are fees of running portfolio vs
	# regular withdrawals should not affect portfolio returns
	#
	cashflow.control = list(
		# named list, for example
		#
		#monthly.income = list(
		#	cashflows = NULL,	# xts object with cashflows, indicates both timing and amount
		#	invest = spl('cash,rebalance,update'), # indicates how to adjust portfolio after cashflow
		#	cashflow.fn = function(info, i, i.prev) round.to((info$cash[i] + sum(info$shares[i,] * prices[i,]))*(5/100), 100), # if specified, cashflows are computed as a function of total equity
		#	type = spl('regular,fee.rebate')
		#),
		#
		#MER = list(
		#	cashflows = NULL,
		#	invest = spl('cash,rebalance,update'),
		#	cashflow.fn = function(info, i, i.prev) sum((info$cash + rowSums(info$shares * prices))[(i.prev+1):i]*(2/100 / 365)) ,
		#	type = 'fee.rebate'
		#),
		#
		#margin.cost = list(
		#	cashflows = NULL,
		#	invest = spl('cash,rebalance,update'),
		#	cashflow.fn = function(info, i, i.prev) sum(iif(info$cash < 0, -info$cash, 0)[(i.prev+1):i]*(2/100 / 365))
		#	type = 'fee.rebate'
		#),
		# value = cash + sum(price * share)
		# short = sum((price * share)[share<0])
		# long = sum((price * share)[share>0])
		# margin = iif(long > value, long - value, 0)
		# 
		# portfolio can accure interest on (cash + short), if (cash + short) > 0
		# portfolio can accure short rebate on -short i.e. short rebate = FED rate - borrow rate
		#	this can turn negative
		#
		#taxes = list(
		#	cashflows = NULL,
		#	invest = spl('cash,rebalance,update'),
		#	cashflow.fn = NULL, # function need to access gain/loss information
		#	type = 'fee.rebate'
		#)				
	),
	
	adjusted = T,
	
	# following functionality only works for unadjusted data
	dividend.control = list(
		foreign.withholding.tax = NULL,
		# http://canadiancouchpotato.com/2012/09/17/foreign-withholding-tax-explained/
		
		invest = spl('cash,rebalance,update')
		# invest can take following values:
		# cash - put dividend to cash
		# rebalance - force full rebalance
		# update - minimally update portfolio to allocate dividend
		#   only add new positions, such that abs(shares) only increases
	),
	
	tax.control = NULL
	#tax.control = default.tax.control()
) 
{
	#---------------------------------------------------------
	# check inputs
	#---------------------------------------------------------
	default.dividend.control = list(
		foreign.withholding.tax = NULL,
		# http://canadiancouchpotato.com/2012/09/17/foreign-withholding-tax-explained/
		
		invest = spl('cash,rebalance,update')
		# invest can take following values:
		# cash - put dividend to cash
		# rebalance - force full rebalance
		# update - minimally update portfolio to allocate dividend
		#   only add new positions, such that abs(shares) only increases
	)
	
	dividend.control = verify.control('dividend.control', default.dividend.control, dividend.control)	

	#---------------------------------------------------------
	# defs
	#---------------------------------------------------------
	invest.type.def = list(none=0, cash=1, rebalance=2, update=3)
	cashflow.type.def = list(regular=0, fee.rebate=1)

	#---------------------------------------------------------
	# process cashflows
	#---------------------------------------------------------
	dummy = NA * prices[,1,drop=F]
	cashflows = list(
		n = len(cashflow.control),
		cash = dummy
	)
	
	if( cashflows$n > 0 ) {
		cashflows$last.index = rep(0, cashflows$n)
	
		cashflows$type = rep('', cashflows$n)
		for(i in 1:cashflows$n) cashflows$type[i] = ifnull(cashflow.control[[i]]$type, 'regular')[1] 
		cashflows$type = cashflow.type.def[[ tolower(cashflows$type) ]]

		cashflows$invest = rep('', cashflows$n)
		for(i in 1:cashflows$n) cashflows$invest[i] = ifnull(cashflow.control[[i]]$invest, 'cash')[1] 
		cashflows$invest = invest.type.def[[ tolower(cashflows$invest) ]]
		
		cashflows$fn = lapply(cashflow.control, function(x) iif(is.null(x$cashflow.fn), x$cashflow.fn, match.fun(x$cashflow.fn)))

		cashflows$cash = matrix(NA, nrow(prices), cashflows$n)
			colnames(cashflows$cash) = names(cashflow.control)
		for(i in 1:cashflows$n)
			if( !is.null(cashflow.control[[i]]$cashflows) ) {
				dummy[] = NA
				dummy[index(cashflow.control[[i]]$cashflows)] = cashflow.control[[i]]$cashflows
				cashflows$cash[,i] = dummy
			}
	}
	cashflows$cash = coredata(ifna(cashflows$cash, 0))
	
	#---------------------------------------------------------
	# process weight
	#---------------------------------------------------------
	# make sure we don't have any abnormal weights
	weight[is.nan(weight) | is.infinite(weight)] = NA
	weight[!is.na(weight) & is.na(prices)] = 0
	
		# lag logic, to be back compatible with bt.run.share
		# default logic is to use current weights to trade at close i.e. no lag
		weight = iif( do.lag == 1, weight, mlag(weight, do.lag - 1) )
	
	weight = coredata(weight)
		temp = bt.exrem(weight)
		
	if(clean.signal) {
		weight = temp
	} else { # always clean up 0's
		index = ifna(weight == 0, F)
		weight[index] = temp[index]
	}
	
	#---------------------------------------------------------
	# process prices
	#---------------------------------------------------------
	check.non.positive.prices = function(p, name) {
		if(any(p<=0, na.rm=T)) {
			index = lookup.index(open,  which(p <= 0))
			stop('bt.run.share.ex detected non positive ', name, ' for ' , join(colnames(p)[index$icol], ' , '))
		}
	}

	
	check.non.positive.prices(prices,'prices')
	
	prices = coredata(prices)
		n = ncol(prices)
	
	# execution.price logic
	trade = !is.na(weight)
	if( sum(trade) > 0 ) {
		execution.price = coredata(b$execution.price)
		prices[trade] = iif( is.na(execution.price[trade]), prices[trade], execution.price[trade] )
	}
	
	check.non.positive.prices(prices,'execution.prices')

	# make sure that prices are available, assume that
	# weights account for missing prices i.e. no price means no allocation
	prices[] = ifna( bt.apply.matrix(prices, ifna.prev), 1)
	
	#---------------------------------------------------------
	# validate
	#---------------------------------------------------------
	if( !is.list(commission) )
		commission = list(cps = commission, fixed = 0.0, percentage = 0.0)	
		
	lot.size = map2vector(lot.size, colnames(prices), 0.01)
		

	
	
	
	
	dividend.control$foreign.withholding.tax = map2vector(dividend.control$foreign.withholding.tax, colnames(prices), 0)
	dividend.control$invest = ifnull(dividend.control$invest, 'cash')
		dividend.control$invest = invest.type.def[[ tolower(dividend.control$invest[1]) ]]
	
	#---------------------------------------------------------
	# execute internal version of bt.run.share.ex
	#---------------------------------------------------------
	bt = bt.run.share.ex.internal(b, prices, capital, commission, weight,
		lot.size, control, cashflows, adjusted, dividend.control, tax.control)			

	#---------------------------------------------------------
	# process results
	#---------------------------------------------------------
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

	bt
}
	
	
###############################################################################
#' verify.control	
#' @export 
###############################################################################
verify.control = function(name, control, inputs) {
	default.names = ls(control, all.names=T)
	input.names = ls(inputs, all.names=T)		
	
	missing = setdiff(input.names, default.names)		
	if(len(missing) > 0) 
		warning(paste(name, 
		'\n\nOnly following variables are supported:\n', join(default.names,'\n '), 
		'\n\nFollowing variables were provided:\n', join(missing,'\n '), '\nbut not supported.'), str(control))
		
	common = intersect(input.names, default.names)
	for(n in common)
		control[[n]] = inputs[[n]]
	
	control	
}

	
###############################################################################
#' default.tax.control	
#' @export 
###############################################################################
default.tax.control = function(nonqualified = c()) {
	list(
		capital.gain = list(
			short.term.tax = 35/100,
			long.term.tax = 15/100,
			#wash.sale.min.holding.period = 30, # 30 days
			wash.sale.min.holding.period = NA, # skip wash sale logic
			long.term.min.holding.period = 365 # one year
		),
		
		dividend = list(			
			qualified.tax = 15/100,
			qualified.min.holding.period = 60, # 60 days
			nonqualified.tax = 35/100,
			nonqualified = nonqualified # tickers of stocks that do not qualify for preferential tax rate
		)
	)
}


# internal version, all inputs are assumed preprocessed
bt.run.share.ex.internal <- function(b, prices, capital, commission, weight,
		lot.size, control, cashflows, adjusted, dividend.control, tax.control
)			
{
	#---------------------------------------------------------
	# defs
	#---------------------------------------------------------
	invest.type.def = list(none=0, cash=1, rebalance=2, update=3)
	cashflow.type.def = list(regular=0, fee.rebate=1)
	
	#---------------------------------------------------------
	# setup
	#---------------------------------------------------------
	# back filled weights
	weight1 = ifna( bt.apply.matrix(weight, ifna.prev), 0)
	
	
	# find cashflows
	trade.cashflow = rowSums(cashflows$cash != 0) > 0
	
		
	# find trades
	trade = !is.na(weight)
		trade.index = rowSums(trade) > 0
				
	#---------------------------------------------------------
	# unadjusted logic
	#---------------------------------------------------------
	if(!adjusted) {
		dividends = coredata(bt.apply(b, function(x) x[,'Dividend']))
			dividends[is.na(dividends)] = 0
		splits = coredata(bt.apply(b, function(x) x[,'Split']))		
			splits[is.na(splits)] = 0
			
			trade.dividend = rowSums(mlag(weight1) != 0 & dividends > 0, na.rm=T) > 0
			trade.split = rowSums(mlag(weight1) != 0 & splits > 0, na.rm=T) > 0
			
		event.index = which(trade.index | trade.cashflow | trade.dividend | trade.split)
	} else
		event.index = which(trade.index | trade.cashflow)			
		

		
	nperiods = nrow(prices)	
	n = ncol(prices)
		
		
	#---------------------------------------------------------
	# taxes
	#---------------------------------------------------------				
	if( !is.null(tax.control) ) {
		# should interact with foreign dividend tax	
		holdings = env(
			n.trades = rep(0, n),
			# start with 100 rows, if goes above 100, increase by 100
			share = matrix(0, 100, n),	# fifo vector of trades for each assset
			date = matrix(0, 100, n),	# index / date of each trade
			price = matrix(0, 100, n)	# entry price each trade			
		)
		
		wash.sale = env(
			n.trades = rep(0, n),
			date = matrix(0, 100, n),
			share = matrix(0, 100, n),
			loss.per.share = matrix(0, 100, n),
			long.term = matrix(T, 100, n)
		)
				
		tax = env(
			long.term.cap = rep(0, nperiods),
			short.term.cap = rep(0, nperiods),
			qualified.div = rep(0, nperiods),
			non.qualified.div = rep(0, nperiods)
		)	
	
		tax.control$dividend$nonqualified = map2vector(tax.control$dividend$nonqualified, colnames(prices), F)
	}
	
	#---------------------------------------------------------
	# setup event driven back test loop
	#---------------------------------------------------------
			
	cash.wt = rep(capital, nperiods)
	event.type = div = com = cashflow = fee.rebate = rep(0, nperiods)
		event.type.def = list(none=0, trade=1, split=2, dividend=3, cashflow=4)
	share.wt = matrix(0, nperiods, n)
		colnames(share.wt) = colnames(prices)
		
		
	info = env(
		cash = cash.wt,
		share = share.wt
	)
	if( !is.null(tax.control) ) {
		info$dates = b$dates
		info$tax = tax
		info$tax.control = tax.control
		info$holdings = holdings
	}
	
		
	last.trade = 0
	weight.last = weight1[1,]
	
	# need to speed up code for dividends invested in cash
	# i.e. just put dividends to cash and go forward
		
	for(i in event.index) {		
		trade.invest.type = iif(trade.index[i], invest.type.def$rebalance, invest.type.def$none)
		trade.today = trade.index[i]
	
	
			
	
	
		if(last.trade > 0) {
			# copy from last trade
			index = (last.trade + 1) : i
				n.index = len(index)
			share.wt[index,] = rep.row(info$share[last.trade,], n.index)
			info$share[index,] = rep.row(info$share[last.trade,], n.index)
			cash.wt[index] = info$cash[last.trade]
			info$cash[index] = info$cash[last.trade]
			
			weight.last = weight1[i-1,]
		}
		
		
		
		# unadjusted logic
		if(!adjusted) {
			if( trade.dividend[i] ) {			
				if( !is.null(tax.control) ) {
					tax.update.dividends(tax.control, dividend.control, holdings, tax, info$share[i,], dividends[i,], i, b$dates)					
				}
					
				# for(a in which(info$share[i,] !=0 & dividends[i,] > 0))
				asset.cashflow = sum(info$share[i,] * dividends[i,] * 
					iif(info$share[i,] < 0, 1, 1 - dividend.control$foreign.withholding.tax)
					)
				
				info$cash[i] = info$cash[i] + asset.cashflow
				cash.wt[i] = cash.wt[i] + asset.cashflow
				div[i] = asset.cashflow
				event.type[i] = event.type.def$dividend
					
				if(dividend.control$invest == invest.type.def$rebalance | dividend.control$invest == invest.type.def$update) {
					trade.index[i] = T
					if(trade.invest.type == invest.type.def$none) trade.invest.type = dividend.control$invest
				}
			}
			
			
			
			# check what happens if dividend and split are on the same day
			if( trade.split[i] ) {
				for(a in which(info$share[i,] !=0 & splits[i,] > 0))
					info$share[i,a] = share.wt[i,a] = info$share[i,a] / splits[i,a]
				event.type[i] = event.type.def$split
				
				if( !is.null(tax.control) ) {
					for(a in which(info$share[i,] !=0 & splits[i,] > 0)) {
						holdings.split(holdings, a, splits[i,a])
						
						n.trades = 1:holdings$n.trades[a]							
						holdings$price[n.trades,a] = holdings$price[n.trades,a] * splits[i,a]						
					}
					
					for(a in which(wash.sale$n.trades > 0 & splits[i,] > 0)) {
						holdings.split(wash.sale, a, splits[i,a])
						
						n.trades = 1:wash.sale$n.trades[a]
						wash.sale$loss.per.share[n.trades,a] = wash.sale$loss.per.share[n.trades,a] * splits[i,a]
					}				
				}
			}
		}
		
		# need to be completed / tested
		if( trade.cashflow[i] ) {
						
			for(c in (1:cashflows$n)[cashflows$cash[i,] != 0]) {				
				if( !is.null(cashflows$fn[[c]]) ) {
					cashflows$cash[i,c] = cashflows$fn[[c]](info, i, cashflows$last.index[c])
				}
			
				info$cash[i] = info$cash[i] + cashflows$cash[i,c]
				
				if(cashflows$type[c] == cashflow.type.def$regular)
					cashflow[i] = cashflow[i] + cashflows$cash[i,c]
				else
					fee.rebate[i] = fee.rebate[i] + cashflows$cash[i,c]
					
				event.type[i] = event.type.def$cashflow
				
				cashflows$last.index[c] = i
				
				if(cashflows$invest[c] == invest.type.def$rebalance | cashflows$invest[c] == invest.type.def$update) {
					trade.index[i] = T
					if(trade.invest.type == invest.type.def$none) trade.invest.type = cashflows$invest[c]
				}							
			}
		}
		
		
		# update share[i,] and cash[i]
		if( trade.index[i] ) {	
			# if there is a big cashflow, we might want to override weight.change.index
			# to set all to TRUE to work with full portfolio instead of subset
			
			weight.change.index = iif(trade.today, !is.na(weight[i,]), rep(T,n))			
			
			if(trade.invest.type == invest.type.def$rebalance)
				out = bt.run.share.ex.allocate(weight.new = weight1[i,], weight.prev = weight.last,
					weight.change.index = weight.change.index,
					price = prices[i,], share = info$share[i,], cash = info$cash[i],
					commission, lot.size, control = control$round.lot,
					cashflow = cashflow[i] + fee.rebate[i] + div[i])
			
			# update - minimally update portfolio to allocate cash
			#   only add new positions, such that abs(shares) only increases
			if(trade.invest.type == invest.type.def$update) {
			
			
				out = bt.run.share.ex.invest(weight.new = weight1[i,], weight.prev = weight.last,
					weight.change.index = weight.change.index,
					price = prices[i,], share = info$share[i,], cash = info$cash[i], 
					cashflow = cashflow[i] + fee.rebate[i] + div[i],
					commission, lot.size, control = control$round.lot)
			}

			if( !is.null(tax.control) && sum(info$share[i,] != out$share) > 0 ) {
			
	if( any(!equal.check(info$share[i,], sapply(1:n, function(a) sum(iif(holdings$n.trades[a] > 0, holdings$share[1:holdings$n.trades[a],a], 0))  )) )	)
	{
		cat('Wrong Holdings', info$share[i,][5], '\n')
		

	}		
				tax.update.holdings(tax.control, holdings, tax, wash.sale, 
					info$share[i,], out$share, prices[i,], i, b$dates)
			}
						
			# only update current ones, not the ones used for weights
			info$share[i,] = out$share
			info$cash[i] = out$cash
			com[i] = out$com
			event.type[i] = event.type.def$trade
		}
		last.trade = i
	}
		
	if( last.trade > 0 & last.trade < nperiods) {
		# copy from last trade
		index = (last.trade + 1) : nperiods
			n.index = len(index)
		share.wt[index,] = rep.row(info$share[last.trade,], n.index)
		info$share[index,] = rep.row(info$share[last.trade,], n.index)
		cash.wt[index] = info$cash[last.trade]
		info$cash[index] = info$cash[last.trade]
	}


	#---------------------------------------------------------
	# setup output structure
	#---------------------------------------------------------
	bt = list(
		type = 'share', 
		capital = capital,
		share = info$share,
		cash = info$cash,
		value = info$cash + rowSums(info$share * prices),
		com = com,
		div = div,
		
		weight = share.wt * prices / (cash.wt + rowSums(share.wt * prices)),
		
		event.type = factor(event.type, as.character(unlist(event.type.def)), names(event.type.def))
	)
	
	if( cashflows$n > 0 ) {
		bt$cashflows = cashflows$cash
		
		bt$cashflow = cashflow
		bt$fee.rebate = fee.rebate
	}
	
	if( !is.null(tax.control) ) {
		bt$long.term.cap = tax$long.term.cap
		bt$short.term.cap = tax$short.term.cap
		bt$qualified.div = tax$qualified.div
		bt$non.qualified.div = tax$non.qualified.div	
	}

	#---------------------------------------------------------
	# compute returns
	#---------------------------------------------------------
	value = c(capital, bt$value)
		bt$ret = (value / mlag(value) - 1)[-1]
	if(sum(abs(cashflow)) > 0) {
		# special logic to compute returns in case of cashflows, external money flows
		# * negative cashflow: assume money will be taken out after the close
		#   return = (total value[T] without cashflow[T]) / total value[T-1]		
		index = cashflow < 0
		bt$ret[index] = (c(capital, bt$value - cashflow) / mlag(value) - 1)[-1][index]

		# * positive cashflow: assume money were availbale a day before and just sat in account
		#   return = (total value[T] including cashflow) / (total value[T-1] + cashflow[T])
		index = cashflow > 0
		value1 = c(capital, bt$value + mlag(cashflow, -1))
		bt$ret[index] = (value / mlag(value1) - 1)[-1][index]
	}
	 
	bt
}


#
# [Numerical Error]
# http://www.burns-stat.com/documents/tutorials/impatient-r/more-r-key-objects/more-r-numbers/
# seq(0, 1, by=.1)[4] == .3
#  
# [Rmpfr package](https://cran.r-project.org/web/packages/Rmpfr/vignettes/Rmpfr-pkg.pdf)
# sum(mpfr(c(1100,  300, 1100,  500 , 500, 2000,  500), 80)/ 0.78) == (mpfr(6000,80)/ 0.78)
# 
# a =  c(1100,  300, 1100,  500 , 500, 2000,  500) / 0.78
# b = 6000/ 0.78
# sum(a) == b
# a[1]=a[1]-  (sum(a) - b)
# sum(a) == b
# 
# print(sum(a)-b,digits=20)
# 
holdings.split = function(holdings, a, split) {
	n.trades = 1:holdings$n.trades[a]
	
	sum.before.split = sum(holdings$share[n.trades,a]) / split
	
	holdings$share[n.trades,a] = holdings$share[n.trades,a] / split
	
	sum.after.split = sum(holdings$share[n.trades,a])
	
	holdings$share[1,a] = holdings$share[1,a] + sum.before.split - sum.after.split
}


bt.run.share.ex.n.days = function(index.today, index.hist, dates) {
	as.numeric(dates[index.today] - dates[index.hist])
}



#download both div and splits in ome file - getSplits
#http://ichart.finance.yahoo.com/x?s=IBM&a=00&b=2&c=1962&d=09&e=6&f=2015&g=v&y=0&z=30000
#		
# update dividends and compute taxes
tax.update.dividends = function(tax.control, dividend.control, holdings, tax, share, dividend, index, dates) 
{
	# NOTES:
	#
	# dividend.control$foreign.withholding.tax are taken out from dividend cash when dividend is paid. i.e.
	# asset.cashflow = sum(info$share[i,] * dividends[i,] * 
	#			iif(info$share[i,] < 0, 1, 1 - dividend.control$foreign.withholding.tax)
	#		)
	#
	# these amount can be claimed back; so we assume that it used to pay taxes. i.e.
	# let's assume $100 dividend is due and there is 20% foreign.withholding.tax
	# so only $80 dividend is paid
	#
	# let's also assume that there is a 20% tax on dividends; in that case taxes already paid and dividend is not considered fo tax calculations. i.e. 
	# qualified.div = dividend  * (1 - dividend.control$foreign.withholding.tax / tax.control$dividend$qualified.tax)
	# qualified.div = dividend  * (1 - 20% / 20%) = dividend * 0 = 100 * 0 = 0
	#
	# another example: assume 20% foreign.withholding.tax and 40% tax on dividends; in this case another $20 in taxes are due
	# at 40% tax on dividends that corresponds to $50 dividend	
	# qualified.div = dividend  * (1 - dividend.control$foreign.withholding.tax / tax.control$dividend$qualified.tax)
	# qualified.div = dividend  * (1 - 20% / 40%) = dividend * 0.5 = 100 * 0.5 = 50
	#
	
	
	# offset by foreign.withholding.tax
	qualified.div = dividend  * (1 - dividend.control$foreign.withholding.tax / iif(tax.control$dividend$qualified.tax == 0, 1, tax.control$dividend$qualified.tax))
	non.qualified.div = dividend * (1 - dividend.control$foreign.withholding.tax / iif(tax.control$dividend$nonqualified.tax == 0, 1, tax.control$dividend$nonqualified.tax))
	qualified.div.adj = non.qualified.div.adj = 0
					
	# long position, separate between qualified and non.qualified
	pos.cashflow = share > 0 & dividend > 0
	# loop over all qualified divs
	for(a in which(pos.cashflow & !tax.control$dividend$nonqualified)) {
		n.trades = 1:holdings$n.trades[a]							
		trade.days = bt.run.share.ex.n.days(index, holdings$date[n.trades,a], dates)
			
		# find holdings that breach qualified.min.holding.period and move them from qualified to non.qualified
		nonqualified.trade.days = trade.days < tax.control$dividend$qualified.min.holding.period
		if(sum(nonqualified.trade.days) == 0) next
		qualified.div.adj = qualified.div.adj - qualified.div[a] * sum(holdings$share[n.trades,a][nonqualified.trade.days])
		non.qualified.div.adj = non.qualified.div.adj + non.qualified.div[a] * sum(holdings$share[n.trades,a][nonqualified.trade.days])
	}
						
	tax$qualified.div[index] = tax$qualified.div[index] + qualified.div.adj +  sum(
		(share * qualified.div)[pos.cashflow & !tax.control$dividend$nonqualified]
		)
		
	tax$non.qualified.div[index] = tax$non.qualified.div[index] + non.qualified.div.adj + sum(
		(share * non.qualified.div)[pos.cashflow & tax.control$dividend$nonqualified]
		)
	
	#http://www.fool.com/personal-finance/taxes/2002/12/06/dividends-paid-on-short-sales.aspx
	# short position - add to cost basis
	for(a in which(share < 0 & dividend > 0)) {
		n.trades = 1:holdings$n.trades[a]							
		holdings$price[n.trades,a] = holdings$price[n.trades,a] - dividend[a]
	}
}				
		

# record wash sale, called every time there is a loosing trade
record.wash.sale = function(a, n.trades, pnl, price, trigger, holdings, wash.sale, tax.control) {
	if( is.na(tax.control$capital.gain$wash.sale.min.holding.period) ) return()
	
	wash.sale.index = pnl[n.trades] < 0
	n.wash.sale.index = sum(wash.sale.index)
	
	if( n.wash.sale.index > 0 ) {
		# increase size of arrays by 100 if needed to store trades
		if( wash.sale$n.trades[a] + n.wash.sale.index > nrow(wash.sale$share) ) {
			n = ncol(wash.sale$date)
			wash.sale$date = rbind(wash.sale$date, matrix(0, 100, n))
			wash.sale$share = rbind(wash.sale$share, matrix(0, 100, n))
			wash.sale$loss.per.share = rbind(wash.sale$loss.per.share, matrix(0, 100, n))
			wash.sale$long.term = rbind(wash.sale$long.term, matrix(T, 100, n))
		}
	
		n1 = wash.sale$n.trades[a] + 1:n.wash.sale.index					
		wash.sale$date[n1,a] = holdings$date[n.trades,a][wash.sale.index]
		wash.sale$share[n1,a] = holdings$share[n.trades,a][wash.sale.index]
		wash.sale$loss.per.share[n1,a] = (price[a] - holdings$price[n.trades,a])[wash.sale.index]
		wash.sale$long.term[n1,a] = trigger[n.trades][wash.sale.index]
		wash.sale$n.trades[a] = wash.sale$n.trades[a] + n.wash.sale.index
	}
}

# check if wash sale took place, only called on new trade enrty
check.wash.sale = function(a, dates, tax, tax.control, holdings, wash.sale) {
	# no losses recorded
	if( is.na(tax.control$capital.gain$wash.sale.min.holding.period) || wash.sale$n.trades[a] == 0) return()
		
	i = holdings$n.trades[a]
	index = holdings$date[i,a]
	
	n.trades = 1:wash.sale$n.trades[a]	
	trade.days = bt.run.share.ex.n.days(index , wash.sale$date[n.trades,a], dates)
		trigger = trade.days <= tax.control$capital.gain$wash.sale.min.holding.period
		n.trigger = sum(trigger)
	
	# all loses are past the wash.sale.min.holding.period
	if( n.trigger == 0 ) {
		wash.sale$n.trades[a] = 0
		return()
	}
	
	
	# detected wash sale, the proper way is to split trade to match number of shares for each loss trade
	# we are simplifying here; no trade splitting, just cost adjusment
	share1 = abs(holdings$share[i,a])
	run.share = cumsum(abs(wash.sale$share[n.trades,a][trigger]))
	n1 = which( run.share > share1 )
	
	# entry has more shares than loss
	if( len(n1) == 0 || mlast(run.share) == share1 ) {
		loss = wash.sale$loss.per.share[n.trades,a][trigger]*wash.sale$share[n.trades,a][trigger]
	
		holdings$price[i,a] = holdings$price[i,a] - sum(loss) / holdings$share[i,a]	
		wash.sale$n.trades[a] = 0
		
		tax$long.term.cap[index] = tax$long.term.cap[index] - sum( iif(wash.sale$long.term[n.trades,a][trigger], loss, 0) )
		tax$short.term.cap[index] = tax$short.term.cap[index] - sum( iif(wash.sale$long.term[n.trades,a][trigger], 0, loss) )
		
		return()	
	}
	
	# there are suffient losses
	n1 = n1[1]
	trigger.index = which(trigger)
	
	
	# matching exactly
	if( run.share[n1] == share1 ) {
		loss = wash.sale$loss.per.share[n.trades,a][trigger][1:n1]*wash.sale$share[n.trades,a][trigger][1:n1]
		
		holdings$price[i,a] = holdings$price[i,a] - sum(loss) / holdings$share[i,a]	
	
		tax$long.term.cap[index] = tax$long.term.cap[index] - sum( iif(wash.sale$long.term[n.trades,a][trigger][1:n1], loss, 0) )
		tax$short.term.cap[index] = tax$short.term.cap[index] - sum( iif(wash.sale$long.term[n.trades,a][trigger][1:n1], 0, loss) )
					
		n1 = n1 + 1	
	} else {
		# split trade
		share.left = run.share[n1] - share1
		last.index = trigger.index[n1]
		wash.sale$share[last.index,a] = wash.sale$share[last.index,a] - share.left

		loss = wash.sale$loss.per.share[n.trades,a][trigger][1:n1]*wash.sale$share[n.trades,a][trigger][1:n1]
		
		holdings$price[i,a] = holdings$price[i,a] - sum(loss) / holdings$share[i,a]	
	
		tax$long.term.cap[index] = tax$long.term.cap[index] - sum( iif(wash.sale$long.term[n.trades,a][trigger][1:n1], loss, 0) )
		tax$short.term.cap[index] = tax$short.term.cap[index] - sum( iif(wash.sale$long.term[n.trades,a][trigger][1:n1], 0, loss) )
		
		wash.sale$share[last.index,a] = share.left
	}
				
	# shift arrays
	if(n1 > 1) {		
		from.index = trigger.index[n1]:wash.sale$n.trades[a]
		to.index = 1:len(from.index)
		wash.sale$date[to.index,a] = wash.sale$date[from.index,a]
		wash.sale$share[to.index,a] = wash.sale$share[from.index,a]
		wash.sale$loss.per.share[to.index,a] = wash.sale$loss.per.share[from.index,a]
		wash.sale$long.term[to.index,a] = wash.sale$long.term[from.index,a]
		wash.sale$n.trades[a] = len(to.index)												
	}
}








# update holdings and compute taxes
tax.update.holdings = function(tax.control, holdings, tax, wash.sale, share0, share1, price, index, dates) 
{				
	n = len(price)
		
	# NOTES:
	#
	# n.trades = 1:holdings$n.trades[a]
	# share0 = sum(holdings$share[n.trades,a]) 
	#
	# capital gains/loses
	# sum(iif(share0 > share1, (share0 - share1)*(price-holdings$price[1,]), 0))
	# tax$short.term.cap[index] + tax$long.term.cap[index]
	
	if( any(!equal.check(share0, sapply(1:n, function(a) sum(iif(holdings$n.trades[a] > 0, holdings$share[1:holdings$n.trades[a],a], 0))  )) )	)
		cat('Mismatch holding shares', index, '\n')
	
	
	for(a in (1:n)[share0 != share1]) {				
		n.trades = 1:holdings$n.trades[a]
		
		# flip
		if( (share0[a] * share1[a]) <= 0) {
			# liquidate all
			if(share0[a] != 0) {
				pnl = holdings$share[n.trades,a] * (price[a] - holdings$price[n.trades,a])
				trade.days = bt.run.share.ex.n.days(index, holdings$date[n.trades,a], dates)
					trigger = trade.days > tax.control$capital.gain$long.term.min.holding.period
				tax$long.term.cap[index] = tax$long.term.cap[index] + sum(iif(trigger, pnl, 0))
				tax$short.term.cap[index] = tax$short.term.cap[index] + sum(iif(trigger, 0, pnl))
				
				# record losses for wash sale checking
				record.wash.sale(a, n.trades, pnl, price, trigger, holdings, wash.sale, tax.control)
				
				holdings$n.trades[a] = 0
			}
			# enter new position
			if(share1[a] != 0) {
				holdings$share[1,a] = share1[a]
				holdings$price[1,a] = price[a]
				holdings$date[1,a] = index				
				holdings$n.trades[a] = 1
# wash sale - check	
check.wash.sale(a, dates, tax, tax.control, holdings, wash.sale)

			
			} else
				holdings$n.trades[a] = 0
		} else {			
			# add
			if( abs(share1[a]) > abs(share0[a]) ) {
				# increase size of arrays by 100 if needed to store trades
				if( holdings$n.trades[a] + 1 > nrow(holdings$share) ) {
					holdings$share = rbind(holdings$share, matrix(0, 100, n))
					holdings$price = rbind(holdings$price, matrix(0, 100, n))
					holdings$date  = rbind(holdings$date, matrix(0, 100, n))
				}
				n1 = holdings$n.trades[a] + 1
				holdings$share[n1,a] = share1[a] - share0[a]
				holdings$price[n1,a] = price[a]
				holdings$date[n1,a]  = index				
				holdings$n.trades[a] = n1			
# wash sale - check				
check.wash.sale(a, dates, tax, tax.control, holdings, wash.sale)
			}
			
			# remove, assume FIFO - first-in first-out
			if( abs(share1[a]) < abs(share0[a]) ) {
				remove.share = share0[a] - share1[a]
			
				pnl = holdings$share[n.trades,a] * (price[a] - holdings$price[n.trades,a])
				
				trade.days = bt.run.share.ex.n.days(index, holdings$date[n.trades,a], dates)
					trigger = trade.days > tax.control$capital.gain$long.term.min.holding.period

				run.share = cumsum(holdings$share[n.trades,a])
				n1 = which( abs(run.share) >= abs(remove.share) )[1]
				
				
				# matching exactly
				if( run.share[n1] == remove.share ) {
					tax$long.term.cap[index] = tax$long.term.cap[index] + sum(iif(trigger, pnl, 0)[1:n1])
					tax$short.term.cap[index] = tax$short.term.cap[index] + sum(iif(trigger, 0, pnl)[1:n1])
					record.wash.sale(a, 1:n1, pnl, price, trigger, holdings, wash.sale, tax.control)
					n1 = n1 + 1	
				} else {
				# split trade
					share.left = run.share[n1] - remove.share

					#pnl[n1] = pnl[n1] - (holdings$share[n1,a] - share.left) *
					#	(price[a] - holdings$price[n1,a])					
					pnl[n1] = pnl[n1] - share.left *
						(price[a] - holdings$price[n1,a])					
					
					tax$long.term.cap[index] = tax$long.term.cap[index] + sum(iif(trigger, pnl, 0)[1:n1])
					tax$short.term.cap[index] = tax$short.term.cap[index] + sum(iif(trigger, 0, pnl)[1:n1])

					holdings$share[n1,a] = holdings$share[n1,a] - share.left
					record.wash.sale(a, 1:n1, pnl, price, trigger, holdings, wash.sale, tax.control)
					
					holdings$share[n1,a] = share.left
				}
								
				# shift arrays
				if(n1 > 1) {					
					from.index = n1:holdings$n.trades[a]
					to.index = 1:len(from.index)					
					holdings$share[to.index,a] = holdings$share[from.index,a]
					holdings$price[to.index,a] = holdings$price[from.index,a]
					holdings$date[to.index,a] = holdings$date[from.index,a]
					holdings$n.trades[a] = len(to.index)												
				}
			}
		}
		
		
		if( !equal.check(share1[a], sum(iif(holdings$n.trades[a] > 0, holdings$share[1:holdings$n.trades[a],a], 0))))
			cat('a', a, index, '\n')
	}
	
	if( any(!equal.check(share1, sapply(1:n, function(a) sum(iif(holdings$n.trades[a] > 0, holdings$share[1:holdings$n.trades[a],a], 0))  )) )	)
		cat('Mismatch holding shares', index, '\n')

	
	
}	

equal.check = function(a,b,eps=1e-10) abs(a - b) < eps
		

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
#' minimally update portfolio to allocate cashflow
#' only add new positions, such that abs(shares) only increases
#'
#' @export 
###############################################################################
bt.run.share.ex.invest = function
(
	weight.new,
	weight.prev,
	weight.change.index,
	price,
	share,
	cash,
	cashflow,
	commission,
	lot.size,
	silent=T,
	# control allocation if round lot is enabled
	control = default.round.lot.control()
) {
# Basic cases, try to satisfy with cash
	if(cashflow >= 0) {
		# do nothing - need to be corrected!!!
		return(list(share = share, cash = cash, com = 0))
	} else {
		# current cash
		current.cash = sum(price * share) + cash - sum(price * abs(share))
		# value - long
		current.cash = (sum(price * share) + cash) - sum((price * share)[share>0])
		if(current.cash >= 0)
			return(list(share = share, cash = cash, com = 0))
		# otherwise continue to satisfy the cash requirement		
	}
	
if(F) {	
# Case A, simple: allocate abs(cashflow) proportionate to weight and to existing share / cash
# does not work for negative cashflows because we need cash to pay commissions
	n = len(share)
	out = bt.run.share.ex.allocate(weight.new = weight.new, weight.prev = rep(0, n),
		weight.change.index = rep(T, n),
		price = price, share = rep(0, n), cash = abs(cashflow),
		commission, lot.size, control = control)
	if(cashflow >= 0)
		return(list(share = share + out$share, cash = (cash - cashflow) + out$cash, com = out$com))
	else {
		out = bt.run.share.ex.allocate(weight.new = weight.new, weight.prev = rep(0, n),
			weight.change.index = rep(T, n),
			price = price, share = rep(0, n), cash = abs(cashflow) + 5 * out$com,
			commission, lot.size, control = control)
		
		return(list(share = share - out$share, cash = cash + sum(share*price) - (sum((share - out$share)*price) + out$com), com = out$com))
	}
}	
	
# Case B, better: do full rebalance, 
# for cashflow(+) freeze weights that has abs(share.new) < abs(share) and repeat
# for cashflow(-) freeze weights that has abs(share.new) > abs(share) and repeat
	out = bt.run.share.ex.allocate(weight.new = weight.new, weight.prev = weight.prev,
		weight.change.index = weight.change.index,
		price = price, share = share, cash = cash,
		commission, lot.size, control = control,
		cashflow = cashflow)
	if(cashflow >= 0) {
		if( any(abs(out$share) < abs(share)) ) {
			weight.change.index[abs(out$share) < abs(share)] = F 
			
			out = bt.run.share.ex.allocate(weight.new = weight.new, weight.prev = weight.prev,
				weight.change.index = weight.change.index,
				price = price, share = share, cash = cash,
				commission, lot.size, control = control)			
		}
	} else {
		if( any(abs(out$share) > abs(share)) ) {
			weight.change.index[abs(out$share) > abs(share)] = F 
			
			out = bt.run.share.ex.allocate(weight.new = weight.new, weight.prev = weight.prev,
				weight.change.index = weight.change.index,
				price = price, share = share, cash = cash,
				commission, lot.size, control = control,
				cashflow = cashflow)			
		}	
	}
	
	out
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
	control = default.round.lot.control(),
	cashflow = 0
) {
	# total value, as if everything is liquidated
	value = sum(price * share) + cash


	
	
	# make lot size fractional over-vise run into rounding problem# i.e.
	# print(762.18,digits=20)
	# 762.17999999999995
	# print(76218/100,digits=20)
	# 762.17999999999995
	# print(76218*0.01,digits=20)
	# 762.18000000000006
	#if( len(lot.size) > 0 && all(lot.size != 0) )
	#	lot.size1 = sapply(lot.size, function(x) MASS:::.rat(x)$rat)
	
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
	if( len(lot.size) == 0 || all(lot.size == 0) )
		return(rep.row(allocate(value, share), 3))
	
	new.total.weight = sum(abs(weight.new[weight.change.index]))
	if(new.total.weight == 0) {
		shares = rep.row(share, 2)
		shares[2, weight.change.index] = 0
	} else {
		allocate.value = value * sum(abs(weight.new)) - sum(abs(share * price)[!weight.change.index])
		lot.size = lot.size[weight.change.index]
		w = weight.new[weight.change.index]/ new.total.weight
		p = price[weight.change.index]
		
		shares = rep.row(share, 3)
		shares[2, weight.change.index] = round.lot.basic(w, p, allocate.value, lot.size)
		shares[3, weight.change.index] = round.lot.basic.base(w, p, allocate.value, lot.size)
	}
	shares	
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
		share1 = allocate.lot(value - 2 * com, share, lot.size)

		# drop current allocation from possible ones
		if( cashflow < 0 )
			if( (value - sum((price * share)[share > 0])) < 0 )
				share1 = share1[-1,,drop=F]		
		
		# create list of possible portfolios and compute commisions, cash, weight diff
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

#' @export 
round.to = function(x, to) {
	sign(x) * floor(abs(x) / to) * to
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
bt.unadjusted.add.div.split = function(
	data.raw, 
	yahoo.round.up.nearest.cents=F,
	infer.div.split.from.adjusted=F # caution, please see example in test.implied.div.split
) {
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

		if(infer.div.split.from.adjusted) {
			close = coredata(Cl(price))
			adjusted = coredata(Ad(price))
	
#Determine Dividend and Split from adjusted and un-adjusted prices
#Implied Dividend: (Pt1+Dt1)/Pt0 = At1/At0 => Dt1 = Pt0 * At1/At0 - Pt1
#Implied Split: St1 * Pt1/Pt0 = At1/At0 => St1 = Pt1/Pt0 * At0/At1			
			implied.split = close / mlag(close) * mlag(adjusted) / adjusted
				isplit.index = ifna(implied.split < 0.9 | implied.split > 1.2,F)
			isplit = implied.split[isplit.index]
				isplit = round(100 * isplit) / 100
		
			implied.div = mlag(close) * adjusted / mlag(adjusted) - close
				idiv.index = ifna(implied.div > 1e-3, F) & !isplit.index
			idiv = implied.div[idiv.index]
				idiv = round(1e3 * idiv) / 1e3
				
			price$Dividend[idiv.index] = idiv
			price$Split[isplit.index] = isplit 				
			
			
		} else {
			# need full history	
			dividend = getDividends(ticker, from = '1900-01-01')	
			split = getSplits(ticker, from = '1900-01-01')	
				split = split[split > 0]
				dividend = dividend[dividend > 0]

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
		}		
		
		# round up to the nearest cents - this is the only way to match IBM prices
		if(yahoo.round.up.nearest.cents) {
			map.col = unlist(find.names('Close,Open,High,Low,Adjusted', price, F))
			price[,map.col] = ceiling(100 * price[,map.col]) / 100 
		}
			
		data.raw[[ticker]] = price
	}
}



#Determine Dividend and Split from adjusted and un-adjusted prices
#Implied Dividend: (Pt1+Dt1)/Pt0 = At1/At0 => Dt1 = Pt0 * At1/At0 - Pt1
#Implied Split: St1 * Pt1/Pt0 = At1/At0 => St1 = Pt1/Pt0 * At0/At1
test.implied.div.split = function(ticker) {
	#*****************************************************************
	# Load historical data
	#*****************************************************************
	load.packages('quantmod')
	
	ticker = 'IBM'
		
	data <- new.env()
	getSymbols.extra(ticker, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T, set.symbolnames=T)
	
	# need full history	
	dividend = getDividends(ticker, from = '1900-01-01')	
	split = getSplits(ticker, from = '1900-01-01')	

	# un-adjust dividends for splits (Yahoo already adjusts div for splits)
	if(is.xts(split) && is.xts(dividend) && nrow(split) > 0 && nrow(dividend) > 0)
		dividend1 = dividend * 1/adjRatios(splits=merge(split, index(dividend)))[,1]
	else
		dividend1 = dividend

	close = Cl(data$IBM)
	adjusted = Ad(data$IBM)
	
	implied.split = close / mlag(close) * mlag(adjusted) / adjusted
		isplit.index = implied.split < 0.8 | implied.split > 1.2
	isplit = implied.split[isplit.index]
		isplit = round(100 * isplit) / 100
		
	cbind(isplit['1970::'], split['1970::'])

	implied.div = mlag(close) * adjusted / mlag(adjusted) - close
	idiv.index = implied.div > 1e-3
	idiv = implied.div[idiv.index & !isplit.index]
		idiv = round(1e3 * idiv) / 1e3
	len(idiv['1970::'])
    len(dividend1['1970::'])

	setdiff( index(dividend1['1970::']), index(idiv['1970::']))
	setdiff( index(idiv['1970::']), index(dividend1['1970::']) )
    
	cbind(idiv['1970::'], dividend1['1970::'])	
	
	#*****************************************************************
	# Check DOW components
	#*****************************************************************
	tickers = dow.jones.components()

for(ticker in tickers) {
		
	data <- new.env()
	getSymbols.extra(ticker, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
	
	# need full history	
	dividend = getDividends(ticker, from = '1900-01-01')	
	split = getSplits(ticker, from = '1900-01-01')	
		split = split[split > 0]
		dividend = dividend[dividend > 0]

	# un-adjust dividends for splits (Yahoo already adjusts div for splits)
	if(is.xts(split) && is.xts(dividend) && nrow(split) > 0 && nrow(dividend) > 0)
		dividend1 = dividend * 1/adjRatios(splits=merge(split, index(dividend)))[,1]
	else
		dividend1 = dividend

	close = Cl(data[[ticker]])
	adjusted = Ad(data[[ticker]])
	
	implied.split = close / mlag(close) * mlag(adjusted) / adjusted
		isplit.index = ifna(implied.split < 0.9 | implied.split > 1.1, F)
	isplit = implied.split[isplit.index]
		isplit = round(100 * isplit) / 100
		
	if(len(isplit)>0)		
		cat(ticker, 'SPL', len(isplit['1970::']) - len(split['1970::']), max(round(isplit['1970::'],3) - round(split['1970::'],3)), '\n')
	else
		cat(ticker, 'SPL', len(isplit['1970::']) - len(split['1970::']), '\n')		
	#cbind(round(isplit['1970::'],3), round(split['1970::'],3))

	implied.div = mlag(close) * adjusted / mlag(adjusted) - close
	idiv.index = ifna(implied.div > 1e-3, F)
	idiv = implied.div[idiv.index & !isplit.index]
		idiv = round(1e3 * idiv) / 1e3
	len(idiv['1970::'])
    len(dividend1['1970::'])

	cat(ticker, 'DIV', len(idiv['1970::']) - len(dividend1['1970::']),    
		len(setdiff( index(dividend1['1970::']), index(idiv['1970::']))),
		len(setdiff( index(idiv['1970::']), index(dividend1['1970::']) )),
		max(round(idiv['1970::'],3)- round(dividend1['1970::'],3)), '\n')
	
	setdiff( index(dividend1['1970::']), index(idiv['1970::']))
	setdiff( index(idiv['1970::']), index(dividend1['1970::']) )
    
	#cbind(round(idiv['1970::'],3), round(dividend1['1970::'],3))
}	
		
}	
    
#*****************************************************************
# Problems with infered div and split data
#*****************************************************************
# KO has wrong dividned, split adjusted, there is Aug 13, 2012	2: 1 Stock Split
# http://finance.yahoo.com/q/hp?s=KO&a=00&b=2&c=1962&d=09&e=14&f=2015&g=v
# Nov 28, 2001	0.09 Dividend
# Sep 12, 2001	0.18 Dividend
# Jun 13, 2001	0.09 Dividend
#
# http://www.nasdaq.com/symbol/ko/dividend-history
# 11/28/2001 	Cash	0.18 	-- 	11/28/2001 	--
# 6/13/2001 	Cash	0.18 	-- 	6/15/2001 	--
# 3/13/2001 	Cash	0.18 	-- 	3/15/2001 	--
#
# implied dividend is correct
#http://finance.yahoo.com/q/hp?s=KO&a=08&b=2&c=2001&d=09&e=14&f=2001&g=d
#> idiv['2001']
#2001-03-13 0.176
#2001-06-13 0.180
#2001-09-17 0.218
#2001-11-28 0.176
#
#============================================================
#
# DIS split is not detected
#http://finance.yahoo.com/q/hp?s=DIS&a=00&b=2&c=1962&d=09&e=14&f=2015&g=v
#Jun 13, 2007	1014: 1000 Stock Split
#
#============================================================
#
# DD  split is not detected
#http://finance.yahoo.com/q/hp?s=DD&a=00&b=2&c=1962&d=09&e=14&f=2015&g=v
#Jul 1, 2015	1053: 1000 Stock Split
# => hence incorrect div is implied
#
#============================================================
#
# MSFT big div is treated as split
# Nov 15, 2004	3.08 Dividend
# http://finance.yahoo.com/q/hp?s=MSFT&a=02&b=13&c=1986&d=09&e=14&f=2015&g=v
#
#============================================================
#
# PG has split and div on the same day
#May 19, 1970	0.02188 Dividend
#May 19, 1970	2: 1 Stock Split
#http://finance.yahoo.com/q/hp?s=PG&a=00&b=2&c=1970&d=09&e=14&f=2015&g=v&z=66&y=132
#
#============================================================
#
# VZ missing splits -spinoffs
#Jul 2, 2010	1000000: 937889 Stock Split
#Apr 1, 2008	100000: 99537 Stock Split
#Nov 20, 2006	100000: 96334 Stock Split
#
#============================================================



###############################################################################	
#' Test for bt.run.share.unadjusted functionality
#' @export 
###############################################################################
bt.run.share.unadjusted.test.data = function() {
	#*****************************************************************
	# Load historical data
	#*****************************************************************
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
#' Summarize bt.run.share.ex events
#' @export 
###############################################################################
bt.make.cashflow.event.summary.table = function(bt, to.text=F) {
	if( is.null(bt$cashflow) ) return()
	
	
	index = rowSums(bt$cashflows != 0) > 0
	
	# create summary table: event.type, date, shares, cash, com, div, value	
	out = data.frame(
		Cashflow = bt$cashflow,
		Fee.Rebate = bt$fee.rebate,
		bt$cashflows
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
	load.packages('quantmod')
	
	tickers = 'IBM'
		
	data = env()
	getSymbols.extra(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T, set.symbolnames=T)
		#bt.start.dates(data)
		
		# copy unadjusted prices
		data.raw = env(data)
		data.raw1 = env(data)
	
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
	# For each asset, append dividend and split columns
	#****************************************************************** 	
data.raw = data.raw1	
	bt.unadjusted.add.div.split(data.raw, yahoo.round.up.nearest.cents = T, infer.div.split.from.adjusted=T)
	
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
	models$test.unadjusted1 = bt.run.share.ex(data.raw, clean.signal=F, silent=F, commission=commission,
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
	#data.raw1 = env(data.raw)
	#data.raw = data.raw1
	#bt.unadjusted.add.div.split(data.raw, infer.div.split.from.adjusted=T)
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
		dividend.control = list(foreign.withholding.tax = 30/100)
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



###############################################################################	
#' Test for bt.run.share.ex cashflow functionality
#' @export 
###############################################################################
bt.run.share.ex.test.cashflow = function() {
	#*****************************************************************
	# Load historical data
	#*****************************************************************
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
	  data$weight[period.ends,] = ntop(prices[period.ends,], n)
	models$test.ex.lot.cashflow = bt.run.share.ex(data, clean.signal=F, silent=F, commission=commission, 
		lot.size=50,
		cashflow.control = list(
			monthly.income = list(
				cashflows = event.at(prices, 'quarter', 1000, offset=0),
				invest = 'cash',
				type = 'regular'
			)
		)
	)
	
	# info tables
	mlast(bt.make.trade.event.summary.table(models$test.ex.lot.cashflow), 20)
	
	mlast(bt.make.cashflow.event.summary.table(models$test.ex.lot.cashflow), 20)
	
	matplot(cbind(
		models$test.ex.lot$value,
		models$test.ex.lot.cashflow$value
	), type='l')
	

	#*****************************************************************
	# Report
	#****************************************************************** 	
	#strategy.performance.snapshoot(models, T)

	#plotbt.transition.map(models$test.unadjusted$weight)
	
	# put all reports into one pdf file
	pdf(file = 'report.c.pdf', width=8.5, height=11)		
		strategy.performance.snapshoot(models, data=data)		
	dev.off()
}	



# Tests
bt.run.share.ex.invest.test = function() {

	# Start with Cash 
	#commission = list(cps = 0.0, fixed = 0.0, percentage = 0.0)
	commission = list(cps = 0.01, fixed = 1.0, percentage = 0.0)
	weight.prev = c(10,0) / 10
	share = c(100, 0)
	price = c(1,2)
	cash = 0
	lot.size=c()
	
	weight.new = c(10,0) / 10
	weight.change.index = c(T, T)
	cashflow = -10
	cash = cash + cashflow

	a = bt.run.share.ex.invest(weight.new,weight.prev,weight.change.index,price,share,cash,cashflow,commission,lot.size,F)

	a
	
	
	
}	


###############################################################################	
#' Test for bt.run.share.ex tax functionality
#' @export 
###############################################################################
bt.run.share.ex.test.tax = function() {
	#*****************************************************************
	# Load historical data
	#*****************************************************************
	load.packages('quantmod')
	
	tickers = 'SPY'
	tickers = 'SPY,XLY,XLP,XLE,XLF,XLV,XLI,XLB,XLK,XLU'
	tickers = 'AAPL,IBM,VTI,IEV,EWJ,EEM,RWX,DBC,GLD,TLT,IEF,SHY'
		
	data <- new.env()
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
#source('../bt.share.r')
	
	prices = data$prices
	  n = ncol(prices)
	  nperiods = nrow(prices)
	
	period.ends = date.ends(data$prices,'months')
	  
	models = list()
	
	commission = list(cps = 0.01, fixed = 1.0, percentage = 0.0)
	
	weights = ntop(prices[period.ends,], n)
	
	#*****************************************************************
	# Buy Hold
	#******************************************************************
	data$weight[] = NA
	  #data$weight[1,] = 1
	  data$weight[period.ends,] = weights
	models$test = bt.run.share(data, clean.signal=F, silent=F, commission=commission)
	
	data$weight[] = NA
	  #data$weight[1,] = 1
	  data$weight[period.ends,] = weights
	models$test.ex = bt.run.share.ex(data, clean.signal=F, silent=F, commission=commission)

	data$weight[] = NA
	  #data$weight[1,] = 1
	  data$weight[period.ends,] = weights
	models$test.ex.lot = bt.run.share.ex(data, clean.signal=F, silent=F, commission=commission, lot.size=50)
	
	
	data$weight[] = NA
	  data$weight[period.ends,] = weights
	models$test.ex.lot.tax = bt.run.share.ex(data, clean.signal=F, silent=F, commission=commission, 
		lot.size=50,
		control = list(round.lot = list(select = 'minimum.turnover', diff.target = 5/100)),
		
		# enable taxes
		tax.control = default.tax.control(),
		cashflow.control = list(
			taxes = list(
				cashflows = event.at(prices, 'year', offset=60),
				cashflow.fn = tax.cashflows,
				invest = 'cash',
				type = 'fee.rebate'
			)
		)
	)
	

	
	# info tables
	mlast(bt.make.trade.event.summary.table(models$test.ex.lot.tax), 20)
	
	mlast(bt.make.cashflow.event.summary.table(models$test.ex.lot.tax), 20)
	
	
	#*****************************************************************
	# Report
	#****************************************************************** 	
	#strategy.performance.snapshoot(models, T)

	#plotbt.transition.map(models$test.unadjusted$weight)
	
	# put all reports into one pdf file
	pdf(file = 'report.t.pdf', width=8.5, height=11)
		
		strategy.performance.snapshoot(models, data=data)
		
	dev.off()
 
		
	

	
	
		
	#*****************************************************************
	# For each asset, append dividend and split columns
	#****************************************************************** 	
	bt.unadjusted.add.div.split(data.raw)
	
	bt.prep(data.raw, align='remove.na', fill.gaps = T)


	#*****************************************************************
	# New Back-test
	#******************************************************************
	data.raw$weight[] = NA
	  data.raw$weight[period.ends,] = weights
	models$test.unadjusted = bt.run.share.ex(data.raw, clean.signal=F, silent=F, commission=commission,
		lot.size=50, adjusted = F)
	  
	data.raw$weight[] = NA
	  data.raw$weight[period.ends,] = weights
	models$test.unadjusted1 = bt.run.share.ex(data.raw, clean.signal=F, silent=F, commission=commission, 
		lot.size=50, 
		adjusted = F,
		control = list(round.lot = list(select = 'minimum.turnover', diff.target = 5/100)),
		dividend.control = list(foreign.withholding.tax = 30/100)
	)		
	

	
	data$weight[] = NA
	  data$weight[period.ends,] = weights
	models$test.unadjusted.tax = bt.run.share.ex(data.raw, clean.signal=F, silent=F, commission=commission, 
		lot.size=50,
		control = list(round.lot = list(select = 'minimum.turnover', diff.target = 5/100)),
		
		adjusted = F,
		# enable taxes
		tax.control = default.tax.control(),
		cashflow.control = list(
			taxes = list(
				cashflows = event.at(prices, 'year', offset=60),
				cashflow.fn = tax.cashflows,
				invest = 'cash',
				type = 'fee.rebate'
			)
		)
	)
	
	
	# info tables
	mlast(bt.make.trade.event.summary.table(models$test.unadjusted.tax), 20)
	
	mlast(bt.make.cashflow.event.summary.table(models$test.unadjusted.tax), 20)
	

	#*****************************************************************
	# Dig dipper
	#****************************************************************** 	
	models$test1 = models$test.ex.lot.tax
	models$test2 = models$test.unadjusted.tax

	look.at.taxes(models$test1)['2007']
	look.at.taxes(models$test2)['2007']
		
	tax.summary(models$test1)
	tax.summary(models$test2)
	
	tax.summary(models$test1, function(x) as.numeric(format(x,'%Y%m')))[1:14,]	
	tax.summary(models$test2, function(x) as.numeric(format(x,'%Y%m')))[1:14,]	

	data$prices[period.ends,][1:14,1:3]
	data.raw$prices[period.ends,][1:14,1:3]
	
	bt.make.trade.event.summary.table(models$test1)[1:14,]
	bt.make.trade.event.summary.table(models$test2)[1:36,]

	
		
	
		
	
	if(F) {
		models$test.unadjusted.tax$long.term.cap	
		models$test.unadjusted.tax$short.term.cap

		models$test.unadjusted.tax$qualified.div
		models$test.unadjusted.tax$non.qualified.div
	}

	
	#*****************************************************************
	# Report
	#****************************************************************** 	
	# put all reports into one pdf file
	pdf(file = 'report.t.u.pdf', width=8.5, height=11)
		
		strategy.performance.snapshoot(models, data=data)
		
	dev.off()
 
	
	
	
}	


###############################################################################	
#' Helper functions, subject to change
#' @export 
###############################################################################
event.at = function(x, period = 'month', amount = 1, period.ends = date.ends(x,period), offset = 1) {
	nperiods = nrow(x)
	index = period.ends + offset
		index[index > nperiods] = nperiods
		index[index < 1] = 1
	cashflow = x[index,1]
		cashflow[] = amount
	cashflow
}	


	
#' @export 	
look.at.taxes = function(m) {
	temp = data.frame(m[spl('long.term.cap,short.term.cap,qualified.div,non.qualified.div')])
	temp = make.xts(cbind(temp, total=rowSums(temp)), data$dates)
	temp[temp$total!=0]
}	

#' @export 	
tax.summary = function(m, by.fn=date.year) {
	temp = aggregate(
		m[spl('long.term.cap,short.term.cap,qualified.div,non.qualified.div')], 
		list(year=by.fn(data$dates)), 
		sum
	)
	cbind(temp, total=rowSums(temp))
}

	
###############################################################################	
#' Compute Taxes due at the end of the year
#' @export 
###############################################################################
tax.cashflows = function(info, index, last.index) {


#gall <<- environment() 
#list2vars(gall)
#if(index == 1511)
#matrix(1,1,1)[1,20]



	# index of last year
	ii = date.year(info$dates) == date.year(info$dates[index]) - 1
		
	# check if there is any data
	if(sum(ii) == 0) return(0)
	
	# work with copy of tax environment
	if( is.null(info$tax.copy) ) {
		info$tax.copy = env(
			long.term.cap = info$tax$long.term.cap,
			short.term.cap = info$tax$short.term.cap
		)
	} else {		
		info$tax.copy$long.term.cap[ii] = info$tax$long.term.cap[ii]
		info$tax.copy$short.term.cap[ii] = info$tax$short.term.cap[ii]
	}
	

	# find end of last year
	i = max(which(ii))
	
	# get all capital gains / losses, assume we can carry indefinitely
	long.term.cap = sum(info$tax.copy$long.term.cap[1:i])
	short.term.cap = sum(info$tax.copy$short.term.cap[1:i])

	tax.cashflows.cap.helper = function(neg, pos) {
		if( -neg > pos) {
			neg = neg + pos
			pos = 0
		} else {
			pos = pos + neg
			neg = 0		
		}
		return(list(neg = neg, pos = pos))
	}
			
	if(long.term.cap < 0 && short.term.cap > 0) {
		temp = tax.cashflows.cap.helper(long.term.cap, short.term.cap)
		long.term.cap = temp$neg
		short.term.cap = temp$pos
	} else if(long.term.cap > 0 && short.term.cap < 0) {
		temp = tax.cashflows.cap.helper(short.term.cap, long.term.cap)
		long.term.cap = temp$pos
		short.term.cap = temp$neg
	}
	
	tax = 0
	info$tax.copy$long.term.cap[1:i] = 0
	if(long.term.cap >= 0)
		tax = tax + long.term.cap * info$tax.control$capital.gain$long.term.tax		
	else # carry over remaining loss
		info$tax.copy$long.term.cap[i] = long.term.cap
	
	info$tax.copy$short.term.cap[1:i] = 0
	if(short.term.cap >= 0)
		tax = tax + short.term.cap * info$tax.control$capital.gain$short.term.tax
	else # carry over remaining loss
		info$tax.copy$short.term.cap[i] = short.term.cap

	
	# get all dividends	
	qualified.div = sum(info$tax$qualified.div[ii])
	non.qualified.div = sum(info$tax$non.qualified.div[ii])
	
	tax = tax + qualified.div * info$tax.control$dividend$qualified.tax
	tax = tax + non.qualified.div * info$tax.control$dividend$nonqualified.tax
		
#cat('Tax', index, format(info$dates[index], '%d-%m-%Y'), tax, '\n')	
	
	-tax
}







###############################################################################	
# General notes on new functionality
###############################################################################
#
#
#Using un-adjusted vs adjusted prices in after tax back-test should produce similar results, but not exact because
#============================================
#* adjusted prices back-test assumes that dividends are automatically reinvested, 
#while un-adjusted prices back-test deposits dividends into cash account that is allocated 
#during next re-balance. There might be some commissions associated with deployment of dividends 
#in this case.
#
# i.e. in test.ex.lot.tax we don't pay any taxes since divs are reinvested
#in test.unadjusted.tax we pay taxes on divs plus commisions costs to invest divs
#
#* the un-adjusted prices back-test compute commissions based on actual price of shares at the time, 
#while the adjusted prices back-test compute commissions based on adjusted prices that might very small 
#far back in the history. Hence, there might be a difference in commissions.
#
 



#
#bt.run.share vs bt.run.share.ex - different assumptions in calculations of shares
#=========================================
#the bt.run.share function scales number of shares to the original capital at each rebalance. 
#To glance at SIT back-test logic for bt.run.share, please have a look at
#  https://systematicinvestor.wordpress.com/2013/11/05/commissions/
#
#This problem with bt.run.share function is one of the reason i'm working on 
#bt.run.share.ex. bt.run.share.ex function properly tracks capital evolution 
#and is not causing artificial turnover.
#
#Following simple example that should clarify the logic in bt.run.share.  
#bt.run.share function computes number of shares at each rebalance using following formula, 
#please note capital is fixed and never changes :
#share = weight * capital / price
#
#Following is an extreme case example:
#Let's say capital = $10, weight is 50/50
#
#period one prices are ($1 and $1) hence share = (0.5, 0.5) * $10 / ($1, $1) = we have 5 shares and 5 shares
#period two prices are ($5 and $5) hence share = (0.5, 0.5) * $10 / ($5, $5) = we have 1 share and 1 share
#above introduces artificial turnover that you see with bt.run.share function
#
     

# Testing with stock that  never distributed any dividend (GOOGL). 
#
# The difference bwtn the unadjusted and the adjusted returns. 
#
#Actually GOOGL had split on Apr 3, 2014
#Hence number of shares was adjusted and on the next rebalance, shares were rounded up to closest 100's
#and commisions were paid
#Apr 3, 2014	1998: 1000 Stock Split
#http://finance.yahoo.com/q/hp?s=GOOGL&a=7&b=19&c=2004&d=9&e=14&f=2015&g=d&z=66&y=330
#
#> mlast(bt.make.trade.event.summary.table(models$test.unadjusted.tax), 25)
#            Type  GOOGL     Cash   Com
#2014-03-31 trade  950.0 2737.991 0.000
#2014-04-03 split 1898.1 2737.991 0.000
#2014-04-30 trade 1900.0 1720.699 1.019
#
# The turnover is not null because there is initial turnover when position is started. I.e. Cash -> Equity Allocation
#



# ToDo it would be nice to have an option to do an incremental back-test
# i.e. append new data and only re-run the updated portion of back-test
#
# bt.run - really fast with no bells or whisles
# working directly with xts is alot slower, so use coredata
#' @export 	
bt.run.share.fast <- function
(
	b,					# enviroment with symbols time series
	clean.signal = T,	# flag to remove excessive signal	
	do.lag = 1, 		# lag signal
	capital = 100000,
	lot.size = 0.01
) 
{
	#---------------------------------------------------------
	# process weight
	#---------------------------------------------------------	
	# make sure we don't have any abnormal weights
	weight = b$weight
	weight[is.nan(weight) | is.infinite(weight)] = NA
	weight[!is.na(weight) & is.na(b$prices)] = 0
	
		# lag logic, to be back compatible with bt.run.share
		# default logic is to use current weights to trade at close i.e. no lag
		weight = iif( do.lag == 1, weight, mlag(weight, do.lag - 1) )
	
	weight = coredata(weight)
if(F) {	
		temp = bt.exrem(weight)
		
	if(clean.signal) {
		weight = temp
	} else { # always clean up 0's
		index = ifna(weight == 0, F)
		weight[index] = temp[index]
	}	
}	
	#---------------------------------------------------------
	# process prices
	#---------------------------------------------------------
	prices = coredata(b$prices)
		n = ncol(prices)
		nperiods = nrow(prices)
		
	# find trades
	trade = !is.na(weight)
		trade.index = which(rowSums(trade) > 0)
	
	#---------------------------------------------------------
	# setup event driven back test loop
	#---------------------------------------------------------
	cash.wt = cash = rep(capital, nperiods)
	share.wt = share = matrix(0, nperiods, n)	
	last.trade = 0
	lot.size = map2vector(lot.size, colnames(prices), 1)
	lot.size = rep(1,n)

	for(i in trade.index) {
		if(last.trade > 0) {
			# copy from last trade
			index = (last.trade + 1) : i
				n.index = len(index)
			share.wt[index,] = rep.row(share[last.trade,], n.index)
			cash.wt[index] = cash[last.trade]
			
			share[index,] = rep.row(share[last.trade,], n.index)
			cash[index] = cash[last.trade]
		}
		
		p = prices[i,]
			p[is.na(p)] = 1
		w = weight[i,]
			w[is.na(w)] = 0
		
		# update share[i,] and cash[i]
		value = cash[i] + sum(p * share[i,])
		#share[i,] = value * w / p
		
		# not going to work for missing prices, probbaly need an index
		#share[i,] = round.lot.basic.base(w, p, value, lot.size)
		share[i,] = round.lot.basic(w, p, value, lot.size)
		cash[i] = value - sum(share[i,] * p)
			
		last.trade = i
	}
		
	if( last.trade > 0 & last.trade < nperiods) {
		# copy from last trade
		index = (last.trade + 1) : nperiods
			n.index = len(index)
		share.wt[index,] = rep.row(share[last.trade,], n.index)
		cash.wt[index] = cash[last.trade]
						
		share[index,] = rep.row(share[last.trade,], n.index)
		cash[index] = cash[last.trade]
	}		
	
	# prepare output
	bt = list(type = 'share', capital = capital, share=share)
	bt$value = cash + rowSums(share * prices, na.rm=T)	
	#bt$weight = share * prices / bt$value
	bt$weight = share.wt * prices / (cash.wt + rowSums(share.wt * prices, na.rm=T))

	
	value = c(capital, bt$value)
	bt$ret = (value / mlag(value) - 1)[-1]
    bt$equity = cumprod(1 + bt$ret)

    bt
}








