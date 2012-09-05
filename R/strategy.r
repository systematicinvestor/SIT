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
# Repository of Benchmark Strategies
# Copyright (C) 2012  Michael Kapler
#
# For more information please visit my blog at www.SystematicInvestor.wordpress.com
# or drop me a line at TheSystematicInvestor at gmail
###############################################################################




###############################################################################
# Helper function to load data
###############################################################################
strategy.load.historical.data <- function
(
	tickers = spl('DIA,SPY'),
	dates = '1900::',
	align = 'keep.all',
	fill.gaps = T,
	adjust = T,
	current = F
) 
{
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load.packages('quantmod')	
	
	tickers = spl(tickers)
	data <- new.env()
		for(i in 1:len(tickers)) {
			temp = try(getSymbols(tickers[i], src = 'yahoo', from = '1900-01-01', env = data, auto.assign = T), TRUE)	
			if(inherits(temp, 'try-error'))
				cat(i, 'out of', len(tickers), 'Error Reading', tickers[i], '\n', sep='\t')
			else if(is.null(data[[ tickers[i] ]]))
				cat(i, 'out of', len(tickers), 'Error Reading', tickers[i], '\n', sep='\t')
			else
				cat(i, 'out of', len(tickers), 'Reading', tickers[i], format(range(index(data[[ tickers[i] ]])), '%d-%b-%Y'), '\n', sep='\t')
		}	
		if(adjust) for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
		
		# current quotes logic
		if(current) {
		quotes = getQuote(tickers)
		for(i in ls(data))
			if( last(index(data[[i]])) < as.Date(quotes[i, 'Trade Time']) ) {
				data[[i]] = rbind( data[[i]], make.xts(quotes[i, spl('Open,High,Low,Last,Volume,Last')],
					as.Date(quotes[i, 'Trade Time'])))
			}
		}
		
	bt.prep(data, align=align, dates=dates, fill.gaps=fill.gaps)
	
	return(data)
}





###############################################################################
# Helper function to create Barplot with strategy stats
###############################################################################
performance.barchart.helper <- function(out, names, custom.order) 
{
	# Bar chart
	layout(mat=matrix(1:4, 2, 2, byrow=FALSE)) 	
	par(mar=c(4, 3, 2, 2))
	col = spl('lightgray,red')
	
	names(custom.order) = spl(names)
	for(i in names(custom.order)) {
		y = as.double(out[i,])
		index = order(y, decreasing = custom.order[i])		
		x = barplot(y[index], names.arg = '', 
			col=iif(y[index] > 0, col[1], col[2]), 
			main=i, 
			border = 'darkgray',las=2)
		grid(NA,NULL)
		abline(h=0, col='black')		
		if(y[1] > 0) 
			text(x, 0 * x, colnames(out)[index], adj=c(-0.1,1), srt=90, xpd = TRUE)
		else
			text(x, 0 * x, colnames(out)[index], adj=c(1.1,1), srt=90, xpd = TRUE)
		
		# add best worst labels
		mtext('worst', side = 1,line = 0, outer = F, adj = 1, font = 1, cex = 1)
		mtext('best', side = 1,line = 0, outer = F, adj = 0, font = 1, cex = 1)		
	}				
}





###############################################################################
# Summary snapshoot of strategy perfroamnce
###############################################################################
strategy.performance.snapshoot <- function(models, one.page = F) {
	#*****************************************************************
	# Create Report
	#****************************************************************** 					
	layout(1:2)
	plotbt(models, plotX = T, log = 'y', LeftMargin = 3)	    	
		mtext('Cumulative Performance', side = 2, line = 1)
		
	out = plotbt.strategy.sidebyside(models, return.table=T)

	if(one.page) return()
		
	performance.barchart.helper(out, 'Sharpe,Cagr,DVR,MaxDD', c(T,T,T,F))

	# Plot transition maps
	layout(1:len(models))
	for(m in names(models)) {
		plotbt.transition.map(models[[m]]$weight, name=m)
			legend('topright', legend = m, bty = 'n')
	}
}




###############################################################################
# Monthly End-of-the-Month (MEOM) by Quanting Dutchman
# http://quantingdutchman.wordpress.com/2010/06/30/strategy-2-monthly-end-of-the-month-meom/
###############################################################################
meom.strategy <- function
(
	tickers = spl('DIA,SPY'),
	dates = '1900::'
) 
{
	#*****************************************************************
	# Load historical data 
	#****************************************************************** 
	data = strategy.load.historical.data(tickers, dates, fill.gaps=T)
	
	prices = data$prices   
		n = ncol(prices)
		nperiods = nrow(prices)

	# find month ends
	month.ends = endpoints(prices, 'months')
		month.ends = month.ends[month.ends > 0]		
	month.ends2 = iif(month.ends + 2 > nperiods, nperiods, month.ends + 2)
	month.ends1 = iif(month.ends + 1 > nperiods, nperiods, month.ends + 1)


	models = list()

	#*****************************************************************
	# Code Strategies
	#****************************************************************** 	
	# Equal Weight
	data$weight[] = NA
		data$weight[] = ntop(prices, n)
	models$equal.weight = bt.run.share(data, clean.signal=F)
		
	# Strategy MEOM - Equal Weight
	data$weight[] = NA
		data$weight[month.ends,] = ntop(prices, n)[month.ends,]	
		data$weight[month.ends2,] = 0
	models$meom.equal.weight = bt.run.share(data, clean.signal=F)
	
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
	models$meom.top2.rank1 = bt.run.share(data, clean.signal=F, trade.summary=T)		
	
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
	models$meom.top2.rank2 = bt.run.share(data, clean.signal=F, trade.summary=T)		
		

	#*****************************************************************
	# Modify MEOM logic -  maybe sell in 1 day
	#****************************************************************** 			
	# Strategy MEOM - top 2, maybe sell in 1 day
	data$weight[] = NA
		data$weight[month.ends,] = ntop(position.score[month.ends,], 2)		
		data$weight[month.ends2,] = 0		

		# Close next day if Today's Close > Today's Open
		popen = bt.apply(data, Op)
		data$weight[month.ends1,] = iif((prices > popen)[month.ends1,], 0, NA)		
				
	models$meom.top2.rank2.hold12 = bt.run.share(data, clean.signal=F, trade.summary=T)		
		
	return(rev(models))
}

meom.strategy.test <- function() 
{
	models = meom.strategy(
				tickers = 'DIA,EEM,EFA,EWH,EWJ,EWT,EWZ,FXI,GLD,GSG,IEF,ILF,IWM,IYR,QQQ,SPY,VNQ,XLB,XLE,XLF,XLI,XLP,XLU,XLV,XLY,XLK',
				dates='1995::'
			)


png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')										
	plotbt.custom.report.part1(models)
dev.off()	

png(filename = 'plot2.png', width = 1200, height = 800, units = 'px', pointsize = 12, bg = 'white')	
	plotbt.custom.report.part2(models)
dev.off()	

png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')	
	plotbt.custom.report.part3(models$meom.top2.rank2, trade.summary=T)		
dev.off()	
			 
}




###############################################################################
# A Quantitative Approach to Tactical Asset Allocation by M. Faber (2006)
# http://www.mebanefaber.com/timing-model/
###############################################################################
timing.strategy <- function
(
	tickers = spl('DIA,SPY,SHY'),
	dates = '1900::',
	periodicity = 'months',
	ma.len = 200,
	cash = 'SHY'	
) 
{
	#*****************************************************************
	# Load historical data 
	#****************************************************************** 
	data = strategy.load.historical.data(tickers, dates)
	
	prices = data$prices   
		n = ncol(prices)
		nperiods = nrow(prices)

	# find period ends
	period.ends = endpoints(data$prices, periodicity)
		period.ends = period.ends[period.ends > 0]


	models = list()
	
	#*****************************************************************
	# Code Strategies
	#****************************************************************** 	
	# ignore cash when selecting funds
	position.score = prices
		position.score$SHY = NA
	
	# Equal Weight
	weight = ntop(position.score[period.ends,], n)	
	
	data$weight[] = NA
		data$weight[period.ends,] = weight
	models$equal.weight = bt.run.share(data, clean.signal=F)
	

	# BuyRule, price > 10 month SMA
	sma = bt.apply.matrix(prices, SMA, ma.len)
	buy.rule = prices > sma
		buy.rule = ifna(buy.rule, F)
	
	weight = ntop(position.score[period.ends,], n)
		# keep in cash the rest of the funds
		weight[!buy.rule[period.ends,]] = 0
		weight$SHY = 1 - rowSums(weight)
	
	data$weight[] = NA
		data$weight[period.ends,]  = weight
	models$timing = bt.run.share(data, clean.signal=F, trade.summary=T)
		
	return(rev(models))
}

timing.strategy.test <- function() 
{
	models = timing.strategy(
				tickers = 'VTI,EFA,IEF,ICF,DBC,SHY',
				dates='2002:08::'
			)


	plotbt.custom.report.part1(models)
	
	plotbt.custom.report.part2(models)
	
		
}




###############################################################################
# Rotational Trading Strategies : ETF Sector Strategy
# http://www.etfscreen.com/sectorstrategy.php
###############################################################################
rotation.strategy <- function
(
	tickers = spl('DIA,SPY,SHY'),
	dates = '1900::',
	periodicity = 'months',
	top.n = 2,
	keep.n = 6	
) 
{
	#*****************************************************************
	# Load historical data 
	#****************************************************************** 
	data = strategy.load.historical.data(tickers, dates)
	
	prices = data$prices   
		n = ncol(prices)
		nperiods = nrow(prices)

	# find period ends
	period.ends = endpoints(data$prices, periodicity)
		period.ends = period.ends[period.ends > 0]


	models = list()
	
	#*****************************************************************
	# Code Strategies
	#****************************************************************** 	
	# Equal Weight
	data$weight[] = NA
		data$weight[period.ends,] = ntop(prices[period.ends,], n)
	models$equal.weight = bt.run.share(data, clean.signal=F)
	
		
	# Rank on 6 month return
	position.score = prices / mlag(prices, 126)	
	
	# Select Top 2 funds
	data$weight[] = NA
		data$weight[period.ends,] = ntop(position.score[period.ends,], top.n)	
	models$top = bt.run.share(data, clean.signal=T, trade.summary=T)

	
	# Seletop Top 2 funds,  and Keep then till they are in 1:6 rank
	data$weight[] = NA
		data$weight[period.ends,] = ntop.keep(position.score[period.ends,], top.n, keep.n)	
	models$top.keep = bt.run.share(data, clean.signal=T, trade.summary=T)
		
	return(rev(models))	
	
}

	
	
rotation.strategy.test <- function() 
{
	models = rotation.strategy(
				tickers = 'XLY,XLP,XLE,XLF,XLV,XLI,XLB,XLK,XLU,IWB,IWD,IWF,IWM,IWN,IWO,IWP,IWR,IWS,IWV,IWW,IWZ',
				dates='1970::'
			)


	plotbt.custom.report.part1(models)
	
	plotbt.custom.report.part2(models)
	
}




###############################################################################
# Portfolio Construction and Optimization routines
###############################################################################
	#*****************************************************************
	# Weighting Schemes
	#*****************************************************************
	equal.weight.portfolio <- function
	(
		ia,				# input assumptions
		constraints		# constraints
	)
	{
		rep(1/ia$n, ia$n)
	}	

	# equal.risk.portfolio
	risk.parity.portfolio <- function
	(
		ia,				# input assumptions
		constraints		# constraints
	)
	{
		# allocate only among risk assets, ignore risk-less ones
		good.asset.index = (ia$risk > 0) & !is.na(ia$risk)
			good.asset.index[is.na(good.asset.index)] = F
						
		x = 1 / ia$risk[good.asset.index]
		
		# normalize weights to sum up to 1
		x = x / sum(x)
		
		# create allocation
		out = rep(0, ia$n)
			out[good.asset.index] = x
		return(out)
	}	
	
	min.var.portfolio <- function
	(
		ia,				# input assumptions
		constraints		# constraints
	)
	{
		# allocate only among risk assets, ignore risk-less ones
		good.asset.index = (ia$risk > 0) & !is.na(ia$risk)
			good.asset.index[is.na(good.asset.index)] = F
	
		# first try to solve QP with given Dmat
		Dmat = ia$cov[good.asset.index, good.asset.index]		
		sol = try(solve.QP(Dmat=Dmat, 
						dvec=rep(0, sum(good.asset.index)), 
						Amat=constraints$A[good.asset.index,], 
						bvec=constraints$b, 
						meq=constraints$meq), silent = TRUE)
		# if error, adjust Dmat to be positive definite
		if(inherits(sol, 'try-error'))
			sol = solve.QP(Dmat=make.positive.definite(Dmat, 0.000000001), 
						dvec=rep(0, sum(good.asset.index)), 
						Amat=constraints$A[good.asset.index,], 
						bvec=constraints$b, 
						meq=constraints$meq)
		
		x = sol$solution
		
		# create allocation
		out = rep(0, ia$n)
			out[good.asset.index] = x
		return(out)
	}
	
	
	
	
	
#*****************************************************************
# Portfolio Allocation Helper - distribute portfolio weights according to 
# the given weighting scheme (min.risk.fns)
#*****************************************************************
portfolio.allocation.helper <- function
(
	prices,					# prices
	periodicity = 'weeks',	#  rebalancing frequency
	period.ends = endpoints(prices, periodicity),	# rebalancing times
	
	lookback.len = 60,		# lookback to construct input assumptions each period
	prefix = '',
	
	universe = prices[period.ends,]>0,
	
	min.risk.fns = 'min.var.portfolio',	# portfolio construction functions
	custom.stats.fn = NULL,
	shrinkage.fn = NULL		# function to compute Covariance Shrinkage Estimator 
) 
{
	load.packages('quadprog,corpcor')
	
	#*****************************************************************
	# Setup
	#*****************************************************************
	period.ends = period.ends[period.ends > 0]
	
	universe[is.na(universe)] = F
		
	#*****************************************************************
	# Transform min.risk.fns in the named list
	#*****************************************************************
	if(is.character(min.risk.fns)) {
		min.risk.fns = spl(min.risk.fns)
		names(min.risk.fns) = min.risk.fns
		min.risk.fns = as.list(min.risk.fns)
	}

	for(i in 1:len(min.risk.fns)) {
		f = spl(names(min.risk.fns)[i], '_')	
		f.name = paste(prefix, gsub('\\.portfolio', '', f[1]),sep='')
		
		if(is.character(min.risk.fns[[i]])) {			
			if(len(f) == 1) {
				min.risk.fns[[ i ]] = match.fun(f[1])
			} else {
				f.name = paste(f.name, f[-1], sep='_')
				min.risk.fns[[ i ]] = match.fun(f[1])(f[-1])
			}
		}
		names(min.risk.fns)[i] = f.name			
	}
	
	if( !is.null(shrinkage.fn) )
		shrinkage.fn = match.fun(shrinkage.fn)
		
	#*****************************************************************
	# Code Strategies
	#****************************************************************** 		
	dates = index(prices)[period.ends]
	
	prices = coredata(prices)
	ret = prices / mlag(prices) - 1
	
	start.i = which(period.ends >= (lookback.len + 1))[1]

	weight = NA * prices[period.ends,]
		weight[] = 0
		
	weights = list()			
	for(f in names(min.risk.fns)) 
		weights[[ f ]] = weight				

	# custom stats logic		
	custom = list()
	if( !is.null(custom.stats.fn) ) {
		custom.stats.fn = match.fun(custom.stats.fn)
	
		dummy = matrix(NA, nr=nrow(weight), nc=len(weights))		
			colnames(dummy) = names(weights)
			dummy = make.xts(dummy, dates)	
			
   		temp = custom.stats.fn(1:ncol(ret), create.historical.ia(ret, 252))
   		
   		for(ci in names(temp)) {
   			temp1 = NA * dummy
   			if(len(temp[[ ci ]]) > 1) {
				temp1 = list()			
				for(w in names(weights)) 
					temp1[[w]] = NA * weights[[w]]   			   				
   			} 
   			custom[[ ci ]] = temp1
   		}
   	} 		
		
						
	# construct portfolios			
	for( j in start.i:len(period.ends) ) {
		i = period.ends[j]
		
		# histtory to construct input assumptions
		hist = ret[ (i- lookback.len +1):i, ]
		
		# require all assets to have full price history
		include.index = count(hist)== lookback.len      

		index = universe[j,] & include.index						
		n = sum(index)
		
		if(n > 0) {					
			hist = hist[ , index]
	
			# 0 <= x.i <= 1
			constraints = new.constraints(n, lb = 0, ub = 1)
				constraints = add.constraints(diag(n), type='>=', b=0, constraints)
				constraints = add.constraints(diag(n), type='<=', b=1, constraints)

			# SUM x.i = 1
			constraints = add.constraints(rep(1, n), 1, type = '=', constraints)		
						
			# create historical input assumptions
			#ia = new.env()
			ia = list()
				ia$index = index
				ia$n = n
				ia$hist.returns = hist
				ia$expected.return = apply(hist, 2, mean)
				ia$risk = apply(hist, 2, sd)		
				ia$correlation = cor(hist, use='complete.obs', method='pearson')
				ia$cov = ia$correlation * (ia$risk %*% t(ia$risk))
				
				
				if( !is.null(shrinkage.fn) )
					ia$cov = shrinkage.fn(hist)
					

				
				# adjust correlation and covariance matrices to be positive defined
				temp = try(make.positive.definite(ia$cov, 0.000000001), TRUE)	
					if(!inherits(temp, 'try-error')) ia$cov = temp				
				temp = try(make.positive.definite(ia$correlation, 0.000000001), TRUE)	
					if(!inherits(temp, 'try-error')) ia$correlation = temp							
				
			# find optimal portfolios under different risk measures
			for(f in names(min.risk.fns)) {
				constraints$x0 = weights[[ f ]][(j-1),index]			
				weights[[ f ]][j,index] = min.risk.fns[[f]](ia, constraints)
			}
			
			
			# custom stats logic		
			if( !is.null(custom.stats.fn) ) {
				for(w in names(weights)) {
					x = as.vector(weights[[ w ]][j, index])
					temp = custom.stats.fn(x, ia)

			   		for(ci in names(temp)) {
			   			if(len(temp[[ ci ]]) > 1)
			   				custom[[ ci ]][[ w ]][j, index] = temp[[ ci ]]
			   			else
			   				custom[[ ci ]][j, w] = temp[[ ci ]]
			   		}
				}
		   	} 		
			
		}
			
		if( j %% 10 == 0) cat(j, '\n')		
	}

	
	return(c(list(weights = weights, period.ends = period.ends,
		periodicity = periodicity, lookback.len = lookback.len), custom))
}



# compute portfolio allocation additional stats
portfolio.allocation.custom.stats <- function(x,ia) {
	return(list(
		# vectors
		risk.contributions = portfolio.risk.contribution(x, ia),
		
		# numbers
		diversification.ratio = sqrt(x %*% ia$cov %*% x) / (sqrt(diag(ia$cov)) %*% x)
	))
}


# Create strategies based on portfolio weights
create.strategies <- function
(
	obj,	# portfolio.allocation object: list(weights = weights, period.ends = period.ends)
	data,	# historical prices
	...		
) 
{
	#*****************************************************************
	# Create strategies
	#****************************************************************** 		
	models = list()
	for(i in names(obj$weights)) {
		data$weight[] = NA
			data$weight[obj$period.ends,] = obj$weights[[i]]	
		models[[i]] = bt.run.share(data, clean.signal = F, ...)
		
#		models[[i]]$risk.contribution = obj$risk.contributions[[i]]
		models[[i]]$period.weight = obj$weights[[i]]
	}
	obj$models = models
	return(obj)			
}
	

	

#*****************************************************************
# Asset Allocation Strategy
#*****************************************************************
asset.allocation.strategy.test <- function() 
{
	tickers = 'SPY,QQQ,EEM,IWM,EFA,TLT,IYR,GLD'
	dates='2000::'
	
	#*****************************************************************
	# Load historical data 
	#****************************************************************** 
	data = strategy.load.historical.data(tickers, dates)
	
	obj = portfolio.allocation.helper(data$prices,
		min.risk.fns = 'equal.weight.portfolio,risk.parity.portfolio,min.var.portfolio',
		custom.stats.fn = 'portfolio.allocation.custom.stats'
	) 
	
	models = create.strategies(obj, data)$models
	
	#*****************************************************************
	# Create Report
	#****************************************************************** 					
	plotbt.custom.report.part1(models)
	
	plotbt.custom.report.part2(models)
	
	strategy.performance.snapshoot(models)
	
}
 
