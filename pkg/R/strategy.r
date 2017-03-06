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
#' @export 
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
			# getSymbols removes ^ from Yahoo indexes
			ticker0 = gsub('\\^', '', tickers[i])
			
			temp = try(getSymbols(tickers[i], src = 'yahoo', from = '1900-01-01', env = data, auto.assign = T), TRUE)	
			if(inherits(temp, 'try-error'))
				cat(i, 'out of', len(tickers), 'Error Reading', tickers[i], '\n', sep='\t')
			else if(is.null(data[[ tickers[i] ]]))
				if( is.null(data[[ ticker0 ]]) )
					cat(i, 'out of', len(tickers), 'Error Reading', tickers[i], '\n', sep='\t')
				else
					cat(i, 'out of', len(tickers), 'Reading', ticker0, format(range(index(data[[ ticker0 ]])), '%d-%b-%Y'), '\n', sep='\t')
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
#' @export 
###############################################################################
performance.barchart.helper <- function(out, 
	names = rownames(out),
	custom.order=rep(T,len(spl(names))), 
	nplots.page = len(spl(names)), 
	nc.plot = 2, 
	sort.performance = T,
	custom.col = NULL
	
) 
{
	# Bar chart
	layout(mat=matrix(1:(nplots.page + nplots.page %% nc.plot), nc=nc.plot, byrow=FALSE))
	par(mar=c(4, 3, 2, 2))
	#col = spl('lightgray,red')
	col = spl('ivory2,red')
	
	names = spl(names)
	names(custom.order) = names
	
	for(i in names) {
		y = as.double(out[i,])
		index = iif(sort.performance, order(y, decreasing = custom.order[i]), 1:len(y))
		
		cols = iif(y[index] > 0, col[1], col[2])
			names(cols) = colnames(out)[index]
		if(!is.null(custom.col))
			cols[names(custom.col)] = custom.col
		
		x = barplot(y[index], names.arg = '', 
			col=cols, 
			main=i, 
			border = 'darkgray',las=2)
		grid(NA,NULL)
		abline(h=0, col='black')		
		if(y[1] > 0) 
			text(x, 0 * x, colnames(out)[index], adj=c(-0.1,1), srt=90, xpd = TRUE)
		else
			text(x, 0 * x, colnames(out)[index], adj=c(1.1,1), srt=90, xpd = TRUE)
		
		# add best worst labels
		if(sort.performance) {			
			mtext('worst', side = 1,line = 0, outer = F, adj = 1, font = 1, cex = 1)
			mtext('best', side = 1,line = 0, outer = F, adj = 0, font = 1, cex = 1)		
		}
	}				
}
	

###############################################################################
# helper function to create barplot with labels
#' @export 
###############################################################################
barplot.with.labels <- function(data, main, plotX = TRUE, label=c('level','name','both')) {
	par(mar=c( iif(plotX, 6, 2), 4, 2, 2))
	x = barplot(100 * data, main = main, las = 2, names.arg = iif(plotX, names(data), ''))
	
	if(label[1] == 'level') text(x, 100 * data, round(100 * data,1), adj=c(0.5,1), xpd = TRUE)
	if(label[1] == 'name') text(x, 0 * data, names(data), adj=c(-0.1,1), srt=90, xpd = TRUE)	
	if(label[1] == 'both') 
		text(x, 0 * data, paste(round(100 * data), '% ', names(data), sep=''), adj=c(-0.1,1), srt=90, xpd = TRUE)
}





###############################################################################
# Summary snapshoot of strategy perfroamnce
#' @export 
###############################################################################
strategy.performance.snapshoot <- function(models, one.page = F, title = NULL, data = NULL,
	control = list(main = T, comparison = T, transition = T, monthly = T),
	sort.performance = T
) {
	for(n in spl('main,comparison,transition,monthly'))
		if(is.null(control[[n]])) control[[n]] = F

	#*****************************************************************
	# Create Report
	#****************************************************************** 					
	out = NULL
if(control$main) {	
	layout(1:2)
	plotbt(models, plotX = T, log = 'y', LeftMargin = 3, main = title)	    	
		mtext('Cumulative Performance', side = 2, line = 1)
		
	out = plotbt.strategy.sidebyside(models, return.table=T)
}	
	if(one.page) return()
	
if(control$comparison) {
	if(is.null(out))
		out = plotbt.strategy.sidebyside(models, return.table=T, make.plot = F)
	# Portfolio Turnover
	if(!is.null(data)) {
		y = 100 * sapply(models, compute.turnover, data)
			out = rbind(y, out)
			rownames(out)[1] = 'Turnover'		
		performance.barchart.helper(out, 'Sharpe,Cagr,DVR,MaxDD,Volatility,Turnover', c(T,T,T,T,F,F), sort.performance = sort.performance)
	} else		
		performance.barchart.helper(out, 'Sharpe,Cagr,DVR,MaxDD', c(T,T,T,T), sort.performance = sort.performance)
}
		
if(control$transition) {	
	# Plot transition maps
	layout(1:min(4,len(models)))
	for(m in names(models)) {
		plotbt.transition.map(models[[m]]$weight, name=m)
			legend('topright', legend = m, bty = 'n')
	}
}	
	
if(control$monthly) {	
	# Plot monthly retunrs tables
	layout(1:min(4,len(models)))
	for(n in names(models))
		plotbt.monthly.table(models[[n]]$equity, smain=n)			
}		
	
}	





###############################################################################
# Monthly End-of-the-Month (MEOM) by Quanting Dutchman
# http://quantingdutchman.wordpress.com/2010/06/30/strategy-2-monthly-end-of-the-month-meom/
#' @export 
##############################################################################
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
#' @export 
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
#' @export 
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
# Create historical input assumptions
#' @export 
###############################################################################
# index - column index in the prices matrix
# nperiod - row index in the prices matrix
#' @export
create.ia <- function(hist.returns, index=1:ncol(hist.returns), nperiod=nrow(hist.returns))
{	
	# setup input assumptions
	ia = list()	
		ia$hist.returns = hist.returns
				
		ia$nperiod = nperiod		
		ia$index = index
		
		ia$n = ncol(ia$hist.returns)
		ia$symbols = colnames(ia$hist.returns)
		
		ia$risk = apply(ia$hist.returns, 2, sd, na.rm = T)
		ia$correlation = cor(ia$hist.returns, use='complete.obs',method='pearson')
		ia$cov = ia$correlation * (ia$risk %*% t(ia$risk))
		
	ia$expected.return = apply(ia$hist.returns, 2, mean, na.rm = T)
							
	return(ia)
}

#' @export
update.ia <- function(ia, name, cov.shrink)
{
	if(name != 'sample') {
		ia$cov = cov.shrink
			s0 = 1 / sqrt(diag(ia$cov))
		ia$correlation = ia$cov * (s0 %*% t(s0))
	}
	ia
}

# Extract subset of ia given index
#' @export
create.ia.subset <- function(ia.base, index=1:ia.base$n)
{	
	# setup input assumptions
	ia = list()	
		ia$hist.returns = ia.base$hist.returns[,index,drop=F]
				
		ia$nperiod = ia.base$nperiod
		ia$index = ia.base$index[index]
		
		ia$n = ncol(ia$hist.returns)
		ia$symbols = colnames(ia$hist.returns)
		
		ia$risk = ia.base$risk[index]
		ia$correlation = ia.base$correlation[index,index,drop=F]
		ia$cov = ia.base$cov[index,index,drop=F]
		
	ia$expected.return = ia.base$expected.return[index]
							
	return(ia)
}

# Change periodicity used for input assumptions
#' @export
create.ia.period <- function
(
	prices, 
	periodicity = 'weeks',
	period.ends = endpoints(prices, periodicity)
)	
{
	prices = prices[period.ends,,drop=F]
	ret = coredata(prices / mlag(prices) - 1)
	
	function(hist.returns, index, nperiod)
	{
		i = nperiod
		create.ia(ret[which(
						period.ends <= i & 
						period.ends >= (i - nrow(hist.returns) + 1)
					), index, drop=F], 
					index,
					nperiod)
	}	
}


# Create historical input assumptions
#' @export 	
create.historical.ia <- function
(
	hist.returns, 
	annual.factor
) {	
	# setup input assumptions
	ia = create.ia(hist.returns)
	
	ia$annual.factor = annual.factor

	ia$arithmetic.return = apply(hist.returns, 2, mean, na.rm = T)	
	ia$geometric.return = apply(hist.returns, 2, function(x) prod(1+x)^(1/len(x))-1 )
	
	# convert ia to annual		
	#ia$arithmetic.return = ia$annual.factor * ia$arithmetic.return
	ia$arithmetic.return = (1 + ia$arithmetic.return)^ia$annual.factor - 1		
	ia$geometric.return = (1 + ia$geometric.return)^ia$annual.factor - 1
	
	ia$risk = sqrt(ia$annual.factor) * ia$risk

	# compute covariance matrix
	ia$risk = iif(ia$risk == 0, 0.000001, ia$risk)
	ia$cov = ia$correlation * (ia$risk %*% t(ia$risk))		
	
	ia$expected.return = ia$arithmetic.return
	
	ia
}


###############################################################################
# The Averaging techniques are used to avoid over-fitting any particular frequency
# created by pierre.c.chretien
###############################################################################
#' @export 	
ia.build.hist <- function(hist.returns, lookbacks, n.lag)
{
	nperiods = nrow(hist.returns)
		
	temp = c()
	for (n.lookback in lookbacks)
			temp = rbind(temp, hist.returns[(nperiods - n.lookback - n.lag + 1):(nperiods - n.lag), , drop=F])
	return(temp)
}

#' @export 	
momentum.averaged <- function(prices, 
	lookbacks = c(20,60,120,250) ,	# length of momentum look back
	n.lag = 3
) {
	momentum = 0 * prices
	for (n.lookback in lookbacks) {
		part.mom = mlag(prices, n.lag) / mlag(prices, n.lookback + n.lag) - 1
		momentum = momentum + 252 / n.lookback * part.mom
	}
	momentum / len(lookbacks)
}
	
#' @export	
create.ia.averaged <- function(lookbacks, n.lag)
{
	lookbacks = lookbacks
	n.lag = n.lag

	function(hist.returns, index, nperiod)
	{
		temp = ia.build.hist(hist.returns, lookbacks, n.lag)
		create.ia(temp, index, nperiod)
	}	
}





###############################################################################
# Portfolio Construction and Optimization routines
###############################################################################
	#*****************************************************************
	# Weighting Schemes
	#*****************************************************************
	
# static allocation
#' @export 
static.weight.portfolio <- function(static.allocation)
{
	static.allocation = static.allocation
	function
	(
		ia,				# input assumptions
		constraints		# constraints
	)
	{
		return(static.allocation[ia$index])	
	}	
}
	
	#' @export 
	equal.weight.portfolio <- function
	(
		ia,				# input assumptions
		constraints		# constraints
	)
	{
		rep(1/ia$n, ia$n)
	}	
	

	# allocate only among assets with risk > 0, ignore cash (i.e. risk = 0)
	#' @export 
	get.risky.asset.index <- function(ia) {
		if(is.null(ia$risk)) ia$risk = sqrt(diag(ia$cov))
		(ia$risk > 0) & !is.na(ia$risk)
	}
	#' @export 
	set.risky.asset <- function(x, risk.index) {
		out = rep(0, len(risk.index))
			out[risk.index] = x
		return(out)
	}
	
	
	# equal.risk.portfolio
	#' @export 	
	risk.parity.portfolio.basic <- function
	(
		ia,				# input assumptions
		constraints		# constraints
	)
	{
		if(is.null(ia$risk)) ia$risk = sqrt(diag(ia$cov))
		risk.index = get.risky.asset.index(ia)
				
		# re-scale weights to penalize for risk		
		x = 1 / ia$risk[risk.index]
		
		# normalize weights to sum up to 1
		set.risky.asset(x / sum(x), risk.index)
	}

# RP = risk.parity.portfolio()
# RP.CVAR = risk.parity.portfolio(function(ia) apply(ia$hist.returns, 2, compute.cvar))
# RP.MD = risk.parity.portfolio(function(ia) apply(apply(1+ia$hist.returns, 2, cumprod), 2, compute.max.drawdown)) 
# RP.CDAR = risk.parity.portfolio(function(ia) apply(apply(1+ia$hist.returns, 2, cumprod), 2, compute.cdar))
#	
# risk.parity allocation with custom risk functions
#' @export 
risk.parity.portfolio <- function(
	risk.fn = function(ia) ia$risk
)
{
	algo.map = list(
		'cvar' = function(ia) -apply(ia$hist.returns, 2, compute.cvar),
		'md' = function(ia) -apply(apply(1+ia$hist.returns, 2, cumprod), 2, compute.max.drawdown),
		'cdar' = function(ia) -apply(apply(1+ia$hist.returns, 2, cumprod), 2, compute.cdar)
	)

	fn = try( match.fun(risk.fn) , silent = TRUE)
	if(class(fn)[1] == 'try-error' && is.character(risk.fn) && any(names(algo.map) == tolower(risk.fn)))
		fn = algo.map[[ tolower(risk.fn) ]]
	if(class(fn)[1] == 'try-error') stop(paste('risk.parity.portfolio', fn))
	
	function
	(
		ia,				# input assumptions
		constraints		# constraints
	)
	{
		if(is.null(ia$risk)) ia$risk = sqrt(diag(ia$cov))
		risk.index = get.risky.asset.index(ia)
				
		# re-scale weights to penalize for risk		
		x = 1 / fn(ia)[risk.index]
			# if an asset has a negative fn this asset’s weight will be 0; 
			x[x < 0] = 0
		
		# normalize weights to sum up to 1
		ifna(set.risky.asset(x / sum(x), risk.index), 0)
	}	
}

			
	
	#' @export 	
	min.var.portfolio <- function
	(
		ia,				# input assumptions
		constraints,	# constraints
		cov.matrix = ia$cov,
		dvec = rep(0, ia$n)
	)
	{
		risk.index = get.risky.asset.index(ia)
	
		# first try to solve QP with given Dmat
		Dmat = cov.matrix[risk.index, risk.index]		
		sol = try(solve.QP(Dmat=Dmat, 
						dvec=dvec[risk.index], 
						Amat=constraints$A[risk.index,,drop=F], 
						bvec=constraints$b, 
						meq=constraints$meq), silent = TRUE)
						
		# if error, adjust Dmat to be positive definite
		if(inherits(sol, 'try-error'))
			sol = try(solve.QP(Dmat=make.positive.definite(Dmat, 0.000000001), 
						dvec=dvec[risk.index], 
						Amat=constraints$A[risk.index,,drop=F], 
						bvec=constraints$b, 
						meq=constraints$meq), silent = TRUE)
		if(inherits(sol, 'try-error')) {
			gia <<- ia
			stop(sol)
		}
						
		set.risky.asset(sol$solution, risk.index)
	}	

	
	# Toward Maximum Diversification by Y. Choueifaty, Y. Coignard
	# The Journal of Portfolio Management, Fall 2008, Vol. 35, No. 1: pp. 40-51
	#' @export 	
	max.div.portfolio <- function
	(
		ia,				# input assumptions
		constraints		# constraints
	)
	{
		risk.index = get.risky.asset.index(ia)
		
		x = min.var.portfolio(ia, constraints, ia$correlation)
				
		# re-scale weights to penalize for risk
		x = x[risk.index] / ia$risk[risk.index]
		
		# normalize weights to sum up to 1
		set.risky.asset(x / sum(x), risk.index)
	}	
	
	#' @export 	
	equal.risk.contribution.portfolio <- function
	(
		ia,                     # input assumptions
	    constraints             # constraints
	)
	{
		risk.index = get.risky.asset.index(ia)
			
		cov = ia$cov[risk.index, risk.index]
	       
	    # obj
	    fn <- function(x){
	    	# sum(x) = 1
	        if (sum(x) == 0) x = x + 1e-2
	        x  = x / sum(x)
	       
	        risk.contribution = (x * (cov %*% x))
	        var(as.double(risk.contribution))                
		}
	         
	        
	              				
		x0 = 1/sqrt(diag(cov))
			x0 = x0 / sum(x0)
				
		if(!is.null(constraints$x0))
			if(all(!is.na(constraints$x0)))
				if( sum(constraints$x0) == 1 )
					if( fn(x0) > fn(constraints$x0[risk.index]) )						
						x0 = constraints$x0[risk.index]
	
		# http://www.ucl.ac.uk/~uctpjyy/nloptr.html
		load.packages('nloptr')
			                       
	    x = nloptr( x0=x0,eval_f=fn,lb = constraints$lb[risk.index],ub = constraints$ub[risk.index],
	    	opts = list('algorithm'='NLOPT_LN_BOBYQA','xtol_rel'=1.0e-10))
	
		# normalize weights to sum up to 1
		set.risky.asset(x$solution / sum(x$solution), risk.index)			
	}
	

# Based on CIR factory by mock-quant
#' @export 	
ef.portfolio <- function(percent = 0.5) 
{
	percent = as.double(percent[1])
	if(percent > 1) percent = percent / 100
	
	function
	(
		ia,			# input assumptions
		constraints	# constraints
	)
	{
		# find extreme solutions
		# need to set ia$expected.return for max.return.portfolio
		max.w = max.return.portfolio(ia, constraints)
		min.w = min.var.portfolio(ia, constraints)	
		
		# compute portfolio returns
		max.r = sum(max.w * ia$expected.return)
		min.r = sum(min.w * ia$expected.return)
		
		# determine target return
		target = (max.r - min.r) * percent + min.r
		
		constraints = add.constraints(ia$expected.return, 
							type='>=', b=target, constraints)
							
		return(min.var.portfolio(ia, constraints))	
	}		
}	


#*****************************************************************	
# Tracking Error minimization:
# http://www.mathworks.com/matlabcentral/answers/59587-how-to-use-the-objective-function-minimize-te-in-quadprog
# The objective is to minimize (w-w0)'myCov(w-w0)
# f = -w0'*myCov
# Expanding
# w.'*myCov*w/2 - w0'*myCov*w
#
# i.e. 
# (x - w0)'Cov(x - w0) = x'Cov(x - w0) - w0'Cov(x - w0)
# = x'Cov x - x'Cov w0 - w0'Cov x + w0'Cov w0
# = x'Cov x - 2 x'Cov w0 + w0'Cov w0, (w0'Cov w0 is constant)
# min 1/2 x'Cov x - x'Cov w0
#
# Minimizing Tracking Error While Restricting the Number of Assets 
# https://cs.uwaterloo.ca/~yuying/papers/indexJay.pdf
#
# A Hybrid Genetic Algorithm for Passive Management by Dirk Eddelb
# dirk.eddelbuettel.com/papers/cef96.ps
# http://www.tbm.tudelft.nl/fileadmin/Faculteit/TBM/Over_de_Faculteit/Afdelingen/Afdeling_Infrastructure_Systems_and_Services/Sectie_Informatie_en_Communicatie_Technologie/medewerkers/jan_van_den_berg/courses/Security_en_Techniek/doc/ACFM-met-Roland.pdf
#*****************************************************************
# solve.QP: min(x'Dx - 2*dvec*x) 
#' @export 
min.te.portfolio.test <- function() 
{
	# minimum Tracking Error portfolios
	min.te.portfolio <- function(ia, constraints, index.weight)	
		min.var.portfolio(ia, constraints, dvec = index.weight %*% ia$cov)
		
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load.packages('quantmod')
		
	# tickers = dow.jones.components()
	tickers = spl('SPY,TLT,XLP')
		
	data <- new.env()
	getSymbols.extra(tickers, src = 'yahoo', from = '1980-01-01', env = data, set.symbolnames = T, auto.assign = T)
		for(i in data$symbolnames) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
	bt.prep(data, align='keep.all', fill.gaps = T)

	#*****************************************************************
	# Setup
	#*****************************************************************
	prices = data$prices
		n = ncol(prices)
		nperiods = nrow(prices)
		
	ret = prices / mlag(prices) - 1
	#*****************************************************************
	# Create input assumptions
	#****************************************************************** 				
	lookback.len = 120
	
	hist = ret[(nperiods - lookback.len) : nperiods, , drop=F]
	ia = create.ia(hist, nperiod=nperiods)

	index.weight = coredata(last(prices))
		index.weight = index.weight / sum(index.weight)

	# packages for quadprog
	load.packages('quadprog,corpcor,kernlab')
	
	# basic constraints			
	constraints = create.basic.constraints(n, 0, 1, 1)
	x = min.te.portfolio(ia, constraints, index.weight)	
		x - index.weight
	252 * portfolio.return(x, ia)
	# x is same as index.weight
	
	# reduce max allocation 40%
	constraints = create.basic.constraints(n, 0, 0.4, 1)
	x = min.te.portfolio(ia, constraints, index.weight)	
		x - index.weight
	252 * portfolio.return(x, ia)
		
	# enforce min return
	constraints = create.basic.constraints(n, 0, 1, 1)
	constraints = add.constraints(ia$expected.return, type = '>=', b=0.95 * max(ia$expected.return), constraints)		
	x = min.te.portfolio(ia, constraints, index.weight)	
		x - index.weight
	252 * portfolio.return(x, ia)
}
	



#*****************************************************************	
# generate given number of historical scenarios
# and average portfolio allocation algo weights 
# across all scenarios
#' @export 
#*****************************************************************
random.hist <- function(
	portfolio.fn = min.var.portfolio, # portfolio allocation algo
    nsamples = 100,         # Number of Samples to draw
    sample.len = 60         # Length of each sample	
)
{
	fn = try( match.fun(portfolio.fn) , silent = TRUE)
	if(class(fn)[1] == 'try-error') stop(paste('random.hist', fn))
	
	nsamples = nsamples
	sample.len = sample.len
	
	function
	(
		ia,				# input assumptions
		constraints		# constraints
	)
	{
	    # load / check required packages
    	load.packages('MASS')

    	# first run base
    	weight = fn(ia, constraints)
    	
	    #Start Monte Carlo simulation of asset returns
    	ia.original = ia
    		
	    for(i in 1:nsamples) {
	        ia$hist.returns = mvrnorm(sample.len, ia.original$expected.return, Sigma = ia.original$cov)
	 
	        ia$expected.return = apply(ia$hist.returns, 2, mean)
	        ia$risk = apply(ia$hist.returns, 2, sd)
	        ia$correlation = cor(ia$hist.returns, use = 'complete.obs', method = 'pearson')
	        ia$cov = ia$correlation * (ia$risk %*% t(ia$risk))
	 
	        weight = weight + fn(ia, constraints)
	    }
	 
	    weight / (nsamples + 1)
	}	
}


#*****************************************************************	
# generate given number of historical scenarios
# and average portfolio allocation algo weights 
# across all scenarios
#' @export 
#*****************************************************************
random.hist.weight = function(
	fn,
	data,
	period.ends,
	nsamples = 100, 
	sample.len = 120,
	silent = F	
)
{
	prices = data$prices * data$universe
	obj = fn(prices, data)
		
	scenarios = asset.paths.at.period.ends(data$prices, period.ends, nsamples, lookback.len=sample.len)
	#plota.matplot(scale.one(make.xts(scenarios[,,j],data$dates)),main='Asset Perfromance')
	
	for(j in 1:nsamples) {
		prices = make.xts(scenarios[,,j],data$dates)
			colnames(prices) = colnames(data$prices)
		prices = prices * data$universe
		temp = fn(prices, data)
		for(i in names(obj$weights))
			obj$weights[[i]] = obj$weights[[i]] + temp$weights[[i]]
			
		if (j %% 5 == 0)
			if (!silent)
				cat(j, 'done', '\n')
	}

	for(i in names(obj$weights))
		obj$weights[[i]] = obj$weights[[i]] / (nsamples + 1)
		
	obj
}
	
	
		
###############################################################################    
# maximum Sharpe ratio or tangency  portfolio
# http://faculty.washington.edu/ezivot/econ424/portfolio_noshorts.r
# http://stackoverflow.com/questions/10526243/quadprog-optimization
# http://comisef.wikidot.com/tutorial:tangencyportfolio
#
# It work with solve.QP only for constraints that are homogeneous of degree 0
# i.e. if we multiply w by a number, the constraint is unchanged
#
# Dmat <- 2*cov.mat
# dvec <- rep.int(0, N)
# er.excess <- er - risk.free
# Amat <- cbind(er.excess, diag(1,N))
# bvec <- c(1, rep(0,N))
# result <- solve.QP(Dmat=Dmat,dvec=dvec,Amat=Amat,bvec=bvec,meq=1)
# w.t <- round(result$solution/sum(result$solution), 6)
#
# R Tools for Portfolio Optimization by Guy Yollin - R/Finance
# http://www.rinfinance.com/RinFinance2009/presentations/yollin_slides.pdf
# In more general case we can use the non linear solver
# V <- cov(mData)
# library(Rsolnp)
# r <- solnp(
#  rep(1/length(mu), length(mu)),
#  function(w) - t(w) %*% mu2 / sqrt( t(w) %*% V %*% w ),
#  eqfun = function(w) sum(w),
#  eqB   = 0,
#  LB = rep(-1, length(mu))
# )
#
# Interesting info
# http://r.789695.n4.nabble.com/The-best-solver-for-non-smooth-functions-td4636934.html
###############################################################################	
	# only works for constraints that are homogeneous of degree 0
	# i.e. if we multiply solution weight by a number, the constraint is unchanged
	#' @export 
	max.sharpe.portfolio.helper <- function
	(
		ia,				# input assumptions
		const = spl('long-only,long-short,market-neutral'),
		const.sum = 1,
		rf = 0
	)
	{
		const = const[1]
		n = ia$n
		constraints = new.constraints(n)
		if( const == 'long-only' )
			constraints = add.constraints(diag(n), type='>=', b=0, constraints)
			
		# SUM x.i * expected.return = 1
		excess.return = ia$expected.return - rf
		if( all(excess.return <= 0) )
			constraints = add.constraints(excess.return, -1 , type = '=', constraints)		
		else
			constraints = add.constraints(excess.return, 1 , type = '=', constraints)		
		
		if( const == 'market-neutral' )
			constraints = add.constraints(rep(1,n), 0 , type = '=', constraints)		
			
		weight = min.var.portfolio(ia,constraints)	
		
		if( const == 'market-neutral' )
			return(2*const.sum * weight / sum(abs(weight)))
		else
			return(const.sum * weight / sum(weight))
	}	
	
	#' @export 	
	max.sharpe.portfolio <- function
	(
		const = spl('long-only,long-short,market-neutral'),
		const.sum = 1
	)
	{
		const = const[1]
		const.sum = const.sum
		function(ia, constraints) { max.sharpe.portfolio.helper(ia, const, const.sum) }
	}
	
	
max.sharpe.portfolio.test <- function() 
{
	#*****************************************************************
	# Create Efficient Frontier
	#****************************************************************** 	
	# create sample historical input assumptions
	ia = aa.test.create.ia()
	
	# create long-only, fully invested efficient frontier
	n = ia$n		

	constraints = create.basic.constraints(n, 0, 1, 1)
	
	# create efficient frontier
	ef = portopt(ia, constraints, 50, 'Efficient Frontier') 
	
	#*****************************************************************
	# Create Plot
	#****************************************************************** 	
png(filename = 'plot1.png', width = 500, height = 500, units = 'px', pointsize = 12, bg = 'white')
	
	# plot efficient frontier
	plot.ef(ia, list(ef), transition.map=F)	 
	
	# find maximum sharpe portfolio
	max(portfolio.return(ef$weight,ia) /  portfolio.risk(ef$weight,ia))
	
	# plot minimum variance portfolio
	weight = min.var.portfolio(ia,constraints)	
	points(100 * portfolio.risk(weight,ia), 100 * portfolio.return(weight,ia), pch=15, col='red')
	portfolio.return(weight,ia) /  portfolio.risk(weight,ia)
		
	# plot maximum Sharpe or tangency portfolio
	weight = max.sharpe.portfolio()(ia,constraints)	
	points(100 * portfolio.risk(weight,ia), 100 * portfolio.return(weight,ia), pch=15, col='orange')
	portfolio.return(weight,ia) /  portfolio.risk(weight,ia)
		
	plota.legend('Minimum Variance,Maximum Sharpe','red,orange', x='topright')
	
dev.off()	
	
	#*****************************************************************
	# Examples of Maximum Sharpe or Tangency portfolios construction
	#****************************************************************** 	
	weight = max.sharpe.portfolio('long-only')(ia,constraints)	
		round(weight,2)
		round(c(sum(weight[weight<0]), sum(weight[weight>0])),2)
		
	weight = max.sharpe.portfolio('long-short')(ia,constraints)			
		round(weight,2)
		round(c(sum(weight[weight<0]), sum(weight[weight>0])),2)

	weight = max.sharpe.portfolio('long-short', -1)(ia,constraints)			
		round(weight,2)
		round(c(sum(weight[weight<0]), sum(weight[weight>0])),2)
		
				
	weight = max.sharpe.portfolio('market-neutral')(ia,constraints)			
		round(weight,2)
		round(c(sum(weight[weight<0]), sum(weight[weight>0])),2)
		
}


# When all asset Sharpe Ratios are equal, this MDP portfolio will 
# have the highest possible Sharpe Ratio. 
# https://www.putnam.com/literature/pdf/whitepaper_parity_strategies.pdf
max.div.portfolio.test <- function() 
{

	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load.packages('quantmod')
		
	tickers = spl('SPY,QQQ,EEM,IWM,EFA,TLT,IYR,GLD')
	
	data = env()
	getSymbols.extra(tickers, src = 'yahoo', from = '1980-01-01', env = data, set.symbolnames = T, auto.assign = T)
		for(i in data$symbolnames) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
	bt.prep(data, align='keep.all', fill.gaps = T)

	#*****************************************************************
	# Setup
	#*****************************************************************
	prices = data$prices
	
	period.ends = date.ends(prices, 'month')

	hist.returns = prices[period.ends] / mlag(prices[period.ends]) - 1
	
	# Create historical input assumptions
	ia = create.historical.ia(hist.returns, 12)
	
	constraints = create.basic.constraints(ia$n, 0, 1, 1)
	
	load.packages('quadprog,corpcor,lpSolve,kernlab')

	sol1 = max.div.portfolio(ia, constraints)
	
	#*****************************************************************
	# Replication
	#*****************************************************************	
	# Properties of the Most Diversified Portfolio by Yves Choueifaty
	# http://papers.ssrn.com/sol3/papers.cfm?abstract_id=1895459
	# p21 min wEw s.t. w[i] > 0 and Sum(w[i] * sigma[i]) = 1
	# normalize weights to sum up to 1	
	max.div.portfolio2 <- function
	(
		ia,				# input assumptions
		constraints		# constraints
	)
	{
		risk.index = get.risky.asset.index(ia)

		constraints = new.constraints(ia$n, lb = 0)
 		constraints = add.constraints(diag(ia$n), type = ">=", b = 0, constraints)
			constraints = add.constraints(ia$risk, type = '=', b = 1, constraints)	
		x = min.var.portfolio(ia, constraints)
		
		# normalize weights to sum up to 1
		set.risky.asset(x / sum(x), risk.index)
	}
	
	sol2 = max.div.portfolio2(ia, constraints)
	
	# weights
	round(100*cbind(sol1,sol2),2)
		
	# risk contributions
	round(100*t(portfolio.risk.contribution(rbind(sol1,sol2), ia)),2)
			
	#*****************************************************************
	# Example from paper: Base
	#*****************************************************************	
	ia = list(
		n = 2,
		symbols = spl('A,B'),
		risk = c(20, 10) / 100,
		correlation = matrix(c(1, 0.5, 0.5, 1), 2, 2),
		expected.return = c(1,1)		
	)
	ia$cov = ia$correlation * (ia$risk %*% t(ia$risk))
	ia.base = ia
	
	constraints = create.basic.constraints(ia$n, 0, 1, 1)
	
	sol = list(
	EW = equal.weight.portfolio(ia, constraints),
	ERC = equal.risk.contribution.portfolio(ia, constraints),
	MV = min.var.portfolio(ia, constraints),
	MDP = max.div.portfolio(ia, constraints),
	MDP2 = max.div.portfolio1(ia, constraints)
	)
	
	# weights
	round(100*t(sapply(sol,c)))
		
	# risk contributions
	round(100*portfolio.risk.contribution(t(sapply(sol,c)), ia))
		
	#*****************************************************************
	# Example from paper: Duplication
	#*****************************************************************	
	ia = list(
		n = 3,
		symbols = spl('A,A1,B'),
		risk = c(20, 20, 10) / 100,
		correlation = matrix(c(1, 1, 0.5, 1, 1, 0.5, 0.5, 0.5, 1), 3, 3),
		expected.return = c(1,1,1)		
	)
	ia$cov = ia$correlation * (ia$risk %*% t(ia$risk))
	
	constraints = create.basic.constraints(ia$n, 0, 1, 1)
	
	sol = list(
	EW = equal.weight.portfolio(ia, constraints),
	ERC = equal.risk.contribution.portfolio(ia, constraints),
	MV = min.var.portfolio(ia, constraints),
	MDP = max.div.portfolio(ia, constraints),
	MDP2 = max.div.portfolio1(ia, constraints)
	)
	
	# weights
	weights = t(sapply(sol,c))
	round(100*weights)
		
	weights.base = cbind(rowSums(weights[,1:2]), weights[,3])
	round(100*weights.base)
	
	# risk contributions
	round(100*portfolio.risk.contribution(weights.base, ia.base))
	
	#*****************************************************************
	# Example from paper: Leverage
	#*****************************************************************	
	ia = list(
		n = 2,
		symbols = spl('LA,B'),
		risk = c(5, 10) / 100,
		correlation = matrix(c(1, 0.5, 0.5, 1), 2, 2),
		expected.return = c(1,1)		
	)
	ia$cov = ia$correlation * (ia$risk %*% t(ia$risk))
	
	constraints = create.basic.constraints(ia$n, 0, 1, 1)
	
	sol = list(
	EW = equal.weight.portfolio(ia, constraints),
	ERC = equal.risk.contribution.portfolio(ia, constraints),
	MV = min.var.portfolio(ia, constraints),
	MDP = max.div.portfolio(ia, constraints),
	MDP2 = max.div.portfolio1(ia, constraints)
	)
	
	# weights
	weights = t(sapply(sol,c))
	round(100*weights)
		
	weights.base = cbind(weights[,1] / 4, weights[,2])	
		weights.base = weights.base/rowSums(weights.base)
	round(100*weights.base)
	
	
	# risk contributions
	round(100*portfolio.risk.contribution(weights.base, ia.base))
	
	#*****************************************************************
	# Example from paper: Polico
	#*****************************************************************	
	ia = list(
		n = 3,
		symbols = spl('A,B,Polico'),
		risk = c(20, 10, 11.46) / 100,
		correlation = matrix(c(1, 0.5, 0.982, 0.5, 1, 0.655, 0.982, 0.655, 1), 3, 3),
		expected.return = c(1,1,1)		
	)
	ia$cov = ia$correlation * (ia$risk %*% t(ia$risk))
	
	constraints = create.basic.constraints(ia$n, 0, 1, 1)
	
	sol = list(
	EW = equal.weight.portfolio(ia, constraints),
	ERC = equal.risk.contribution.portfolio(ia, constraints),
	MV = min.var.portfolio(ia, constraints),
	MDP = max.div.portfolio(ia, constraints),
	MDP2 = max.div.portfolio1(ia, constraints)
	)
	
	# weights
	weights = t(sapply(sol,c))
	round(100*weights)
		
	weights.base = cbind(weights[,1] +  1/2 * weights[,3], weights[,2] +  1/4 * weights[,3])	
		weights.base = weights.base/rowSums(weights.base)
	round(100*weights.base)
	
	# risk contributions
	round(100*portfolio.risk.contribution(weights.base, ia.base))	
}

#*****************************************************************
# Max Sharpe portfolio using non-linear solver, based on
# http://stackoverflow.com/questions/10526243/quadprog-optimization
#' @export 
#*****************************************************************
	max.sharpe.nlp.portfolio <- function
	(
		ia,                     # input assumptions
	    constraints             # constraints
	)
	{
		risk.index = get.risky.asset.index(ia)
		cov = ia$cov[risk.index, risk.index]		
		er = ia$expected.return[risk.index]		
		
	    # obj
	    fn <- function(x){
	        (-x %*% er / sqrt( x %*% cov %*% x ))[1]
		}
        
		x0 = constraints$ub[risk.index] / sum(constraints$ub[risk.index])
	
		load.packages('Rsolnp')
		x = solnp(x0, fn, eqfun = function(w) sum(w), eqB   = 1,
  			LB = constraints$lb[risk.index], UB = constraints$ub[risk.index],
  			control = list(trace=0))
				
		set.risky.asset(x$pars, risk.index)	
	}

find.portfolio.given.risk.test <- function() 
{
	#*****************************************************************
	# Create Efficient Frontier
	#****************************************************************** 	
	# create sample historical input assumptions
	ia = aa.test.create.ia()
	
	# create long-only, fully invested efficient frontier
	n = ia$n		

	constraints = create.basic.constraints(n, 0, 1, 1)

	#*****************************************************************
	# Look at portfolios
	#****************************************************************** 	
	# load / check required packages
	load.packages('quadprog,corpcor,lpSolve,kernlab')
		
	weight = max.return.portfolio(ia, constraints)
		weight
cat(round(100*c(portfolio.return(weight,ia), portfolio.risk(weight,ia), portfolio.return(weight,ia) /  portfolio.risk(weight,ia)),2), '\n')
	
	weight = min.var.portfolio(ia,constraints)	
		weight
cat(round(100*c(portfolio.return(weight,ia), portfolio.risk(weight,ia), portfolio.return(weight,ia) /  portfolio.risk(weight,ia)),2), '\n')
	
	
	weight = target.return.portfolio.helper(ia,constraints, 12/100)	
		weight
cat(round(100*c(portfolio.return(weight,ia), portfolio.risk(weight,ia), portfolio.return(weight,ia) /  portfolio.risk(weight,ia)),2), '\n')

	weight = target.risk.portfolio.helper(ia,constraints, 10/100, silent=F)	
		weight
cat(round(100*c(portfolio.return(weight,ia), portfolio.risk(weight,ia), portfolio.return(weight,ia) /  portfolio.risk(weight,ia)),2), '\n')

}





	
	# MinCorr by David Varadi	
	# http://cssanalytics.files.wordpress.com/2012/10/minimum-correlation-mincorr-spreadsheet.xlsx
	#' @export 
min.corr.excel <- function(power.function = 1, final.scale = 'risk')
{
	power.function = as.numeric(power.function)
	final.scale = final.scale
	
	function
	(
		ia,				# input assumptions
		constraints		# constraints
	)
	{					
		risk.index = get.risky.asset.index(ia)
			n = sum(risk.index)
		
		x = min.corr.special.case(ia$risk[risk.index])
		if(!is.null(x)) return( set.risky.asset(x / sum(x), risk.index) )
		
		
		cor.matrix = ia$correlation[risk.index, risk.index]
		
		upper.index = upper.tri(cor.matrix)
		cor.m = cor.matrix[upper.index]
			cor.mu = mean(cor.m)
			cor.sd = sd(cor.m)
			
		avg.corr.contribution = (rowSums(cor.matrix) - 1) / (n - 1)
		avg.rank = rank(avg.corr.contribution)
		avg.rank = avg.rank ^ power.function
		
		rr.adjustment = avg.rank / sum(avg.rank)
						
		norm.dist.m = 0 * cor.matrix
			diag(norm.dist.m) = 0
			norm.dist.m[upper.index] = 1-pnorm(cor.m, cor.mu, cor.sd)
		norm.dist.m = (norm.dist.m + t(norm.dist.m))
		
		rr.norm.dist.m = rep.col(rr.adjustment,n) * norm.dist.m
		rr.norm.dist = colSums(rr.norm.dist.m)
		
		rr.weighted = rr.norm.dist / sum(rr.norm.dist)
		
		vol.scale = ia$risk[risk.index]
		if(final.scale == 'vol') vol.scale = diag(ia$cov[risk.index, risk.index])
		inverse.volatility.weight = (1 / vol.scale) / sum(1 / vol.scale)
		
		x = rr.weighted * inverse.volatility.weight / sum(rr.weighted * inverse.volatility.weight)
		
		# normalize weights to sum up to 1
		set.risky.asset(x / sum(x), risk.index)		
	}	
}	

# http://cssanalytics.files.wordpress.com/2012/10/minimum-correlation-mincorr-spreadsheet.xlsx
min.corr.excel.portfolio.test <- function() 
{
	ia = list()
	ia$n = 3
	ia$risk = c(14, 18, 22) / 100;
	ia$correlation = matrix(
				c(1, 0.90, 0.85,
				0.90, 1, 0.70,
				0.85, 0.70, 1), nr=3, byrow=T)
	ia$cov = ia$correlation * (ia$risk %*% t(ia$risk))

	constraints = create.basic.constraints(ia$n, 0, 1, 1)
								
	min.corr.excel.portfolio(ia,constraints)				
	
	#0.21059807 0.30878663 0.4806153
							

}

	
#' @export 
min.corr.special.case <- function(risk) {
	n = len(risk)
	if(n == 1) 1
	else if(n == 2) 1/risk
	else NULL
}

# MinCorr by David Varadi
#' @export 
min.corr <- function(power.function = 1)
{
	power.function = as.numeric(power.function)
	
	function
	(
		ia,				# input assumptions
		constraints		# constraints
	)
	{					
		risk.index = get.risky.asset.index(ia)
		
		x = min.corr.special.case(ia$risk[risk.index])
		if(!is.null(x)) return( set.risky.asset(x / sum(x), risk.index) )
		
		
		cor.matrix = ia$correlation[risk.index, risk.index]
		
		upper.index = upper.tri(cor.matrix)
		cor.m = cor.matrix[upper.index]
			cor.mu = mean(cor.m)
			cor.sd = sd(cor.m)
			
		norm.dist.m = 0 * cor.matrix
			diag(norm.dist.m) = NA
			norm.dist.m[upper.index] = 1-pnorm(cor.m, cor.mu, cor.sd)
		norm.dist.m = (norm.dist.m + t(norm.dist.m))
		
		norm.dist.avg = rowMeans(norm.dist.m, na.rm=T)
		
		norm.dist.rank = rank(-norm.dist.avg)
		norm.dist.rank = norm.dist.rank ^ power.function

		norm.dist.weight = norm.dist.rank / sum(norm.dist.rank)
		
			diag(norm.dist.m) = 0
		weighted.norm.dist.average = norm.dist.weight %*% norm.dist.m

		final.weight = weighted.norm.dist.average / sum(weighted.norm.dist.average)

				
		# re-scale weights to penalize for risk
		x = final.weight / ia$risk[risk.index]
		
		# normalize weights to sum up to 1
		set.risky.asset(x / sum(x), risk.index)		
	}		
}	
	


# MinCorr2 by David Varadi
#' @export 
min.corr2 <- function(power.function = 1)
{
	power.function = as.numeric(power.function)
	
	function
	(
		ia,				# input assumptions
		constraints		# constraints
	)
	{					
		risk.index = get.risky.asset.index(ia)
		
		x = min.corr.special.case(ia$risk[risk.index])
		if(!is.null(x)) return( set.risky.asset(x / sum(x), risk.index) )
		
		
		cor.matrix = ia$correlation[risk.index, risk.index]
			
		cor.m = cor.matrix
			diag(cor.m) = 0
			
		avg = rowMeans(cor.m)
			cor.mu = mean(avg)
			cor.sd = sd(avg)
		norm.dist.avg = 1-pnorm(avg, cor.mu, cor.sd)
		
		norm.dist.rank = rank(-norm.dist.avg)
		norm.dist.rank = norm.dist.rank ^ power.function
		
		
		norm.dist.weight = norm.dist.rank / sum(norm.dist.rank)
				
		weighted.norm.dist.average = norm.dist.weight %*% (1-cor.m)
		final.weight = weighted.norm.dist.average / sum(weighted.norm.dist.average)

				
		# re-scale weights to penalize for risk
		x = final.weight / ia$risk[risk.index]
		
		# normalize weights to sum up to 1
		set.risky.asset(x / sum(x), risk.index)		
	}		
}
	

# MinVar2 by David Varadi
# i.e. take MinCorr2 and use Covariance instead of Correlation
# The performance is similar to Minimum Variance with smaller turnover and larger drawdown
#' @export 
min.var2111 <- function(power.function = 1)
{
    power.function = as.numeric(power.function)
    
    function
    (
        ia,                # input assumptions
        constraints        # constraints
    )
    {                    
        risk.index = get.risky.asset.index(ia)
        
        x = min.corr.special.case(ia$risk[risk.index])
        if(!is.null(x)) return( set.risky.asset(x / sum(x), risk.index) )
                
        data.matrix = ia$cov[risk.index, risk.index]
            
        avg = rowMeans(data.matrix)
            data.mu = mean(avg)
            data.sd = sd(avg)
        norm.dist.avg = 1 - pnorm(avg, data.mu, data.sd)
        
        norm.dist.rank = rank(-norm.dist.avg)
        norm.dist.rank = norm.dist.rank ^ power.function        
        
        norm.dist.weight = norm.dist.rank / sum(norm.dist.rank)
                
        weighted.norm.dist.average = norm.dist.weight %*% (max(data.matrix) - data.matrix)
        final.weight = weighted.norm.dist.average / sum(weighted.norm.dist.average)
                
        # re-scale weights to penalize for risk
        x = final.weight / ia$risk[risk.index]
        
        # normalize weights to sum up to 1
        set.risky.asset(x / sum(x), risk.index)        
    }        
}

#' @export 
min.var.excel <- function(power.function = 1)
{
    power.function = as.numeric(power.function)
    
    function
    (
        ia,                # input assumptions
        constraints        # constraints
    )
    {                    
        risk.index = get.risky.asset.index(ia)
        
        x = min.corr.special.case(ia$risk[risk.index])
        if(!is.null(x)) return( set.risky.asset(x / sum(x), risk.index) )
                
        data.matrix = ia$cov[risk.index, risk.index]
            
        avg = rowMeans(data.matrix)
            data.mu = mean(avg)
            data.sd = sd(avg)
        norm.dist.avg = 1 - pnorm(avg, data.mu, data.sd)
                
        final.weight = norm.dist.avg / sum(norm.dist.avg)
                                
        # re-scale weights to penalize for risk
        x = final.weight / diag(data.matrix)
        
        # normalize weights to sum up to 1
        set.risky.asset(x / sum(x), risk.index)        
    }        
}

# http://cssanalytics.files.wordpress.com/2013/04/minimum-variance-algorithm.xlsx
min.var.excel.portfolio.test <- function() 
{
	tickers = spl('DBC,EEM,EWJ,GLD')

data = '
-0.004903678  0.005815362  0.006696429  -0.010055275
0.000703977  0.01035895  0.014412417  0.006355806
0.000351741  0.007868383  0.005464481  0.000708299
-0.000351617  -0.002838893  0.006521739  -0.004423735
-0.015124868  -0.015421115  -0.012958963  -0.010782629
-0.004642857  0.009638554  0.014223195  0.003653351
'

	ia = create.ia(matrix(scan(text=data), nc = len(tickers)))

	# overwrite cov matrix with values from excel
data = '
0.000090  0.000044  0.000028  0.000034
0.000044  0.000084  0.000068  0.000039
0.000028  0.000068  0.000101  0.000036
0.000034  0.000039  0.000036  0.000039
'

	ia$cov = matrix(scan(text=data), nc = ia$n)
	ia$risk = sqrt(diag(ia$cov))
	
	constraints = create.basic.constraints(ia$n, 0, 1, 1)
								
	min.var.excel.portfolio(ia,constraints)				
	
	#0.180495776 0.074095784 0.067164775 0.678243665
						
}


    
    


	# Portfolio Allocation adaptors for above functions
	#' @export 
	min.var2.portfolio <- function(ia,constraints) { min.corr.excel(final.scale = 'vol')(ia,constraints) } 
	
	

#' @export 
min.var2 <- function(power.function = 1)
{
    power.function = as.numeric(power.function)    
    function(ia,constraints) min.corr.excel(power.function, final.scale = 'vol')(ia,constraints)      
}
	
	

	#' @export 
	min.var.excel.portfolio <- function(ia,constraints) { min.var.excel()(ia,constraints) } 	
		
	#' @export 	
	min.corr.excel.portfolio <- function(ia,constraints) { min.corr.excel()(ia,constraints) }
	
	#' @export 	
	min.corr.portfolio <- function(ia,constraints) { min.corr()(ia,constraints) }
	
	#' @export 	
	min.corr2.portfolio <- function(ia,constraints) { min.corr2()(ia,constraints) }
	
	#' @export 	
	min.cvar <- function(alpha = 0.95) { 
		alpha = alpha
		function(ia,constraints) {
			ia$parameters.alpha = as.numeric(alpha)
			min.cvar.portfolio(ia,constraints) 
		}
	}
	#' @export 	
	min.cdar <- function(alpha = 0.95) {
		alpha = alpha
		function(ia,constraints) {
			ia$parameters.alpha = as.numeric(alpha)
			min.cdar.portfolio(ia,constraints) 
		}
	}

	#' @export 	
	min.risk.downside <- function(mar = 0) { 
		mar = mar
		function(ia,constraints) {
			ia$parameters.mar = as.numeric(mar)
			min.risk.downside.portfolio(ia,constraints) 
		}
	}
	#' @export 	
	min.mad.downside <- function(mar = 0) { 
		mar = mar
		function(ia,constraints) {
			ia$parameters.mar = as.numeric(mar)
			min.mad.downside.portfolio(ia,constraints) 
		}
	}
	
	#*****************************************************************
	# Shrinkage functions - maybe instead provide full input assumptions
	#*****************************************************************
	#' @export 	
	sample.shrinkage <- function( hist, hist.all ) {
		cov(hist, use='complete.obs', method='pearson')
	}

	#' @export 	
	sample.anchored.shrinkage <- function( hist, hist.all ) {
		cov(hist.all, use='complete.obs', method='pearson')
	}
	
	#' @export 	
	sample.mix.shrinkage <- function( hist, hist.all ) {
		0.5 * sample.shrinkage(hist, hist.all) +
		0.5 * sample.anchored.shrinkage(hist, hist.all)
	}
	
	#' @export 	
	exp.sample.shrinkage <- function( hist, hist.all ) {
		hist = na.omit(hist)
		# Exponentially weighted
		lam = 0.9
		i = 0:(nrow(hist)-1)
		wt = lam^i
		wt = wt/sum(wt)
		cov.wt(hist, wt=rev(wt))$cov
	}		
		
	#' @export 	
	diagonal.shrinkage <- function( hist, hist.all ) {
		n = ncol(hist)
		s0 = apply(hist, 2, sd, na.rm=T)
		diag(n) * (s0 %*% t(s0))
	}
		
	#' @export 	
	average.shrinkage <- function( hist, hist.all ) {
		n = ncol(hist)
		correlation = cor(hist, use='complete.obs', method='pearson')
		avg.correlation = (sum(correlation) - n) / (n*n - n)		
		create.cov.matrix(avg.correlation, hist)
	}

	#' @export 	
	min.shrinkage <- function( hist, hist.all ) {
		correlation = cor(hist, use='complete.obs', method='pearson')
		create.cov.matrix(min(correlation), hist)
	}
	
	#' @export 	
	max.shrinkage <- function( hist, hist.all ) {
		n = ncol(hist)
		correlation = cor(hist, use='complete.obs', method='pearson')
		create.cov.matrix(max(correlation-diag(n)), hist)
	}
		
	#' @export 	
	create.cov.matrix <- function( value, hist ) {
		n = ncol(hist)
		s0 = apply(hist, 2, sd, na.rm=T)
		temp = diag(n)
		((matrix(1,n,n) - temp) * value + temp) * (s0 %*% t(s0))	
	}

	#' @export 	
	ledoit.wolf.shrinkage <- function( hist, hist.all ) {
		require(BurStFin)
		var.shrink.eqcor(hist, 1, compatible = T)
	}
		
	#' @export 	
	factor.model.shrinkage <- function( hist, hist.all ) {
		require(BurStFin)
		factor.model.stat(hist, 1)
	}
	
	#*****************************************************************
	# Shrinkage functions
	#*****************************************************************	
	# OAS
	# http://tbayes.eecs.umich.edu/_media/yilun/covestimation/demo.m
	# http://www.ledoit.net/ole2_abstract.htm
	# http://www.ledoit.net/honey_abstract.htm
	#
	# Matlab
	# x=randn(2000,30);
	# [sigma,shrinkage]=covCor(x,-1)
	# csvwrite('test.csv',x)
	#
	# Shrinks towards constant correlation matrix, Ledoit and Wolf (2004)
	#
	# x = as.matrix(read.csv('c:/Michael_Kapler/Soft/R/ira/test.csv', header =F))
	# x=matrix(rnorm(3*200),200,3)
	# covCor(x)
	#
	# http://www.jasonhsu.org/uploads/1/0/0/7/10075125/covariance_estimations.pdf
	# http://code.google.com/p/pmtk3/source/browse/trunk/toolbox/ProbDist/sub/shrink2para.m?r=1669
	#
	# http://www.ledoit.net/honey_abstract.htm
	# http://www.ledoit.net/honey.pdf  ,  page  14
	#*****************************************************************	
	
	# based on the cov_shrink in tawny package
	#' @export 	
	cov.shrink <- function(h, prior = NULL, shrinkage = NULL, roff.method = 1) {	
		require(tawny)
		#class(h) = c('AssetReturns', class(h)) 
	
		T = nrow(h)
		S = cov.sample(h)
	  	
		if( is.function(prior) ) prior = prior(h)
		if( is.null(prior) ) prior = tawny::cov.prior.cc(S)
	  
	  	if( is.null(shrinkage) ) {
	  		# shrinkage.intensity
			p = tawny::shrinkage.p(h, S)
		
			if( roff.method == 0 )
				r = sum(p$diags, na.rm=TRUE) 
			else			
		  		r = tawny::shrinkage.r(h, S, p)
		  		
		  	c = tawny::shrinkage.c(prior, S)
	  		k = (p$sum - r) / c
	  		#k = tawny::shrinkage.intensity(h, prior, S) 	  		
			shrinkage = max(0, min(k/T, 1))
		}	  		
		return(list(sigma = shrinkage * prior + (1 - shrinkage) * S, shrinkage = shrinkage))  
	}	
	
	
	# cov(h, use='complete.obs', method='pearson') *(T - 1)/T
	#' @export 	
	cov.sample <- function(h) {		
		# center x, de-mean returns
		T = nrow(h)
		x = h - rep.row(colMeans(h), T)
		
		# compute sample covariance matrix
		(t(x) %*% x) / T
	}
	
	#' @export 	
	cov.const.cor <- function(h) {		
		sample = cov.sample(h)
	
		# compute average correlation
		n = ncol(h)
		var = diag(sample)
		cov.mat = sqrt(var) %*% t(sqrt(var))	
		avg.rho = (sum(sample / iif(cov.mat==0,1,cov.mat))-n)/(n*(n-1))
		
		# compute prior
		prior = avg.rho * cov.mat
		diag(prior) = var
		
		prior
	}
	
	#' @export 	
	cov.diag <- function(h) {		
		S = cov.sample(h)
		diag(diag(S))
	}
	
	#' @export 	
	cov.market <- function(h) {		
		# center x, de-mean returns
		T = nrow(h)
		x = h - rep.row(colMeans(h), T)
		xmkt = rowMeans(x)
		
		# compute sample covariance matrix and prior
		n = ncol(h)	
		sample=cov(cbind(x, xmkt))*(T - 1)/T
		covmkt=sample[1:n,n+1]
		varmkt=sample[n+1,n+1]	
		prior=covmkt %*% t(covmkt) / varmkt
		diag(prior) = diag(sample[1:n,1:n])
		
		prior
	}	
		
	#' @export 	
	cov.2param <- function(h) {
		sample = cov.sample(h)		
	
		# compute prior
		n = ncol(h)
		meanvar=mean(diag(sample))
		meancov=(sum(sample) -  sum(diag(sample)))/(n*(n-1))
		meanvar*diag(n) + meancov*(1-diag(n))
	}
	
	
	# Shrinkage adaptors for above functions
	#' @export 
	shrink.diag <- function(s=NULL) { s=s; function(x, a) { cov.shrink(x, cov.diag, s, 0)$sigma }}
	
	#' @export 	
	shrink.const.cor <- function(s=NULL) { s=s; function(x, a) { cov.shrink(x, cov.const.cor, s, 1)$sigma }}
	
	#' @export 	
	shrink.single.index <- function(s=NULL) { s=s; function(x, a) { cov.shrink(x, cov.market, s, 1)$sigma }}
	
	#' @export 	
	shrink.two.parameter <- function(s=NULL) { s=s; function(x, a) { cov.shrink(x, cov.2param, s, 1)$sigma }}

	
	#*****************************************************************
	# Group Methods
	#*****************************************************************
	#' @export 	
	empty.group <- function
	(
		ia				# input assumptions
	)
	{
		return( rep(1,ia$n) )
	}		

	
#' @export 		
static.group <- function(group) 
{	
	group = as.numeric(group)
	function
	(
		ia			# input assumptions
	)
	{
		group[ia$index]
	}		
}		
	
	
	
	
	# Find groups using clustering algorithm
	#' @export 	
	cluster.group.hclust <- function
	(
		ia				# input assumptions
	)
	{		
		if(ia$n <= 2) return(c(1,1)[1:ia$n])
		
		dissimilarity = 1 - ia$correlation
    	distance = as.dist(dissimilarity)
    	
    	fit =  hclust(distance, method='ward')
    	minh = min(fit$height)
    	maxh = max(fit$height)
		group = cutree(fit, h=minh + (maxh - minh) /3)			
		return( group )		
		
		#plot(fit, axes=F,xlab='', ylab='',sub ='')
		#rect.hclust(fit, h=minh + (maxh - minh) /3 , border='red') 	
			
		# alternative		
		#group = cutree(fit, h=max(fit$height)/2)			
		#group = cutree(fit, k=3)			
	}		
	
	

	# Find groups using clustering algorithm
	#' @export 	
	cluster.group.kmeans.90 <- function
	(
		ia				# input assumptions
	)
	{
		if(ia$n <= 2) return(c(1,1)[1:ia$n])
		
		dissimilarity = 1 - cor(ia$hist.returns, use='complete.obs',method='spearman')
		#dissimilarity = 1 - cor(ia$hist.returns, use='pairwise.complete.obs',method='spearman')		
    	distance = as.dist(dissimilarity)
    	
		n = ncol(ia$correlation)
			n = ceiling(n*2/3)
		xy = cmdscale(distance)

		for (i in 2:n) {
			fit = kmeans(xy, centers=i, iter.max=100, nstart=100)
			# percentage of variance explained by clusters
			p.exp = 1- fit$tot.withinss / fit$totss
			if(p.exp > 0.9) break			
		}

    	group = fit$cluster
		return( group )
	}		

	
	# Find groups using clustering algorithm
	#' @export 	
	cluster.group.kmeans.elbow <- function
	(
		ia				# input assumptions
	)
	{		
		if(ia$n <= 2) return(c(1,1)[1:ia$n])
		
		dissimilarity = 1 - cor(ia$hist.returns, use='complete.obs',method='spearman')
		#dissimilarity = 1 - cor(ia$hist.returns, use='pairwise.complete.obs',method='spearman')
    	distance = as.dist(dissimilarity)
    	
		n = ncol(ia$correlation)
			n = ceiling(n*2/3)
		xy = cmdscale(distance)

		p.exp = rep(NA, n)
		for (i in 2:n) {
			fit = kmeans(xy, centers=i, iter.max=100, nstart=100)
			# percentage of variance explained by clusters
			p.exp[i] = 1- fit$tot.withinss / fit$totss
		}

		# http://stackoverflow.com/questions/2018178/finding-the-best-trade-off-point-on-a-curve
		icluster = find.maximum.distance.point(p.exp[-1]) + 1
		
		fit = kmeans(xy, centers=icluster, iter.max=100, nstart=100)
    	group = fit$cluster
		return( group )
	}		
	
# Idea by David Varadi	
# http://cssanalytics.wordpress.com/2013/11/26/fast-threshold-clustering-algorithm-ftca/
# Original code by Pierre Chretien
# Small updates by Michael Kapler 	
#' @export
cluster.group.FTCA <- function
(
	threshold = 0.5
)
{
	function
	(
	ia				# input assumptions
	)
	{		
		n = ia$n
		map.index = 1:n
		min.cluster.group = 1
		
		if (threshold >= 1) return(map.index)
	
		group = rep(0, n)
			names(group) = names(ia$risk)
		index = rep(TRUE, n)
			names(index) = names(ia$risk)
					
		while (n > 0) {			
			if (n == 1) {
				group[index] = min.cluster.group
				break
			} else {
				cor.matrix = ia$correlation[index, index]
				if (n == 2) {
					if (cor.matrix[1,2] > threshold)
						group[index] = min.cluster.group
					else
						group[index] = c(min.cluster.group, min.cluster.group + 1)				
					break
				} else {
					avg.corr.contribution = (rowSums(cor.matrix) - 1) / (n - 1)
					avg.rank = rank(avg.corr.contribution)
					tip = which.min(avg.rank)
					top = which.max(avg.rank)
					if (cor.matrix[tip,top] > threshold) {
						group[index] = min.cluster.group
						break
					} else {
						index.top = map.index[index][cor.matrix[,top] > threshold]					
						index.tip = map.index[index][cor.matrix[,tip] > threshold]
							
						group[index.tip] = min.cluster.group
						group[index.top] = min.cluster.group + 1
	
						index[index.tip] = F
						index[index.top] = F
						
						min.cluster.group = min.cluster.group + 2
						n = sum(index)
					}
				}	
			}
		}
		return(group)
	}
}	


cluster.group.FTCA.test <- function() {
	#*****************************************************************
	# Load historical data for ETFs
	#****************************************************************** 
	load.packages('quantmod')

	tickers = spl('GLD,UUP,SPY,QQQ,IWM,EEM,EFA,IYR,USO,TLT')	
	tickers = spl('GLD,TLT,SPY,IWM,QQQ,EFA,EEM,IYR')
	tickers = spl('XLY,XLP,XLE,XLF,XLV,XLI,XLB,XLK,XLU')
	
	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '1900-01-01', env = data, auto.assign = T)
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)		
	bt.prep(data, align='keep.all')

	
	#*****************************************************************
	# Helper function to compute portfolio allocation additional stats
	#****************************************************************** 
	portfolio.allocation.custom.stats.clusters <- function(x,ia) {
		return(list(
			clusters.FTCA = cluster.group.FTCA(0.5)(ia)			
		))
	}
	
	#*****************************************************************
	# Find clusters
	#****************************************************************** 		
	periodicity = 'months'
	lookback.len = 252
		
	obj = portfolio.allocation.helper(data$prices, 
		periodicity = periodicity, lookback.len = lookback.len,
		min.risk.fns = list(EW=equal.weight.portfolio),
		custom.stats.fn = portfolio.allocation.custom.stats.clusters
	) 			
	
	clusters = obj$clusters.FTCA$EW	
	
	clusters['2012:05::']
	
	# create temp matrix with data you want to plot
	temp1 = clusters['2011::']
	plot.data = coredata(temp1)
		rownames(plot.data) = format(index.xts(temp1), '%Y%m')		
	plot.table(plot.data, highlight = plot.data + 1)
	
	#*****************************************************************
	# Code Strategies
	#****************************************************************** 					
	obj = portfolio.allocation.helper(data$prices, 
		periodicity = periodicity, lookback.len = lookback.len, 
		min.risk.fns = list(
			# cluster
			C.EW.kmeans = distribute.weights(equal.weight.portfolio, cluster.group.kmeans.90),
			C.EW.FTCA = distribute.weights(equal.weight.portfolio, cluster.group.FTCA(0.5)),

			C.RP.kmeans = distribute.weights(risk.parity.portfolio(), cluster.group.kmeans.90),
			C.RP.FTCA = distribute.weights(risk.parity.portfolio(), cluster.group.FTCA(0.5)),

			C.MD.kmeans = distribute.weights(max.div.portfolio, cluster.group.kmeans.90),
			C.MD.FTCA = distribute.weights(max.div.portfolio, cluster.group.FTCA(0.5)),

			C.MV.kmeans = distribute.weights(min.var.portfolio, cluster.group.kmeans.90),
			C.MV.FTCA = distribute.weights(min.var.portfolio, cluster.group.FTCA(0.5)),
						
			C.MVE.kmeans = distribute.weights(min.var.excel.portfolio, cluster.group.kmeans.90),
			C.MVE.FTCA = distribute.weights(min.var.excel.portfolio, cluster.group.FTCA(0.5)),

			C.MCE.kmeans = distribute.weights(min.corr.excel.portfolio, cluster.group.kmeans.90),
			C.MCE.FTCA = distribute.weights(min.corr.excel.portfolio, cluster.group.FTCA(0.5)),

			C.MS.kmeans = distribute.weights(max.sharpe.portfolio(), cluster.group.kmeans.90),
			C.MS.FTCA = distribute.weights(max.sharpe.portfolio(), cluster.group.FTCA(0.5)),
			
			C.ERC.kmeans = distribute.weights(equal.risk.contribution.portfolio, cluster.group.kmeans.90),
			C.ERC.FTCA = distribute.weights(equal.risk.contribution.portfolio, cluster.group.FTCA(0.5))		
		)
	)
	
	models = create.strategies(obj, data)$models
						
    #*****************************************************************
    # Create Report
    #******************************************************************    
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
	strategy.performance.snapshoot(models, T)
dev.off()

png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')	
	barplot.with.labels(sapply(models, compute.turnover, data), 'Average Annual Portfolio Turnover')
dev.off()
		
}	

	

	###############################################################################
	# Distribute Weights according to Weighting Scheme and Group Method(clusters)
	###############################################################################
	# min.risk.fns = list(G0.MV = distribute.weights(min.risk.portfolio, empty.group),					
	# 					G2.MV = distribute.weights(min.risk.portfolio, cluster.group))
	#' @export 	
	distribute.weights <- function
	(
		fn,				# function that dictates how to distribute weights both across and within groups
		group.fn = NA,	# group factor
		fn.within = NA
	)
	{
		fn = match.fun(fn)
		if(!is.function(group.fn)) if(!is.na(group.fn)) group.fn = match.fun(group.fn)
		
		if(!is.function(fn.within)) if(!is.na(fn.within)) fn.within = match.fun(fn.within)
		if(!is.function(fn.within)) fn.within = fn
	
		function
		(
			ia,			# input assumptions
			constraints	# constraints
		)
		{		
			if(!is.function(group.fn)) return(fn(ia, constraints))
			
			group = as.numeric(group.fn(ia))

			groups = unique(group[!is.na(group)])
				ngroups = len(groups)
			if(ngroups == 1) return(fn.within(ia, constraints))
			
					
			weight0 = rep(0, ia$n)
			
			if(ngroups == 0) return(weight0)
			group[is.na(group)] = Inf			
				
			# returns for each group			
			hist.g = NA * ia$hist.returns[,1:ngroups]
				
			# compute weights within each group	
			for(g in 1:ngroups) {
				index = group == groups[g]
				if( sum(index) == 1 ) {
					weight0[index] = 1
					hist.g[,g] = ia$hist.returns[, index, drop=F]
				} else {
					ia.temp = create.ia.subset(ia, index)

					constraints.temp = create.basic.constraints(ia.temp$n, 0, 1, 1)

					w0 = match.fun(fn.within)(ia.temp, constraints.temp)
						weight0[index] = w0
						# Note that: sd(return0) = portfolio.risk(weight0, ia)	
						# return0 = ia$hist.returns	%*% weight0
					hist.g[,g] = ia.temp$hist.returns %*% w0
				}
			}
				
			# create GROUP input assumptions
			ia.g = create.ia(hist.g)
							
			constraints.g = create.basic.constraints(ngroups, 0, 1, 1)				
				
			# find group weights
			group.weights = match.fun(fn)(ia.g, constraints.g)
					
			# multiply out group.weights by within group weights			
			for(g in 1:ngroups)
					weight0[group == groups[g]] = weight0[group == groups[g]] * group.weights[g]
			
			weight0			
		}
	}	
	
	
	
	
	
	#*****************************************************************
	# Maps for portfolio optimization and clustering functions
	#*****************************************************************
	#' @export 	
	get.algo <- function(algo.name, has.param = F) {
		algo.map = list(
			'cluster' = distribute.weights,
			'max.sharpe' = max.sharpe.portfolio,
			'risk.parity' = risk.parity.portfolio
		)
	
		if(any(names(algo.map) == algo.name))
			if(has.param)
				algo.map[[ algo.name ]]
			else
				algo.map[[ algo.name ]]()
		else {
			if(has.param) {
				fn = try( match.fun(algo.name) , silent = TRUE)				
				if(class(fn)[1] == 'try-error')	fn = try( match.fun(paste0(algo.name, '.portfolio')) , silent = TRUE)			
			} else {
				fn = try( match.fun(paste0(algo.name, '.portfolio')) , silent = TRUE)
				if(class(fn)[1] == 'try-error')	fn = try( match.fun(algo.name) , silent = TRUE)
			}
			if(class(fn)[1] == 'try-error') stop(paste('get.algo', fn))
			fn
		}
	}
	
	#' @export 		
	get.group <- function(group.name) {
		group.map = list(
			'none' = empty.group,
			'hclust' = cluster.group.hclust,
			'kmeans90' = cluster.group.kmeans.90
		)	
		
		if(any(names(group.map) == group.name))
			group.map[[ group.name ]]
		else
			stop(paste('Unknown group', group.name))
	}
	
	# parse strategys
	#' @export 	
	map.min.risk.fns <- function(strategys) {	
		strategys = spl(strategys,';')
		
		min.risk.fns = list()
		#strategys = spl(";EW,Equal.Weight,;MCE,Min.Corr.Excel,1;MC,Min.Corr,1;MC2,Min.Corr2,1;C.EW,Cluster,kmeans90:min.var;Empty,Cluster,hclust:min.corr2",';')		
		#strategys = spl('EW-Equal.Weight,RP-Risk.Parity,RP.CvaR-Risk.Parity:CVaR,MD-Max.Div,MV-Min.Var[AAA],MCA-Min.Corr,MCA2-Min.Corr2,MVA-Min.Var.Excel,MVA2-Min.Var2')
		#strategys = gsub(':',',',gsub('-',',',gsub('\\[.*?\\]','',strategys)))
		strategys = strategys[ nchar(strategys) > 0]		
		for(i in 1:len(strategys)) {
			temp = spl(strategys[i])
						
			# create name, if empty
			f.name = temp[1]
			if(nchar(f.name) == 0 || f.name == 'Empty') f.name = paste(temp[-1], collapse='-') 
			
			# remove name and create functions
			temp = tolower(temp)[-1]		
			if(len(temp) == 1)
				min.risk.fns[[ f.name ]] = get.algo(temp[1])
			else {
				if(temp[1] == 'cluster') {
					params = trim(spl(temp[2], ':'))
					min.risk.fns[[ f.name ]] = get.algo(temp[1], T)( get.algo(params[2]), get.group(params[1]) )								
				} else
					min.risk.fns[[ f.name ]] = get.algo(temp[1],T)(temp[-1])
			}
		}					
		min.risk.fns	
	}



			
	#*****************************************************************
	# Random Subspace Optimization(RSO)
	# https://cssanalytics.wordpress.com/2013/10/06/random-subspace-optimization-rso/
	# http://systematicedge.wordpress.com/2013/10/14/random-subspace-optimization-max-sharpe/
	#*****************************************************************
	#' @export 	
rso.portfolio <- function
(
    weight.fn,    # function that dictates how to distribute weights
    k,            # number of assets to include, should be less than ia$n
    s,            # number of samples
	const.lb = 0, 
	const.ub = 1,
	const.sum = 1    
)
{
    weight.fn = match.fun(weight.fn)
    k = k
    s = s
    
    const.lb = const.lb
    const.ub = const.ub
    const.sum = const.sum
    
    constraints0 = create.basic.constraints(k, const.lb, const.ub, const.sum)
            
    function
    (
        ia,            # input assumptions
        constraints    # constraints
    )
    {
    	constraints1 = constraints0
    	k1 = k
		if(k > ia$n) {
			k1 = ia$n			
			constraints1 = create.basic.constraints(k1, const.lb, const.ub, const.sum)
		}
         
        # randomly select k assets; repeat s times
		space = seq(1:ia$n)
        index.samples =t(replicate(s, sample(space, size=k1)))
        weight = matrix(NA, nrow = s, ncol = ia$n)
    
        # resample across randomly selected assets
        for(i in 1:s){
        	ia.temp = create.ia.subset(ia, index.samples[i,])
            weight[i,index.samples[i,]] = weight.fn(ia.temp, constraints1)
        }
        final.weight = ifna(colMeans(weight, na.rm=T), 0)
        # normalize weights to sum up to 1
        final.weight / sum(final.weight)
    }    
}    
        
    
    

#*****************************************************************
# Parallel version of Portfolio Allocation Helper
#' @export 
#*****************************************************************
portfolio.allocation.helper.parallel <- function
(
	cores = 1,
	
	prices,					# prices
	periodicity = 'weeks',	#  rebalancing frequency
	period.ends = endpoints(prices, periodicity),	# rebalancing times
	
	lookback.len = 60,		# lookback to construct input assumptions each period
	n.skip = 1, # number of observations required for computaions. i.e. to compute return we need at least one observation
	
	universe = prices[period.ends,,drop=F]>0,
	
	prefix = '',
	
	min.risk.fns = 'min.var.portfolio',	# portfolio construction functions
	custom.stats.fn = NULL,
	shrinkage.fns = 'sample.shrinkage',	# covariance Shrinkage Estimator functions
	
	create.ia.fn = create.ia,
	update.ia.fn = update.ia,
	
	adjust2positive.definite = T,
	silent = F,
	
	log = log.fn(),	
	log.frequency = 10,		
	
	const.lb = 0, 
	const.ub = 1,
	const.sum = 1
) 
{
	cores = round(cores)
	if(cores <= 1)
		return(portfolio.allocation.helper(prices, periodicity, period.ends, lookback.len, n.skip,
			universe, prefix,
			min.risk.fns, custom.stats.fn, shrinkage.fns,
			create.ia.fn, update.ia.fn,
			adjust2positive.definite, silent, log,
			const.lb, const.ub, const.sum))	
	
	# http://vikparuchuri.com/blog/parallel-r-loops-for-windows-and-linux/
	# http://statcompute.wordpress.com/page/3/
	load.packages('foreach,doParallel')
	#registerDoParallel(cores = cores)
	
cl<-makeCluster(cores)
registerDoParallel(cl, cores = cores)
	
	#*****************************************************************
	# Setup
	#*****************************************************************
	period.ends = period.ends[period.ends > 0]
	start.i = which(period.ends >= (lookback.len + n.skip))[1]
	chunks = c(1, floor(seq(start.i, len(period.ends)+1, length.out = cores + 1)[-1]))
	temp = 1:len(period.ends)
	
#	i=1
#	temp[-c(chunks[i] : (chunks[i+1]-1))]
	
	if( nrow(universe) != len(period.ends) ) {
		if( nrow(universe) == nrow(prices) )
			universe = universe[period.ends,,drop=F]
		else
			stop("universe incorrect number of rows")		
	}
				
	universe[is.na(universe)] = F	
	

	
	# run allocations
	#out <- foreach(i=1:cores, .packages='quantmod') %do% {
	# .verbose=TRUE
	out <- foreach(i=1:cores, .packages=spl('quantmod,SIT')) %dopar% {
		new.universe = universe
		new.universe[temp[-c(chunks[i] : (chunks[i+1]-1))],]=F
		portfolio.allocation.helper(prices, periodicity, period.ends, lookback.len, n.skip,
			universe = new.universe, prefix,
			min.risk.fns, custom.stats.fn, shrinkage.fns,
			create.ia.fn, update.ia.fn,
			adjust2positive.definite, silent, log, log.frequency, 
			const.lb, const.ub, const.sum)
	}		
	
stopCluster(cl)	
	
	# combine
	base.out = out[[1]]
	for(i in 2:cores) {
		include.index = temp[c(chunks[i] : (chunks[i+1]-1))]
		for(v in names(out[[i]])) {
			if(is.list(out[[i]][[v]]))
				for(n in names(out[[i]][[v]]))
					base.out[[v]][[n]][include.index,] = out[[i]][[v]][[n]][include.index,]
			
			if(is.matrix(out[[i]][[v]]))
				base.out[[v]][include.index,] = out[[i]][[v]][include.index,]
		}
	}	
	base.out	
	
}


#*****************************************************************
# Portfolio Allocation Helper - distribute portfolio weights according to 
# the given weighting scheme (min.risk.fns)
#' @export 
#*****************************************************************
portfolio.allocation.helper <- function
(
	prices,					# prices
	periodicity = 'weeks',	#  rebalancing frequency
	period.ends = endpoints(prices, periodicity),	# rebalancing times
	
	lookback.len = 60,		# lookback to construct input assumptions each period
	n.skip = 1, # number of observations required for computaions. i.e. to compute return we need at least one observation
	
	universe = prices[period.ends,,drop=F]>0,
	
	prefix = '',
	
	min.risk.fns = 'min.var.portfolio',	# portfolio construction functions
	custom.stats.fn = NULL,
	shrinkage.fns = 'sample.shrinkage',	# covariance Shrinkage Estimator functions
	
	create.ia.fn = create.ia,
	update.ia.fn = update.ia,
	
	adjust2positive.definite = T,
	silent = F,
	
	log = log.fn(),	
	log.frequency = 10,	

	
	const.lb = 0, 
	const.ub = 1,
	const.sum = 1
) 
{
	load.packages('quadprog,corpcor')
	load.packages('quadprog,corpcor,lpSolve,kernlab')

		
	
	#*****************************************************************
	# Setup
	#*****************************************************************
	period.ends = period.ends[period.ends > 0]
	
	if( nrow(universe) != len(period.ends) ) {
		if( nrow(universe) == nrow(prices) )
			universe = universe[period.ends,,drop=F]
		else
			stop("universe incorrect number of rows")		
	}
			
	
	universe[is.na(universe)] = F
	
	if(len(const.lb) == 1) const.lb = rep(const.lb, ncol(prices))
	if(len(const.ub) == 1) const.ub = rep(const.ub, ncol(prices))
		
	#*****************************************************************
	# Transform min.risk.fns and shrinkage.fns to the named lists
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

	
	if(is.character(shrinkage.fns)) {
		shrinkage.fns = spl(shrinkage.fns)
		names(shrinkage.fns) = shrinkage.fns
		shrinkage.fns = as.list(shrinkage.fns)
	}
	for(i in 1:len(shrinkage.fns)) {
		f = names(shrinkage.fns)[i]
		f.name = gsub('\\.shrinkage', '', f[1])
		
		if(is.character(shrinkage.fns[[ i ]]))
			shrinkage.fns[[ i ]] = match.fun(f)		
		names(shrinkage.fns)[i] = f.name			
	}
	
		
	#*****************************************************************
	# Code Strategies
	#****************************************************************** 		
	dates = index(prices)[period.ends]
	
	weight = NA * prices[period.ends,,drop=F]
	
	prices = coredata(prices)
	ret = prices / mlag(prices) - 1
	
	start.i = which(period.ends >= (lookback.len + n.skip))[1]

	#weight = NA * prices[period.ends,,drop=F]
		weight[] = 0
		
	weights = list()			
	for(f in names(min.risk.fns)) 
		for(c in names(shrinkage.fns)) 
			weights[[ paste(f,c,sep='.') ]] = weight
				

	# custom stats logic		
	custom = list()
	if( !is.null(custom.stats.fn) ) {
		custom.stats.fn = match.fun(custom.stats.fn)
	
		dummy = matrix(NA, nr=nrow(weight), nc=len(weights))		
			colnames(dummy) = names(weights)
			dummy = make.xts(dummy, dates)	
			
   		#temp = custom.stats.fn(1:ncol(ret), create.ia(ret))
   		temp = ret
   			temp[] = rnorm(prod(dim(ret)))
   		temp = custom.stats.fn(1:ncol(ret), create.ia(temp))
   		
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
		
	index.map = 1:ncol(ret)
   				
if(!is.na(start.i)) {
	# construct portfolios
	for( j in start.i:len(period.ends) ) {
		i = period.ends[j]
		
		# histtory to construct input assumptions
		hist = ret[ (i- lookback.len +1):i,, drop=F]
		
		# require all assets to have full price history
		include.index = count(hist)== lookback.len      

		index = universe[j,] & include.index						
		n = sum(index)
		
		if(n > 0) {
			hist = hist[ , index, drop=F]
			hist.all = ret[ 1:i, index, drop=F]		

			if(n > 1) {
				constraints = create.basic.constraints(n, const.lb[index], const.ub[index], const.sum)
							
				# create historical input assumptions
				ia.base = create.ia.fn(hist, index.map[index], i)
				
				for(c in names(shrinkage.fns)) {
					cov.shrink = shrinkage.fns[[c]](hist, hist.all)					
					ia = update.ia.fn(ia.base, c, cov.shrink)
										
					# adjust correlation and covariance matrices to be positive defined
					if(adjust2positive.definite) {
						temp = try(make.positive.definite(ia$cov, 0.000000001), TRUE)	
							if(!inherits(temp, 'try-error')) ia$cov = temp				
						temp = try(make.positive.definite(ia$correlation, 0.000000001), TRUE)	
							if(!inherits(temp, 'try-error')) ia$correlation = temp							
					}
					
					# find optimal portfolios under different risk measures
					for(f in names(min.risk.fns)) {
						fname = paste(f,c,sep='.')	
						if (j > 1) constraints$x0 = as.vector( weights[[ fname ]][(j-1),index] )
						weights[[ fname ]][j,index] = min.risk.fns[[f]](ia, constraints)
					}
				}							
			} else {
				ia = create.ia.fn(hist, index.map[index], i)
				
				for(c in names(shrinkage.fns)) {
					for(f in names(min.risk.fns)) {
						fname = paste(f,c,sep='.')				
						weights[[ fname ]][j,index] = 1
					}
				}			
			}
				
			# custom stats logic		
			if( !is.null(custom.stats.fn) ) {
				for(w in names(weights)) {
					x = as.vector(weights[[ w ]][j, index])
					temp = custom.stats.fn(x, ia)

			   		for(ci in names(temp)) {
			   			if(is.list(custom[[ ci ]]))
			   				custom[[ ci ]][[ w ]][j, index] = temp[[ ci ]]
			   			else
			   				custom[[ ci ]][j, w] = temp[[ ci ]]
			   		}
				}
		   	} 		
			
		}
			
		if( j %% log.frequency == 0) if(!silent) log(j, percent = (j - start.i) / (len(period.ends) - start.i))			
	}
}
	
	if( len(shrinkage.fns) == 1 ) {
		names(weights) = gsub( paste('\\.', names(shrinkage.fns), '$', sep=''), '', names(weights) )
		for(ci in names(custom))
			names(custom[[ ci ]]) = gsub( paste('\\.', names(shrinkage.fns), '$', sep=''), '', names(custom[[ ci ]]) )		
	}

	
	return(c(list(weights = weights, period.ends = period.ends,
		periodicity = periodicity, lookback.len = lookback.len), custom))
}



# compute portfolio allocation additional stats
#' @export 
portfolio.allocation.custom.stats <- function(x,ia) {
	risk.contributions = portfolio.risk.contribution(x, ia)
	return(list(
		# vectors
		risk.contributions = risk.contributions,				
		
		# numbers
		degree.diversification = 1 - sqrt(x %*% ia$cov %*% x) / (ia$risk %*% x),
		risk.gini = 1 - portfolio.concentration.gini.coefficient(risk.contributions)
	))
}



# this is a basic version of portfolio.allocation.helper function
#' @export 
portfolio.allocation.helper.basic <- function
(
	prices,					# prices
	periodicity = 'weeks',	#  rebalancing frequency
	period.ends = endpoints(prices, periodicity),	# rebalancing times
	
	lookback.len = 60,		# lookback to construct input assumptions each period
	prefix = '',
	
	universe = prices[period.ends,]>0,
	
	min.risk.fns = 'min.var.portfolio',	# portfolio construction functions
	silent = F
) 
{
	load.packages('quadprog,corpcor')
	
	#*****************************************************************
	# Setup
	#*****************************************************************
	period.ends = period.ends[period.ends > 0]
	
	universe[is.na(universe)] = F
	
	if(is.character(min.risk.fns)) min.risk.fns = spl(min.risk.fns)
		
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
	for(f in min.risk.fns) 
		weights[[f]] = weight
		
						
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
			if(n > 1) {			
				hist = hist[ , index]
	
				constraints = create.basic.constraints(n, 0, 1, 1)
							
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
					
				# find optimal portfolios under different risk measures
				for(f in min.risk.fns)
					weights[[ f ]][j,index] = match.fun(f)(ia, constraints)						
			} else {
				for(f in min.risk.fns)
					weights[[ f ]][j,index] = 1
			}
		}
			
		if( j %% 10 == 0) if(!silent) cat(j, '\n')		
	}
		
	return(list(weights = weights, period.ends = period.ends,
		periodicity = periodicity, lookback.len = lookback.len))
}




# Create strategies based on portfolio weights
#' @export 
create.strategies <- function
(
	obj,	# portfolio.allocation object: list(weights = weights, period.ends = period.ends)
	data,	# historical prices
	leverage = 1,	
	min.weight = NA,
	round.weight = NA,
	execution.price = NA,
	close.all.positions.index = NULL,
	silent = F,	
	log = log.fn(),	
	prefix = '',
	suffix = '',
	clean.signal = F,	# flag to remove excessive signal
	
	...		
) 
{
	# adjust weights
	if(len(leverage) > 1 || leverage[1] != 1) {				
		if(len(leverage) == 1) leverage = rep(leverage, len(obj$weights))		
		for(i in 1:len(obj$weights)) obj$weights[[i]] = leverage[i] * obj$weights[[i]]		
	}
	
	if(!is.na(min.weight) && min.weight != 0) for(i in names(obj$weights)) 
		obj$weights[[i]][] = bt.apply.min.weight(coredata(obj$weights[[i]]), min.weight)
	
	if(!is.na(round.weight) && round.weight != 0) for(i in names(obj$weights)) {
		# round a few times to get more consisitent results
		obj$weights[[i]][] = bt.apply.round.weight(coredata(obj$weights[[i]]), round.weight)
		obj$weights[[i]][] = bt.apply.round.weight(coredata(obj$weights[[i]]), round.weight)
		obj$weights[[i]][] = bt.apply.round.weight(coredata(obj$weights[[i]]), round.weight)
	}

	#*****************************************************************
	# Create strategies
	#****************************************************************** 		
	models = list()
	n = len(names(obj$weights))
	for(j in 1:n) {
		i = names(obj$weights)[j]
		i = paste(prefix, i, suffix, sep='')
		
if(!silent) log(i, percent = j / n)
	
		data$weight[] = NA
			data$execution.price[] = execution.price
			data$weight[obj$period.ends,] = obj$weights[[j]]
#			data$weight[obj$period.ends,] = as.matrix(obj$weights[[j]][1:len(obj$period.ends),])

			if( !is.null(close.all.positions.index) ) data$weight[close.all.positions.index,] = 0

		models[[i]] = bt.run.share(data, clean.signal = clean.signal, silent = silent, ...)
		
#		models[[i]]$risk.contribution = obj$risk.contributions[[j]]
		models[[i]]$period.weight = obj$weights[[j]]
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
		periodicity = 'months', lookback.len = 60, 
		min.risk.fns = list(
			EW=equal.weight.portfolio,
			RP=risk.parity.portfolio(),
			MV=min.var.portfolio
		),
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
 
#*****************************************************************
# Adjust portfolio leverage to given target volatility
#' @export 
#****************************************************************** 				
target.vol.strategy <- function(model, weight, 
	target = 10/100, 
	lookback.len = 21,
	max.portfolio.leverage = 100/100,
	annual.periods = 252
) 
{	
	ret = diff(log(model$equity))
	hist.vol.model = sqrt(annual.periods) * runSD(ret, n = lookback.len)	
		hist.vol.model = as.vector(hist.vol.model)
		
	weight.target = weight * (target / hist.vol.model)
	
	# limit total leverage		
	rs = rowSums(abs(weight.target), na.rm=T)
	weight.target = weight.target / iif(rs > max.portfolio.leverage, rs/max.portfolio.leverage, 1)		
		
	return(weight.target)	
}


#*****************************************************************
# Calendar Strategy
#*****************************************************************
	#signals = calendar.signal(key.date, 0, 1, 2, -1, -2)
	#names(signals)
	#signals = calendar.signal(key.date, T0=0, 1, 2, N1=-1, -2,P2N2=-2:2)
	#names(signals)	
	# advanced ... - offsets
	#' @export 
	calendar.signal <- function(key.date, ...) {
		offsets = list( ... )
		if( is.list(offsets[[1]]) ) offsets = offsets[[1]]
		else {
			default.names = as.character(substitute(c(...))[-1])
			default.names = paste0('L(', default.names, ')')
			
			# replace missing names for offsets
			if(is.null(names(offsets))) names(offsets) = default.names
			else names(offsets) = iif(nchar(names(offsets))==0, default.names, names(offsets))
		}

		signals = list()
		for(n in names(offsets)) {
			offset = offsets[[n]]
			signal = mlag(key.date, offset[1])
			for(i in offset) signal = signal | mlag(key.date, i)
			signals[[n]] = signal
		}
		signals
	}

	#models = calendar.strategy(data, signals, universe = universe)
	#names(models)	
	#models = calendar.strategy(data, A=signals[[1]], signals[[1]])
	#names(models)	
	# advanced ... - signals
	#' @export 
	calendar.strategy <- function(data, ..., universe = data$prices > 0, do.lag.universe = 1, commission = 0) {
		signals = list( ... )		
		if( is.list(signals[[1]]) ) signals = signals[[1]]
		else {
			default.names = as.character(substitute(c(...))[-1])
			if(is.null(names(signals))) names(signals) = default.names
			else names(signals) = iif(nchar(names(signals))==0, default.names, names(signals))
		}

		# we need to manually lag universe = prices > SMA(prices,100) because
		# it is based on our knowledge of price today and back-test
		# below is run with do.lag = 0		
		universe = mlag(universe, do.lag.universe)
		
		models = list()
		nassets = ncol(data$prices)
		for(n in names(signals)) {
			data$weight[] = NA
			temp = ifna(universe & signals[[n]], F)
				if(nassets == 1)
					data$weight[] = temp
				else
					data$weight[] = ifna(temp / rowSums(temp),0)
					#data$weight[] = ntop(iif(temp,1,NA), nassets)
			models[[n]] = bt.run.share(data, do.lag = 0, trade.summary=T, clean.signal=T, commission = commission, silent=T)  	
		}
		models
	}	
	
	#*****************************************************************
	# Look at trades
	#' @export 
	#*****************************************************************
	last.trades <- function(..., n=20, make.plot=T, return.table=F, smain = NULL) {
		models = variable.number.arguments( ... )
		model = models[[1]]
		name=ifnull(names(models),NULL)[1]
	
		if(!is.null(model$trade.summary)) {		
			ntrades = min(n, nrow(model$trade.summary$trades))		
			trades = last(model$trade.summary$trades, ntrades)
			if(!is.null(smain) || !is.null(name)) colnames(trades)[1] = iif(is.null(smain),name,smain)
			if(make.plot) {
				layout(1)
				plot.table(trades)
			}	
			if(return.table) trades	
		}
	}	

	#*****************************************************************
	# Look at signals
	#' @export 
	#*****************************************************************	
	last.signals <- function(..., n=20, make.plot=T, return.table=F, smain = NULL) {
		models = variable.number.arguments( ... )
		model = models[[1]]
		name=ifnull(names(models),NULL)[1]
	
		if(!is.null(model$period.weight)) {			
	      	data = round(100*model$period.weight,0)
	      	ntrades = min(n, nrow(data))		
	      	trades = last(data, ntrades)
	      	
	      	if(!is.null(smain) || !is.null(name)) 
	      		smain = iif(is.null(smain),name,smain)
	      	else
	      		smain = 'Date'
	      		
	      	#colnames(trades)[1] = 'Date'
	      	#if(!is.null(smain) || !is.null(name)) colnames(trades)[1] = iif(is.null(smain),name,smain)
	      	
	      	if(make.plot) {
	        	layout(1)
	        	plot.table(as.matrix(trades))
	      	}	
	      	if(return.table) trades	
      	}
}		