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
performance.barchart.helper <- function(out, names, custom.order, nplots.page = len(spl(names)), nc.plot = 2) 
{
	# Bar chart
	layout(mat=matrix(1:(nplots.page + nplots.page %% 2), nc=nc.plot, byrow=FALSE))
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
strategy.performance.snapshoot <- function(models, one.page = F, title = NULL, data = NULL) {
	#*****************************************************************
	# Create Report
	#****************************************************************** 					
	layout(1:2)
	plotbt(models, plotX = T, log = 'y', LeftMargin = 3, main = title)	    	
		mtext('Cumulative Performance', side = 2, line = 1)
		
	out = plotbt.strategy.sidebyside(models, return.table=T)
	
	if(one.page) return()
	
	# Portfolio Turnover
	if(!is.null(data)) {
		y = 100 * sapply(models, compute.turnover, data)
			out = rbind(y, out)
			rownames(out)[1] = 'Turnover'		
		performance.barchart.helper(out, 'Sharpe,Cagr,DVR,MaxDD,Volatility,Turnover', c(T,T,T,T,F,F))
	} else		
		performance.barchart.helper(out, 'Sharpe,Cagr,DVR,MaxDD', c(T,T,T,T))

	# Plot transition maps
	layout(1:len(models))
	for(m in names(models)) {
		plotbt.transition.map(models[[m]]$weight, name=m)
			legend('topright', legend = m, bty = 'n')
	}
	
	# Plot monthly retunrs tables
	layout(1:4)
	for(n in names(models))
		plotbt.monthly.table(models[[n]]$equity, smain=n)			
	
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
create.ia <- function(hist.returns, index=1:ncol(hist.returns))
{	
	# setup input assumptions
	ia = list()	
		ia$hist.returns = hist.returns
		ia$n = ncol(ia$hist.returns)
		ia$index = index
		ia$symbols = colnames(ia$hist.returns)
		
		ia$risk = apply(ia$hist.returns, 2, sd, na.rm = T)
		ia$correlation = cor(ia$hist.returns, use='complete.obs',method='pearson')
		ia$cov = ia$correlation * (ia$risk %*% t(ia$risk))
		
	ia$expected.return = apply(ia$hist.returns, 2, mean, na.rm = T)
							
	return(ia)
}

update.ia <- function(ia, name, cov.shrink)
{
	if(name != 'sample.shrinkage') {
		ia$cov = cov.shrink
			s0 = 1 / sqrt(diag(ia$cov))
		ia$correlation = ia$cov * (s0 %*% t(s0))
	}
	ia
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
	risk.parity.portfolio <- function
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
	
	#' @export 	
	min.var.portfolio <- function
	(
		ia,				# input assumptions
		constraints,	# constraints
		cov.matrix = ia$cov
	)
	{
		risk.index = get.risky.asset.index(ia)
	
		# first try to solve QP with given Dmat
		Dmat = cov.matrix[risk.index, risk.index]		
		sol = try(solve.QP(Dmat=Dmat, 
						dvec=rep(0, sum(risk.index)), 
						Amat=constraints$A[risk.index,,drop=F], 
						bvec=constraints$b, 
						meq=constraints$meq), silent = TRUE)
						
		# if error, adjust Dmat to be positive definite
		if(inherits(sol, 'try-error'))
			sol = solve.QP(Dmat=make.positive.definite(Dmat, 0.000000001), 
						dvec=rep(0, sum(risk.index)), 
						Amat=constraints$A[risk.index,,drop=F], 
						bvec=constraints$b, 
						meq=constraints$meq)
						
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
		if( all(excess.return < 0) )
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

	# 0 <= x.i <= 1
	constraints = new.constraints(n, lb = 0, ub = 1)
		constraints = add.constraints(diag(n), type='>=', b=0, constraints)
		constraints = add.constraints(diag(n), type='<=', b=1, constraints)
		
	# SUM x.i = 1
	constraints = add.constraints(rep(1, n), 1, type = '=', constraints)		
	
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

find.portfolio.given.risk.test <- function() 
{
	#*****************************************************************
	# Create Efficient Frontier
	#****************************************************************** 	
	# create sample historical input assumptions
	ia = aa.test.create.ia()
	
	# create long-only, fully invested efficient frontier
	n = ia$n		

	# 0 <= x.i <= 1
	constraints = new.constraints(n, lb = 0, ub = 1)
		constraints = add.constraints(diag(n), type='>=', b=0, constraints)
		constraints = add.constraints(diag(n), type='<=', b=1, constraints)
		
	# SUM x.i = 1
	constraints = add.constraints(rep(1, n), 1, type = '=', constraints)		

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
	
	
	weight = given.return.portfolio(ia,constraints, 12/100)	
		weight
cat(round(100*c(portfolio.return(weight,ia), portfolio.risk(weight,ia), portfolio.return(weight,ia) /  portfolio.risk(weight,ia)),2), '\n')

	weight = given.risk.portfolio(ia,constraints, 10/100, silent=F)	
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


	# 0 <= x.i <= 1
	constraints = new.constraints(ia$n, lb = 0, ub = 1)
		constraints = add.constraints(diag(ia$n), type='>=', b = 0, constraints)
		constraints = add.constraints(diag(ia$n), type='<=', b = 1, constraints)
	
	# SUM x.i = 1
	constraints = add.constraints(rep(1, ia$n), type = '=', b = 1, constraints)
								
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
	

	# 0 <= x.i <= 1
	constraints = new.constraints(ia$n, lb = 0, ub = 1)
		constraints = add.constraints(diag(ia$n), type='>=', b = 0, constraints)
		constraints = add.constraints(diag(ia$n), type='<=', b = 1, constraints)
	
	# SUM x.i = 1
	constraints = add.constraints(rep(1, ia$n), type = '=', b = 1, constraints)
								
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
		avg.rho = (sum(sample / cov.mat)-n)/(n*(n-1))
		
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
	group = group
	function
	(
		ia			# input assumptions
	)
	{
		return(group[ia$index])	
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

	###############################################################################
	# Distribute Weights according to Weighting Scheme and Group Method(clusters)
	###############################################################################
	# min.risk.fns = list(G0.MV = distribute.weights(min.risk.portfolio, empty.group),					
	# 					G2.MV = distribute.weights(min.risk.portfolio, cluster.group))
	#' @export 	
	distribute.weights <- function
	(
		fn,				# function that dictates how to distribute weights
		group.fn = NA		# group factor
	)
	{
		fn = match.fun(fn)
		if(!is.function(group.fn)) if(!is.na(group.fn)) group.fn = match.fun(group.fn)
	
		function
		(
			ia,			# input assumptions
			constraints	# constraints
		)
		{
		
			if(!is.function(group.fn)) {
				return(fn(ia, constraints))
			} else {
				group = as.numeric(group.fn(ia))
				
				ngroups = max(group)
				if(ngroups == 1) return(fn(ia, constraints))
	
				
				weight0 = rep(NA, ia$n)
				
				# returns for each group			
				hist.g = NA * ia$hist.returns[,1:ngroups]
				
				# compute weights within each group	
				for(g in 1:ngroups) {
					if( sum(group == g) == 1 ) {
						weight0[group == g] = 1
						hist.g[,g] = ia$hist.returns[, group == g, drop=F]
					} else {
		
					ia.temp = create.ia(ia$hist.returns[, group == g, drop=F])
													
					constraints.temp = new.constraints(ia.temp$n, lb = 0, ub = 1)
						constraints.temp = add.constraints(diag(ia.temp$n), type='>=', b=0, constraints.temp)
						constraints.temp = add.constraints(diag(ia.temp$n), type='<=', b=1, constraints.temp)
					constraints.temp = add.constraints(rep(1, ia.temp$n), 1, type = '=', constraints.temp)
					
	
					w0 = match.fun(fn)(ia.temp, constraints.temp)
						weight0[group == g] = w0
						# Note that: sd(return0) = portfolio.risk(weight0, ia)	
						# return0 = ia$hist.returns	%*% weight0
						hist.g[,g] = ia.temp$hist.returns %*% w0
					}
				}
				
				# create GROUP input assumptions
				ia.g = create.ia(hist.g)
											
				constraints.g = new.constraints(ngroups, lb = 0, ub = 1)
					constraints.g = add.constraints(diag(ngroups), type='>=', b=0, constraints.g)
					constraints.g = add.constraints(diag(ngroups), type='<=', b=1, constraints.g)	
				constraints.g = add.constraints(rep(1, ngroups), 1, type = '=', constraints.g)		
				
				# find group weights
				group.weights = match.fun(fn)(ia.g, constraints.g)
					
						
					
				# mutliply out group.weights by within group weights
				for(g in 1:ngroups) {
					weight0[group == g] = weight0[group == g] * group.weights[g]
				}
				return(weight0)
			}
		}
	}
	
	#*****************************************************************
	# Maps for portfolio optimization and clustering functions
	#*****************************************************************
	#' @export 	
	get.algo <- function(algo.name, has.param = F) {
		algo.map = list(
			'cluster' = distribute.weights,
			'max.sharpe' = max.sharpe.portfolio()
		)
	
		if(any(names(algo.map) == algo.name))
			algo.map[[ algo.name ]]
		else {
			if(has.param)
				match.fun(algo.name)					
			else
				match.fun(paste(algo.name, '.portfolio', sep=''))					
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
		#strategys = spl(";,EW,Equal.Weight,;,MCE,Min.Corr.Excel,1;,MC,Min.Corr,1;,MC2,Min.Corr2,1;,C.EW,Cluster,kmeans90:min.var;,Empty,Cluster,hclust:min.corr2",';')		
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
					min.risk.fns[[ f.name ]] = get.algo(temp[1])( get.algo(params[2]), get.group(params[1]) )								
				} else
					min.risk.fns[[ f.name ]] = get.algo(temp[1],T)(temp[-1])
			}
		}					
		min.risk.fns	
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
	prefix = '',
	
	universe = prices[period.ends,]>0,
	
	min.risk.fns = 'min.var.portfolio',	# portfolio construction functions
	custom.stats.fn = NULL,
	shrinkage.fns = 'sample.shrinkage',	# covariance Shrinkage Estimator functions
	
	create.ia.fn = create.ia,
	update.ia.fn = update.ia,
	
	adjust2positive.definite = T,
	silent = F,
	
	log = log.fn(),	
	
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
	
	prices = coredata(prices)
	ret = prices / mlag(prices) - 1
	
	start.i = which(period.ends >= (lookback.len + 1))[1]

	weight = NA * prices[period.ends,]
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
				hist.all = ret[ 1:i, index]		
	
		
				# 0 <= x.i <= 1
				constraints = new.constraints(n, lb = const.lb[index], ub = const.ub[index])
					constraints = add.constraints(diag(n), type='>=', b=const.lb[index], constraints)
					constraints = add.constraints(diag(n), type='<=', b=const.ub[index], constraints)
	
				# SUM x.i = 1
				if(!is.na(const.sum))
					constraints = add.constraints(rep(1, n), type = '=', b=const.sum, constraints)
							
				# create historical input assumptions
				ia.base = create.ia.fn(hist, index)
				
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
						constraints$x0 = weights[[ fname ]][(j-1),index]			
						weights[[ fname ]][j,index] = min.risk.fns[[f]](ia, constraints)
					}
				}							
			} else {
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
			   			if(len(temp[[ ci ]]) > 1)
			   				custom[[ ci ]][[ w ]][j, index] = temp[[ ci ]]
			   			else
			   				custom[[ ci ]][j, w] = temp[[ ci ]]
			   		}
				}
		   	} 		
			
		}
			
		if( j %% 10 == 0) if(!silent) log(j, percent = (j - start.i) / (len(period.ends) - start.i))
		
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
gx <<- x
gia <<- ia
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
	
				# 0 <= x.i <= 1
				constraints = new.constraints(n, lb = 0, ub = 1)
					constraints = add.constraints(diag(n), type='>=', b=0, constraints)
					constraints = add.constraints(diag(n), type='<=', b=1, constraints)
	
				# SUM x.i = 1
				constraints = add.constraints(rep(1, n), type = '=', b=1, constraints)
							
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

		models[[i]] = bt.run.share(data, clean.signal = clean.signal, ...)
		
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
 
#*****************************************************************
# Adjust portfolio leverage to given target volatility
#' @export 
#****************************************************************** 				
target.vol.strategy <- function(model, weight, 
	target = 10/100, 
	lookback.len = 21,
	max.portfolio.leverage = 100/100) 
{	
	ret.log.model = ROC(model$equity, type='continuous')
	hist.vol.model = sqrt(252) * runSD(ret.log.model, n = lookback.len)	
		hist.vol.model = as.vector(hist.vol.model)
		
	weight.target = weight * (target / hist.vol.model)
	
	# limit total leverage		
	rs = rowSums(abs(weight.target))
	weight.target = weight.target / iif(rs > max.portfolio.leverage, rs/max.portfolio.leverage, 1)		
		
	return(weight.target)	
}
