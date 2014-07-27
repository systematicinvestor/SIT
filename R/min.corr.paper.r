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
# Examples for the Minimum Correlation Algorithm paper
# Copyright (C) 2012  Michael Kapler
#
# Forecast-Free Algorithms: A New Benchmark For Tactical Strategies
# http://cssanalytics.wordpress.com/2011/08/09/forecast-free-algorithms-a-new-benchmark-for-tactical-strategies/
#
# For more information please visit my blog at www.SystematicInvestor.wordpress.com
# or drop me a line at TheSystematicInvestor at gmail
###############################################################################






###############################################################################
# Main program to run all examples
#
# Please Load Systematic Investor Toolbox prior to running this routine
###############################################################################
# Load Systematic Investor Toolbox (SIT)
# http://systematicinvestor.wordpress.com/systematic-investor-toolbox/
###############################################################################
#setInternet2(TRUE)
#con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
#    source(con)
#close(con)
###############################################################################
min.corr.paper.examples <- function() 
{
#*****************************************************************
# Load historical data sets
#****************************************************************** 
	load.packages('quantmod')	
	
	#*****************************************************************
	# Load historical data for Futures and Forex
	#****************************************************************** 
	data <- new.env()
	getSymbols.TB(env = data, auto.assign = T, download = T)
		
	bt.prep(data, align='remove.na', dates='1990::')
	save(data,file='FuturesForex.Rdata')
	#load(file='FuturesForex.Rdata')


	#*****************************************************************
	# Load historical data for ETFs
	#****************************************************************** 
	tickers = spl('SPY,QQQ,EEM,IWM,EFA,TLT,IYR,GLD')

	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)							
	# TLT first date is 7/31/2002
	bt.prep(data, align='keep.all', dates='2002:08::')
	save(data,file='ETF.Rdata')
	#load(file='ETF.Rdata')

	
	#*****************************************************************
	# Load historical data for dow stock (engle)
	#****************************************************************** 
	load.packages('quantmod,quadprog')
	tickers = spl('AA,AXP,BA,CAT,DD,DIS,GE,IBM,IP,JNJ,JPM,KO,MCD,MMM,MO,MRK,MSFT')
		
	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)							
		
	bt.prep(data, align='keep.all', dates='1980::')
	
	# backfill prices
	prices = coredata(data$prices)
		prices[is.na(prices)] = mlag(prices)[is.na(prices)]
		prices[is.na(prices)] = mlag(prices)[is.na(prices)]
	data$prices[] = prices
		
	save(data,file='Dow.Engle.Rdata')
	#load(file='Dow.Engle.Rdata')


	#*****************************************************************
	# Load historical data for ETFs
	#****************************************************************** 
	load.packages('quantmod,quadprog')
	tickers = spl('VTI,IEV,EEM,EWJ,AGG,GSG,GLD,ICF')
		
	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)							
		
	bt.prep(data, align='keep.all', dates='2003:10::')	
	save(data,file='ETF2.Rdata')
	#load(file='ETF2.Rdata')

	
	#*****************************************************************
	# Load historical data for nasdaq 100 stocks
	#****************************************************************** 
	load.packages('quantmod,quadprog')
	#tickers = nasdaq.100.components()
	tickers = spl('ATVI,ADBE,ALTR,AMZN,AMGN,APOL,AAPL,AMAT,ADSK,ADP,BBBY,BIIB,BMC,BRCM,CHRW,CA,CELG,CERN,CHKP,CTAS,CSCO,CTXS,CTSH,CMCSA,COST,DELL,XRAY,DISH,EBAY,EA,EXPD,ESRX,FAST,FISV,FLEX,FLIR,FWLT,GILD,HSIC,HOLX,INFY,INTC,INTU,JBHT,KLAC,LRCX,LIFE,LLTC,LOGI,MAT,MXIM,MCHP,MSFT,MYL,NTAP,NWSA,NVDA,ORLY,ORCL,PCAR,PDCO,PAYX,PCLN,QGEN,QCOM,BBRY,ROST,SNDK,SIAL,SPLS,SBUX,SRCL,SYMC,TEVA,URBN,VRSN,VRTX,VOD,XLNX,YHOO')
		
	data <- new.env()
	for(i in tickers) {
		try(getSymbols(i, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T), TRUE)
		data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)							
	}
	bt.prep(data, align='keep.all', dates='1995::')
	
	# backfill prices
	prices = coredata(data$prices)
		prices[is.na(prices)] = mlag(prices)[is.na(prices)]
		prices[is.na(prices)] = mlag(prices)[is.na(prices)]
	data$prices[] = prices
		
	# plot
	#plota(make.xts(count(t(data$prices)),index(data$prices)),type='l')
		
	save(data,file='nasdaq.100.Rdata')
	#load(file='nasdaq.100.Rdata')


	
	
#*****************************************************************
# Run all strategies
#****************************************************************** 
	names = spl('ETF,FuturesForex,Dow.Engle,ETF2,nasdaq.100')	
	lookback.len = 60
	periodicitys = spl('weeks,months')
	periodicity = periodicitys[1]
	prefix = paste(substr(periodicity,1,1), '.', sep='')
	
	
	
	for(name in names) {
		load(file = paste(name, '.Rdata', sep=''))
		
		obj = portfolio.allocation.helper(data$prices, periodicity, lookback.len = lookback.len, prefix = prefix,
			min.risk.fns = 'min.corr.portfolio,min.corr2.portfolio,max.div.portfolio,min.var.portfolio,risk.parity.portfolio(),equal.weight.portfolio',
			custom.stats.fn = 'portfolio.allocation.custom.stats')		
			
		save(obj, file=paste(name, lookback.len, periodicity, '.bt', '.Rdata', sep=''))
	}
		
	
	#*****************************************************************
	# Create Reports
	#****************************************************************** 
	for(name in names) {
		load(file=paste(name, '.Rdata', sep=''))
		
		# create summary of inputs report
		custom.input.report.helper(paste('report.', name, sep=''), data)
		
		# create summary of strategies report
		load(file=paste(name, lookback.len, periodicity, '.bt', '.Rdata', sep=''))
		custom.report.helper(paste('report.', name, lookback.len, periodicity, sep=''), 
			create.strategies(obj, data))	
	}

	
	#*****************************************************************
	# Futures and Forex: rescale strategies to match Equal Weight strategy risk profile
	#****************************************************************** 
	names = spl('FuturesForex')	
	for(name in names) {
		load(file=paste(name, '.Rdata', sep=''))

		# create summary of strategies report
		load(file=paste(name, lookback.len, periodicity, '.bt', '.Rdata', sep=''))
			leverage = c(5, 4, 15, 20, 3, 1)
		custom.report.helper(paste('report.leverage.', name, lookback.len, periodicity, sep=''), 
			create.strategies(obj, data, leverage))	
	}

}



###############################################################################
# Custom Report routines
###############################################################################

#*****************************************************************
# Create summary of inputs report
#*****************************************************************
custom.input.report.helper <- function(filename, data) {
	filename.pdf = paste(filename, '.pdf', sep='')
	filename.csv = paste(filename, '.csv', sep='')

	#*****************************************************************
	# Create Report
	#****************************************************************** 
	# put all reports into one pdf file
	pdf(file = filename.pdf, width=8.5, height=11)
	

	# Input Details
	layout(1:2)
	asset.models = list()
	for(i in data$symbolnames) {
		data$weight[] = NA
			data$weight[,i] = 1	
		asset.models[[i]] = bt.run(data, silent=T)
	}		
	asset.summary = plotbt.strategy.sidebyside(asset.models, return.table=T)

		
	# plot correlations
	ret.log = bt.apply.matrix(data$prices, ROC, type='continuous')
	temp = cor(ret.log, use='complete.obs', method='pearson')
			temp[] = plota.format(100 * temp, 0, '', '%')
	plot.table(temp, smain='Correlation', highlight = TRUE, colorbar = TRUE)	


	# line plot for each input series
	layout(matrix(1:4,2,2))	
	if( is.null(data$symbol.groups) ) {
		index = order(data$symbolnames)
		for(i in data$symbolnames[index]) 
			plota(data[[i]], type='l', cex.main=0.7,main= i)
	} else {
		index = order(data$symbol.groups)
		for(i in data$symbolnames[index]) 
			plota(data[[i]], type='l', cex.main=0.7, main= paste(i, data$symbol.groups[i], data$symbol.descriptions.print[i], sep=' / ') )
		
		asset.summary = rbind(data$symbol.groups, data$symbol.descriptions.print, asset.summary)
	}
	
	
	# close pdf
	dev.off()	

	#*****************************************************************
	# save summary & equity curves into csv file
	#****************************************************************** 
	load.packages('abind')
	write.csv(asset.summary, filename.csv)
	cat('\n\n', file=filename.csv, append=TRUE)
	
    write.table(temp, sep=',',  row.names = , col.names = NA,
		file=filename.csv, append=TRUE)
		
}


#*****************************************************************
# Create summary of strategies report
#*****************************************************************
custom.report.helper <- function(filename, obj) {
	filename.pdf = paste(filename, '.pdf', sep='')
	filename.csv = paste(filename, '.csv', sep='')

	models = obj$models
		
	#*****************************************************************
	# Create Report
	#****************************************************************** 
	# put all reports into one pdf file
	pdf(file = filename.pdf, width=8.5, height=11)

	
	# Plot perfromance
	plotbt(models, plotX = T, log = 'y', LeftMargin = 3)	    	
		mtext('Cumulative Performance', side = 2, line = 1)

		
	# Plot Strategy Statistics  Side by Side
	out = plotbt.strategy.sidebyside(models, perfromance.fn = 'custom.returns.kpi', return.table=T)

	
	
	# Plot time series of components of Composite Diversification Indicator
	cdi = custom.composite.diversification.indicator(obj)	
		out = rbind(colMeans(cdi, na.rm=T), out)
		rownames(out)[1] = 'Composite Diversification Indicator(CDI)'
			
	# Portfolio Turnover for each strategy
	y = 100 * sapply(models, compute.turnover, data)
		out = rbind(y, out)
		rownames(out)[1] = 'Portfolio Turnover'		
	
	# Bar chart in descending order of the best algo by Sharpe Ratio, CAGR, Gini, Herfindahl	
	performance.barchart.helper(out, 'Sharpe,Cagr,RC Gini,RC Herfindahl,Volatility,Portfolio Turnover,Composite Diversification Indicator(CDI)', c(T,T,F,F,F,F,T))

		
	# summary allocation statistics for each model	
	custom.summary.positions(obj$weights)
	
	
	# monhtly returns for each model	
	custom.period.chart(models)

		
	# Plot transition maps
	layout(1:len(models))
	for(m in names(models)) {
		plotbt.transition.map(models[[m]]$weight, name=m)
			legend('topright', legend = m, bty = 'n')
	}
	
	
	# Plot transition maps for Risk Contributions
	dates = index(models[[1]]$weight)[obj$period.ends]
	layout(1:len(models))
	for(m in names(models)) {
		plotbt.transition.map(make.xts(obj$risk.contributions[[m]], dates), 
		name=paste('Risk Contributions',m))
			legend('topright', legend = m, bty = 'n')
	}

	# close pdf
	dev.off()	

	
	#*****************************************************************
	# save summary & equity curves into csv file
	#****************************************************************** 
	load.packages('abind')
	write.csv(out, filename.csv)
	cat('\n\n', file=filename.csv, append=TRUE)
	
	out = abind(lapply(models, function(m) m$equity))
		colnames(out) = names(models)
	write.xts(make.xts(out, index(models[[1]]$equity)), filename.csv, append=TRUE)		
}



#*****************************************************************
# Composite Diversification Indicator (CDI) is 50/50 of
# * 1 - Gini(portfolio risk contribution weights) and  
# * Minimum Average Correlation (from max.div) / Average Portfolio Correlation (w * Correlation Matrix * w) 			
#' @export 
#*****************************************************************
custom.composite.diversification.indicator <- function
(
	obj,	# portfolio.backtest object
	avg.flag = T,
	avg.len = 10,
	plot.main = T,
	plot.table = T
) 
{
	cdi = 0.5 * obj$risk.gini + 0.5 * obj$degree.diversification
		if(avg.flag) cdi = bt.apply.matrix(cdi, EMA, avg.len)

	if(plot.main) {	
		avg.name = iif(avg.flag, paste(avg.len,'period EMA') , '')

		layout(1:3)
		out = obj$degree.diversification
			if(avg.flag) out = bt.apply.matrix(out, EMA, avg.len)
		plota.matplot(out, cex.main = 1, 
			main=paste('D = 1 - Portfolio Risk/Weighted Average of asset vols in the portfolio', avg.name))
					
		out = obj$risk.gini
			if(avg.flag) out = bt.apply.matrix(out, EMA, avg.len)
		plota.matplot(out, cex.main = 1, 
			main=paste('1 - Gini(Risk Contributions)', avg.name))
			
		plota.matplot(cdi, cex.main = 1, 
			main=paste('Composite Diversification Indicator (CDI) = 50/50 Gini/D', avg.name))
	}


	# create sensitivity plot		
	if(plot.table) {	
		weights = seq(0,1,0.1)
		# create temp matrix with data you want to plot
		temp = matrix(NA, nc=len(weights), nr=ncol(cdi))
			colnames(temp) = paste(weights)
			rownames(temp) = colnames(cdi)
			
		for(j in 1:len(weights)) {
			i = weights[j]
			temp[,j] = rank(-colMeans((1-i) * obj$risk.gini + i * obj$degree.diversification, na.rm=T))
		}
		temp = cbind(temp, round(rowMeans(temp),1))
			colnames(temp)[ncol(temp)] = 'AVG'
		
		# highlight each column separately
		highlight = apply(temp,2, function(x) plot.table.helper.color(t(x)) )
		
		# plot temp with colorbar
		layout(1)
		plot.table(temp, smain = 'CDI Rank\nAlgo vs %D',highlight = highlight, colorbar = TRUE)
	}	

	return(cdi)
} 
			


#*****************************************************************
# Summary Positions for each model
#*****************************************************************
custom.summary.positions <- function(weights) {
	layout(1:len(weights))
	
	for(w in names(weights)) {
		tickers = colnames(weights[[w]])
		n = len(tickers)
		
		temp = matrix(NA, nr=4, nc=n)
			colnames(temp) = tickers
			rownames(temp) = spl('Avg Pos,Max Pos,Min Pos,# Periods')
			
		temp['Avg Pos',] = 100 * apply(weights[[w]],2,mean,na.rm=T)
		temp['Max Pos',] = 100 * apply(weights[[w]],2,max,na.rm=T)
		temp['Min Pos',] = 100 * apply(weights[[w]],2,min,na.rm=T)
		temp['# Periods',] = apply(weights[[w]] > 1/1000,2,sum,na.rm=T)
		
		temp[] = plota.format(temp, 0, '', '')
		plot.table(temp, smain=w)
	}
}	


#*****************************************************************
# Helper function to create barplot
#*****************************************************************
custom.profit.chart <- function(data, main, cols) {
	par(mar=c(4, 3, 2, 2),cex.main=2,cex.sub=1, cex.axis=1.5,cex.lab=1.5)	
	
	barplot(data, names.arg = names(data),
		col=iif(data > 0, cols[1], cols[2]), 
		main=main, 
		cex.names = 1.5, border = 'darkgray',las=2) 
	grid(NA,NULL) 
	abline(h=0,col='black')
	abline(h=mean(data),col='gray',lty='dashed',lwd=3)
}


#*****************************************************************
# Monhtly returns for each model	
#*****************************************************************
custom.period.chart <- function(models) {
	for(imodel in 1:len(models)) {
		equity = models[[imodel]]$equity
	
		#*****************************************************************
		# Compute monthly returns
		#****************************************************************** 
		period.ends = endpoints(equity, 'months')
			period.ends = unique(c(1, period.ends[period.ends > 0]))
	
		ret = equity[period.ends,] / mlag(equity[period.ends,]) - 1
			ret = ret[-1]
	
		ret.by.month = create.monthly.table(ret)	
		ret.by.month = 100 * apply(ret.by.month, 2, mean, na.rm=T)
		
		#*****************************************************************
		# Compute annual returns
		#****************************************************************** 
		period.ends = endpoints(equity, 'years')
			period.ends = unique(c(1, period.ends[period.ends > 0]))
	
		ret = equity[period.ends,] / mlag(equity[period.ends,]) - 1
		ret.by.year = ret[-1]
			
		#*****************************************************************
		# Create plots
		#****************************************************************** 
		# create layout	
		ilayout = 
			'1,1
			2,2
			2,2
			2,2
			2,2
			2,2
			3,4
			3,4
			3,4
			3,4
			3,4
			3,4'
		plota.layout(ilayout)
		
		# make dummy table with name of strategy		
		make.table(1,1)
		a = matrix(names(models)[imodel],1,1)
		cex = plot.table.helper.auto.adjust.cex(a)
		draw.cell(a[1],1,1, text.cex=cex,frame.cell=F)		
		
		# plots
		temp = plotbt.monthly.table(equity)	
	
		# plot months
		cols = spl('green,red')
		custom.profit.chart(ret.by.month, 'Average Monthly Returns', cols)
		
		# plot years
		ret = 100*as.vector(ret.by.year)
			names(ret) = date.year(index(ret.by.year))
		custom.profit.chart(ret, 'Annual Returns', cols)
	}
}	
	

#*****************************************************************
# Custom Summary function to add consentration statistics (Gini and  Herfindahl)
#
# On the properties of equally-weighted risk contributions portfolios by
# S. Maillardy, T. Roncalliz,  J. Teiletchex (2009)
# A.4 Concentration and turnover statistics, page 22
#*****************************************************************
custom.returns.kpi <- function
(
	bt,		# backtest object
	trade.summary = NULL
) 
{	
	out = list()
	w = bt$period.weight
	rc = bt$risk.contribution
	
	# Average Number of Holdings
	out[[ 'Avg #' ]] =  mean(rowSums(w > 1/1000)) / 100
	
	# Consentration stats
	out[[ 'W Gini' ]] = mean(portfolio.concentration.gini.coefficient(w), na.rm=T)
	out[[ 'W Herfindahl' ]] = mean(portfolio.concentration.herfindahl.index(w), na.rm=T) 
	
	# Consentration stats on marginal risk contributions
	out[[ 'RC Gini' ]] = mean(portfolio.concentration.gini.coefficient(rc), na.rm=T)
	out[[ 'RC Herfindahl' ]] = mean(portfolio.concentration.herfindahl.index(rc), na.rm=T) 

	out = lapply(out, function(x) if(is.double(x)) round(100*x,1) else x)
	out = c(bt.detail.summary(bt)$System, out)
	
	return( list(System=out))
}

			
###############################################################################
# "Let us skip now to the general case. If we sum up the situations from the point
# of view of mathematical de nitions of these portfolios, they are as follows (where
# we use the fact that MV portfolios are equalizing marginal contributions to risk; see
# Scherer, 2007b)"  (this is from the Roncalli Paper)
#
# "All stocks belonging to the MDP have the same correlation to it" (Choeifaty: Properties of the Most Diversified Portfolio)
#
# The marginals for the minimum variance and maximum diversification portfolios 
# and the property of equal marginals ONLY holds is we do not impose long-only constraints. 
#
# If we only have a budget constraint (i.e. sum of portfolio weights = 100%)
# min.var weights =  | 2.05, -0.57,  -0.48 |
# marginal risk contributions = | 0.01468981, 0.01468981, 0.01468981 |
#
# max.div weights =  | -0.26,  0.65,  0.61 |
# marginal correlation contributions = |  0.8434783, 0.8434783, 0.8434783 |
# marginal contributions are the same
#
# Now if we add long only constraint (i.e. all weights >= 0 and sum of portfolio weights = 100%)
# min.var weights =  | 1, 0,  0 |
# marginal risk contributions = | 0.0196, 0.02268, 0.02618 |
# 
# max.div weights =  | 0,  0.5,  0.5 |
# marginal correlation contributions = |   0.875, 0.85, 0.85 |
# marginal contributions are the different
###############################################################################
#
# Numerical examples used in the Minimum Correlation Algorithm papaer
#
###############################################################################
min.corr.paper.numerical.examples <- function() 
{
	#*****************************************************************
	# create input assumptions
	#*****************************************************************
		n = 3
		ia = list()
			ia$n = 3
			ia$risk = c(14, 18, 22) / 100;
			ia$correlation = matrix(
				c(1, 0.90, 0.85,
				0.90, 1, 0.70,
				0.85, 0.70, 1), nr=3, byrow=T)
			ia$cov = ia$correlation * (ia$risk %*% t(ia$risk))

	#*****************************************************************
	# create constraints
	#*****************************************************************
		constraints = new.constraints(n)
		# 0 <= x.i <= 1
		constraints = new.constraints(n, lb = 0, ub = 1)
			constraints = add.constraints(diag(n), type='>=', b=0, constraints)
			constraints = add.constraints(diag(n), type='<=', b=1, constraints)

		# SUM x.i = 1
		constraints = add.constraints(rep(1, n), 1, type = '=', constraints)		
				
	#*****************************************************************
	# Minimum Variance Portfolio 
	#*****************************************************************
		x = min.var.portfolio(ia, constraints)
		
		sol = solve.QP(Dmat=ia$cov, dvec=rep(0, ia$n), 
			Amat=constraints$A, bvec=constraints$b, meq=constraints$meq)
		x = sol$solution
		
			round(x,4)
			sqrt(x %*% ia$cov %*% x)
			
		# marginal contributions	
		x %*% ia$cov
		
	#*****************************************************************
	# Maximum Diversification Portfolio 
	#*****************************************************************			
		sol = solve.QP(Dmat=ia$correlation, dvec=rep(0, ia$n), 
			Amat=constraints$A, bvec=constraints$b, meq=constraints$meq)
		x = sol$solution
			round(x,4)
			
		# marginal contributions
		x %*% ia$correlation
		
		
		# re-scale and normalize weights to sum up to 1
		x = x / sqrt( diag(ia$cov) )
		x = x / sum(x)
			round(x,4)
			sqrt(x %*% ia$cov %*% x)
			
	#*****************************************************************
	# Minimum Correlation Portfolio 
	#*****************************************************************						
		upper.index = upper.tri(ia$correlation)
		cor.m = ia$correlation[upper.index]
			cor.mu = mean(cor.m)
			cor.sd = sd(cor.m)
			
		norm.dist.m = 0 * ia$correlation	
			diag(norm.dist.m) = NA
			norm.dist.m[upper.index] = sapply(cor.m, function(x) 1-pnorm(x, cor.mu, cor.sd))
		norm.dist.m = (norm.dist.m + t(norm.dist.m))
		
		norm.dist.avg = apply(norm.dist.m, 1, mean, na.rm=T)
		
		norm.dist.rank = rank(-norm.dist.avg)
		
			adjust.factor = 1
		adjusted.norm.dist.rank = norm.dist.rank ^ adjust.factor
		
		norm.dist.weight = adjusted.norm.dist.rank / sum(adjusted.norm.dist.rank)
		
		weighted.norm.dist.average = norm.dist.weight %*% ifna(norm.dist.m,0)
		
		final.weight = weighted.norm.dist.average / sum(weighted.norm.dist.average)
		
		x = final.weight
		
		# re-scale and normalize weights to sum up to 1
		x = x / sqrt( diag(ia$cov) )
		x = x / sum(x)
			round(x,4)
			x = as.vector(x)
			sqrt(x %*% ia$cov %*% x)
			
	#*****************************************************************
	# Minimum Correlation 2 Portfolio 
	#*****************************************************************						
		cor.m = ia$correlation
			diag(cor.m) = 0
			
		avg = rowMeans(cor.m)
			cor.mu = mean(avg)
			cor.sd = sd(avg)
		norm.dist.avg = 1-pnorm(avg, cor.mu, cor.sd)
		
		norm.dist.rank = rank(-norm.dist.avg)
		
			adjust.factor = 1
		adjusted.norm.dist.rank = norm.dist.rank ^ adjust.factor
		
		norm.dist.weight = adjusted.norm.dist.rank / sum(adjusted.norm.dist.rank)
				
		weighted.norm.dist.average = norm.dist.weight %*% (1-cor.m)
		final.weight = weighted.norm.dist.average / sum(weighted.norm.dist.average)
		
		x = final.weight
		
		# re-scale and normalize weights to sum up to 1
		x = x / sqrt( diag(ia$cov) )
		x = x / sum(x)
			round(x,4)
			x = as.vector(x)
			sqrt(x %*% ia$cov %*% x)
					
			
		#min.corr.portfolio(ia, constraints)
		#min.corr2.portfolio(ia, constraints)					
}
	


