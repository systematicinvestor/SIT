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
# Examples for the Adaptive Shrinkage paper
# Copyright (C) 2013  Michael Kapler
#
# Adaptive Shrinkage
# http://cssanalytics.wordpress.com/2013/10/24/adaptive-shrinkage/
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


#adaptive.shrinkage.paper.backtests()


###############################################################################
# Main routine to run all backtests
###############################################################################
adaptive.shrinkage.paper.backtests <- function() 
{
	stats = list()
	for(i in 0:8) {
		temp = bt.run.data.set(i)
		stats[[temp$title]] = temp$stats
	}

	save(stats, file='stats.Rdata')
	
if(F) {	

	load(file='stats.Rdata')
	
	# rownames(stats[[1]])
	names = c("Portfolio Turnover", "Composite Diversification Indicator(CDI)", "Cagr", "Sharpe", "Volatility", "MaxDD")
	custom.order=c(F,T,T,T,F,F)
	
	names = spl('Portfolio Turnover,Sharpe,Volatility,Composite Diversification Indicator(CDI)')
	custom.order = c(T,F,T,F)
	
	out = c()
	for(n in names) {
		temp = sapply(stats, function(x) x[n,])
			temp = apply(temp, 1, as.double)
		out = rbind(out, rowMeans(apply(temp, 1, rank)))
	}
	rownames(out) = names
	
	performance.barchart.helper(out, custom.order = !custom.order,
		custom.col=c('MV.S_SA_A'='red', 'best.sharpe'='green'))	
}		
	
	
	
	bt.summary.stats(stats)
} 

adaptive.shrinkage.paper.backtests.leverage <- function() 
{
	# FuturesForex with leverage, i.e. scale all strategies to have
	# leverage of the equal weight strategy
	temp = bt.run.data.set(0)

	vol = as.double(temp$stats['Volatility',])
	leverage = vol[1] / vol
	
	temp1 = bt.run.data.set(0, leverage = leverage, title = paste(temp$title, 'leverage'))
} 
		
###############################################################################
# Shrinkage functions
###############################################################################
# Setup tawny
#load.packages('tawny')
# disable logging
#flog.threshold(0)


#cssa.average.shrinkage(gia$hist.returns)

# Average Correlation Shrinkage based on David Varadi example
# http://cssanalytics.wordpress.com/2013/10/27/average-correlation-shrinkage-acs/

cssa.average.shrinkage <- function(hist) {		 	
	n = ncol(hist)
	correlation = cor(hist, use='complete.obs', method='pearson')
	s0 = apply(hist, 2, sd, na.rm=T)
	
	index = s0==0
	if(sum(index) > 0) {
		correlation[index,] = 0
		correlation[,index] = 0	
	}	
		
	column.avg = (colSums(correlation) - 1) / (n - 1)
	new.matrix = outer(column.avg, column.avg, '+') / 2
		diag(new.matrix) = 1
		
	new.matrix * (s0 %*% t(s0))
}	
		

shrink.average.cssa <- function(s=NULL) { s=s; function(x, a) cov.shrink(x, cssa.average.shrinkage, s, 1)$sigma }



###############################################################################
# Sample Data Sets
###############################################################################
bt.load.data.set <- function(data.set = 1) 
{	
	if(data.set == 0) {
		data <- new.env()
			getSymbols.TB(env = data, auto.assign = T, download = F)		
		bt.prep(data, align='remove.na', dates='1990::')
		
		return(list(data = data, title = 'Futures Forex', tickers = data$symbolnames))
	}
		
	data.sets = list()
	data.sets[['Selected ETFs']] = list(
		tickers = spl('SPY,QQQ,EEM,IWM,EFA,TLT,IYR,GLD'),		
		align='keep.all', dates='2002:08::')
		
	data.sets[['Selected ETFs 1']] = list(
		tickers = spl('GLD,UUP,SPY,QQQ,IWM,EEM,EFA,IYR,USO,TLT'),
		align='keep.all', dates='2003:04::')
										
	data.sets[['Selected ETFs 2']] = list(
		tickers = spl('VTI,IEV,EEM,EWJ,AGG,GSG,GLD,ICF'),
		align='keep.all', dates='2003:10::')		
			
	data.sets[['Dow stock (Engle)']] = list(
		tickers = spl('AA,AXP,BA,CAT,DD,DIS,GE,IBM,IP,JNJ,JPM,KO,MCD,MMM,MO,MRK,MSFT'),
		align='keep.all', dates='1980::')
		
	data.sets[['Nasdaq 100']] = list(
		tickers = spl('ATVI,ADBE,ALTR,AMZN,AMGN,APOL,AAPL,AMAT,ADSK,ADP,BBBY,BIIB,BRCM,CHRW,CA,CELG,CEPH,CERN,CHKP,CTAS,CSCO,CTXS,CTSH,CMCSA,COST,DELL,XRAY,DISH,EBAY,EA,EXPD,ESRX,FAST,FISV,FLEX,FLIR,FWLT,GILD,HSIC,HOLX,INFY,INTC,INTU,JBHT,KLAC,LRCX,LIFE,LLTC,LOGI,MAT,MXIM,MCHP,MSFT,MYL,NTAP,NWSA,NVDA,ORLY,ORCL,PCAR,PDCO,PAYX,PCLN,QGEN,QCOM,BBRY,ROST,SNDK,SIAL,SPLS,SBUX,SRCL,SYMC,TEVA,URBN,VRSN,VRTX,VOD,XLNX,YHOO'),
		align='keep.all', dates='1995::')


		
	data.sets[['Selected ETFs 10 Major Markets']] = list(
		tickers = spl('SPY,EFA,EWJ,EEM,IYR,RWX,IEF,TLT,DBC,GLD'),
		align='keep.all', dates='2002:08::')
				
	data.sets[['Sector ETFs']] = list(
		tickers = spl('XLY,XLP,XLE,XLF,XLV,XLI,XLB,XLK,XLU'),
		align='keep.all', dates='1998:12::')		
				
	data.sets[['MSCI Country']] = list(
		tickers = spl('SPY,TUR,EIRL,THD,EWL,EWK,EWA,EWD,EWW,EWN,EWJ,EWQ,EWO,EWP,EWH,EWG,MES,EWS,EIDO,EWU,EPOL,EWM,EIS,AFK,EWI,EWT,EWC,FXI,EZA,EWY,VNM,ECH,RSX,EWZ')	,
		align='keep.all', dates='2000:08::')
						

# 	other(s), not used
#	data.sets[['Selected ETFs 3']] = list(
#		tickers = spl('DIA,EEM,EFA,EWH,EWJ,EWT,EWZ,FXI,GLD,GSG,IEF,ILF,IWM,IYR,QQQ,SPY,VNQ,XLB,XLE,XLF,XLI,XLP,XLU,XLV,XLY,XLK')	,
#		align='keep.all', dates='2000:08::')
#		
#	data.sets[['Dow Jones']] = dow.jones.components()	
#	data.sets[['SP500']] = sp500.components()$tickers 
		
		
	#*****************************************************************
	# Load historical data for ETFs
	#****************************************************************** 
	load.packages('quantmod')

	title = names(data.sets)[data.set]
	info = data.sets[[data.set]]
			
	data <- new.env()
	getSymbols(info$tickers, src = 'yahoo', from = '1900-01-01', env = data, auto.assign = T)
data.clean(data, min.ratio=3)	
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)							
	bt.prep(data, align = info$align, dates = info$dates)
	
	# backfill prices
	prices = coredata(data$prices)
		prices[is.na(prices)] = mlag(prices)[is.na(prices)]
		prices[is.na(prices)] = mlag(prices)[is.na(prices)]
	data$prices[] = prices	
	
	return(list(data = data, title = title, tickers = info$tickers))
}


###############################################################################
# Run Back-test for selected data-set
###############################################################################
bt.run.data.set <- function(i = 1, dates = '::',
	leverage = 1, 
	title = ''
) 
{
	#*****************************************************************
	# Load Data
	#****************************************************************** 	
	data.set = bt.load.data.set(i)
		data = data.set$data
		if(nchar(title) == 0) title = data.set$title

	#*****************************************************************
	# Code Strategies
	#****************************************************************** 	
	obj = portfolio.allocation.helper(data$prices, 
#		periodicity = 'weeks', lookback.len = 252,
		periodicity = 'months', lookback.len = 60,
		min.risk.fns = list(MV=min.var.portfolio),
		shrinkage.fns = list(
			S=sample.shrinkage,
			SA=sample.anchored.shrinkage,
			
			D=shrink.diag(1),					
			CC=shrink.const.cor(1),
			SI=shrink.single.index(1),
			A=shrink.average.cssa(1),			

			AVG=function(h,a) 1/4*(
				shrink.diag(1)(h,a) + shrink.const.cor(1)(h,a) + 
				shrink.single.index(1)(h,a) + shrink.average.cssa(1)(h,a)
			),
						
			S_SA_A=function(h,a) 1/3*(shrink.average.cssa(1)(h,a)
			+ sample.shrinkage(h,a) + sample.anchored.shrinkage(h,a)),			

			D_S=shrink.diag(),														
			CC_S=shrink.const.cor(),
			SI_S=shrink.single.index(),			
			A_S=shrink.average.cssa(),

			AVG_S=function(h,a) cov.shrink(h, 1/4 *(
				shrink.diag(1)(h,a) + shrink.const.cor(1)(h,a) + 
				shrink.single.index(1)(h,a) + shrink.average.cssa(1)(h,a)
			))$sigma,
			
			S_SA_A_S=function(h,a) cov.shrink(h, 1/3 *(
			shrink.average.cssa(1)(h,a) + sample.shrinkage(h,a) + sample.anchored.shrinkage(h,a)
			))$sigma

		),
		adjust2positive.definite = F,
		custom.stats.fn = 'portfolio.allocation.custom.stats'
	) 			
	

	#*****************************************************************
	# Add strategy that invests into highest sharpe strategy	
	#****************************************************************** 		
	obj = bt.shrinkage.best.sharpe('S,SA,A', 252, obj, data)
			

	models = create.strategies(obj, data, dates = dates, leverage = leverage)$models
	list(title = title, stats = bt.summary.report(models, title, data, obj,
		control = list(plot.weight.transition.maps = F,
			plot.risk.contribution.transition.maps = F)
		) 
	)
}	


###############################################################################
# Best Sharpe Idea
###############################################################################
bt.shrinkage.best.sharpe <- function(methods, sharpe.lookback.len, obj, data) 
{	
	models = create.strategies(obj, data)$models
	
	#*****************************************************************
	# Setup
	#****************************************************************** 	
	# select strategies to consider
	methods = spl(methods)
	methods = paste('MV',methods,sep='.')
		
	# get equity curves
	global.data <- new.env()
	for(i in methods) {
		temp = models[[i]]$equity
			temp[1:min(which(temp != 1))] = NA			
		global.data[[i]] = make.stock.xts(temp)
	}
	bt.prep(global.data, align='keep.all')
	
	
	#*****************************************************************
	# Strategy that invests into highest sharpe strategy	
	#****************************************************************** 		
	# compute sharpe and select best one - if we use 60 here not very good results
	rets = global.data$prices / mlag(global.data$prices) - 1
	
	sharpe = bt.apply.matrix(rets, runMean, sharpe.lookback.len) / bt.apply.matrix(rets, runSD, sharpe.lookback.len)
		sharpe = ifna(sharpe,-Inf)
		
	index.best = unlist(apply(sharpe[obj$period.ends,],1,which.max))
	
	# create strategy that allocates to the strategy with best sharpe ratio over look back period
	obj$weights$best.sharpe = obj$weights[[1]]
	for(m in 1:len(methods))
		obj$weights$best.sharpe[ index.best == m, ] = obj$weights[[ methods[m] ]][ index.best == m, ]

		
	# create benchmark strategy, equally allocate to all given strategies
	n = 1/len(methods)
	obj$weights$simple.avg = obj$weights[[1]] * 0
	for(m in 1:len(methods))
		obj$weights$simple.avg[] = obj$weights$simple.avg[] + n * obj$weights[[ methods[m] ]]
						
	return(obj);
}



   
###############################################################################
# Create Back-test report
###############################################################################
bt.summary.report <- function(models, title, data, obj=NULL,
	control = list(plot.weight.transition.maps = F,
		plot.risk.contribution.transition.maps = !is.null(obj)
		)
) {
	#*****************************************************************
	# Setup
	#****************************************************************** 					
	if(is.null(control$plot.weight.transition.maps)) control$plot.weight.transition.maps = F
	if(is.null(control$plot.risk.contribution.transition.maps)) control$plot.risk.contribution.transition.maps = obj!=NULL

    filename = title
    filename.pdf = paste(filename, '.pdf', sep='')
    filename.csv = paste(filename, '.csv', sep='')
	
    # put all reports into one pdf file
	pdf(file = filename.pdf, width=8.5, height=11)
	
	

	#*****************************************************************
	# Create Plots
	#****************************************************************** 					
	layout(1:2)
	plotbt(models, plotX = T, log = 'y', LeftMargin = 3, main = title)	    	
		mtext('Cumulative Performance', side = 2, line = 1)
		
	out = plotbt.strategy.sidebyside(models, return.table=T) 

	#*****************************************************************
	# Create Table
	#****************************************************************** 							
	# Composite Diversification Indicator
	cdi = custom.composite.diversification.indicator(obj, plot.main = F, plot.table = F)	
		out = rbind(colMeans(cdi, na.rm=T), out)
		rownames(out)[1] = 'Composite Diversification Indicator(CDI)'	
	
	# Portfolio Turnover
	y = 100 * sapply(models, compute.turnover, data)
		out = rbind(y, out)
		rownames(out)[1] = 'Portfolio Turnover'		
	
	# Bar chart in descending order of the best algos
	performance.barchart.helper(out, 'Sharpe,Cagr,DVR,MaxDD,Volatility,Portfolio Turnover,Composite Diversification Indicator(CDI)', c(T,T,T,T,F,F,T))			
	
	
	#*****************************************************************
	# Create Plots
	#****************************************************************** 					
	
	# Plot transition maps	
	if(control$plot.weight.transition.maps) {
		layout(1:len(models))
		for(m in names(models)) {
			plotbt.transition.map(models[[m]]$weight, name=m)
				legend('topright', legend = m, bty = 'n')
		}
	}
		
	# Plot transition maps for Risk Contributions
	if(control$plot.risk.contribution.transition.maps) {
		dates = index(data$prices)[obj$period.ends]
		layout(1:len(models))
		layout(1:4)
		for(m in names(models)) {
			plotbt.transition.map(make.xts(obj$risk.contributions[[m]], dates), 
			name=paste('Risk Contributions',m))
				legend('topright', legend = m, bty = 'n')
		}	
	}	
	
	# close pdf file
    dev.off()    	
	
	
	#*****************************************************************
	# save summary & equity curves into csv file
	#****************************************************************** 
	load.packages('abind')
	append=FALSE
	
	cat(title, '\n', file=filename.csv, append=append)
	write.table(out, sep=',',  row.names = , col.names = NA, file=filename.csv, append=TRUE)	
	cat('\n\n', file=filename.csv, append=TRUE)
	
	if(F) {	
		out = abind(lapply(models, function(m) m$equity))
			colnames(out) = names(models)
		write.xts(make.xts(out, index(models[[1]]$equity)), filename.csv, append=TRUE)	
	}	
	
	return(out) 
}


#######################################################################
# Create Summary Stats across data sets
# 
# average turnover / sharpe / volatility across data sets
# compute:
# standardized turnover / sharpe / volatility by =1-NORMSDIST((B3-AVERAGE($B$3:$E$3))/(STDEVA($B$3:$E$3)))
# notes: lower turnover is better, lower vol is better, higher sharpe is better
#######################################################################
bt.summary.stats <- function(stats) 
{
	# notes: lower turnover is better, lower vol is better, higher sharpe is better
	names = spl('Portfolio Turnover,Sharpe,Volatility,Composite Diversification Indicator(CDI)')
	custom.order = c(T,F,T,F)
		
	# composite standardized score
	out = stats[[1]][names,]
	for(i in 1:len(names)) {	
		temp = apply(sapply(stats, function(x) x[names[i],]), 1, function(x) mean(as.double(x)))
		temp = pnorm(temp, mean(temp), sd(temp)) 
		if(custom.order[i]) temp = 1 - temp
		out[i,] = temp
	}
	# higher is better, just consider Portfolio Turnover, Sharpe, Volatility for Composite score
	score = apply(out[1:3,], 2, function(x) mean(as.double(x)))
	
all.out1 = rbind(out, score)
	rownames(all.out1)[5] = 'Composite Standardized Score (higher is better)'
	
	# composite rank score
	out = stats[[1]][names,]
	for(i in 1:len(names)) {	
		temp = apply(sapply(stats, function(x) x[names[i],]), 2, function(x) rank(iif(custom.order[i],1,-1)*as.double(x)))
		out[i,] = rowMeans(temp)
	}
	# lower is better, just consider Portfolio Turnover, Sharpe, Volatility for Composite score
	score = apply(out[1:3,], 2, function(x) mean(as.double(x)))

all.out2 = rbind(out, score)
	rownames(all.out2)[5] = 'Composite Rank Score (lower is better)'
		
write.table(rbind(all.out1, '', all.out2), sep=',',  row.names = , col.names = NA, file='summary.csv')	
		
	# make plot
out = read.csv('summary.csv', stringsAsFactors=F)
	temp = as.matrix(out[,-1,drop=F])
	colnames(temp) = colnames(out)[-1]
	rownames(temp) = out[,1]

pdf(file = 'summary.pdf', width=8.5, height=11)	
	performance.barchart.helper(temp, 'Composite Standardized Score (higher is better),Composite Rank Score (lower is better)', c(T,F), nc.plot=1,
		custom.col=c('MV.S_SA_A'='red', 'best.sharpe'='green'))
dev.off()   	

png(filename = 'plot2.png', width = 600, height = 400, units = 'px', pointsize = 12, bg = 'white')

	performance.barchart.helper(temp, 'Composite Rank Score (lower is better)', c(F), nc.plot=1,
		custom.col=c('MV.S_SA_A'='red', 'best.sharpe'='green'))
	
dev.off()   		


}
