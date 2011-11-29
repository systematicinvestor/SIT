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
# Backtest Summary Report Functions
# Copyright (C) 2011  Michael Kapler
#
# For more information please visit my blog at www.SystematicInvestor.wordpress.com
# or drop me a line at TheSystematicInvestor at gmail
###############################################################################




###############################################################################
# Custom Backtest Report
###############################################################################
plotbt.custom.report <- function
( 
	..., 
	dates = NULL, 
	main = '', 
	trade.summary = FALSE 
) 
{	
	# create layout	
	ilayout = 
		'1,1
		1,1
		2,2
		3,3
		4,6
		4,6
		5,7
		5,8'
	plota.layout(ilayout)

	models = variable.number.arguments( ... )
		
	# Main plot
	plotbt(models, dates = dates, main = main, plotX = F, log = 'y', LeftMargin = 3)	    	
		mtext('Cumulative Performance', side = 2, line = 1)
	
	plotbt(models[1], plottype = '12M', dates = dates, plotX = F, LeftMargin = 3)	    	
		mtext('12 Month Rolling', side = 2, line = 1)
		
	plotbt(models[1], dates = dates, xfun = function(x) { 100 * compute.drawdown(x$equity) }, LeftMargin = 3)
		mtext('Drawdown', side = 2, line = 1)
			
	model = models[[1]]
	
	# Additional Info
	plotbt.transition.map(model$weight)			
	temp = plotbt.monthly.table(model$equity)	
	plotbt.holdings.time(model$weight)
	
	if ( !is.null(model$trade.summary) ) {
		plot.table( list2matrix(bt.detail.summary(model, model$trade.summary)), keep_all.same.cex = TRUE)		
	} else {
		plot.table( list2matrix(bt.detail.summary(model)), keep_all.same.cex = TRUE)
	}
			
	if( len(models) > 1 ) plotbt.strategy.sidebyside(models)
		
	if ( trade.summary ) {	
		ntrades = min(20, nrow(model$trade.summary$trades))
		print( last(model$trade.summary$trades, ntrades) )
		print( model$trade.summary$stats )

		layout(1)
		plot.table( last(model$trade.summary$trades,ntrades) )
	}	
}	

# split plotbt.custom.report into 3 functions
plotbt.custom.report.part1 <- function
( 
	..., 
	dates = NULL, 
	main = '', 
	trade.summary = FALSE 
) 
{	
	layout(1:3)

	models = variable.number.arguments( ... )
	model = models[[1]]
	
	# Main plot
	plotbt(models, dates = dates, main = main, plotX = F, log = 'y', LeftMargin = 3)	    	
		mtext('Cumulative Performance', side = 2, line = 1)
	
	plotbt(models[1], plottype = '12M', dates = dates, plotX = F, LeftMargin = 3)	    	
		mtext('12 Month Rolling', side = 2, line = 1)
		
	plotbt(models[1], dates = dates, xfun = function(x) { 100 * compute.drawdown(x$equity) }, LeftMargin = 3)
		mtext('Drawdown', side = 2, line = 1)
}

plotbt.custom.report.part2 <- function
( 
	..., 
	dates = NULL, 
	main = '', 
	trade.summary = FALSE 
) 
{	
	models = variable.number.arguments( ... )
	model = models[[1]]
		
	# create layout	
	ilayout = 
		'1,3		
		2,4
		2,5'
	plota.layout(ilayout)
	
			
	# Additional Info
	plotbt.transition.map(model$weight)			
	temp = plotbt.monthly.table(model$equity)	
	plotbt.holdings.time(model$weight)
	
	if ( !is.null(model$trade.summary) ) {
		plot.table( list2matrix(bt.detail.summary(model, model$trade.summary)), keep_all.same.cex = TRUE)		
	} else {
		plot.table( list2matrix(bt.detail.summary(model)), keep_all.same.cex = TRUE)
	}
			
	if( len(models) > 1 ) plotbt.strategy.sidebyside(models)
}	

plotbt.custom.report.part3 <- function
( 
	..., 
	dates = NULL, 
	main = '', 
	trade.summary = FALSE 
) 
{	

	models = variable.number.arguments( ... )
	model = models[[1]]
		
	if ( trade.summary ) {	
		ntrades = min(20, nrow(model$trade.summary$trades))
		print( last(model$trade.summary$trades, ntrades) )
		print( model$trade.summary$stats )

		layout(1)
		plot.table( last(model$trade.summary$trades,ntrades) )
	}	
}

###############################################################################
# Backtest Detail summary
###############################################################################
bt.detail.summary <- function
(
	bt,		# backtest object
	trade.summary = NULL
) 
{	
	out.all = list()

	# System Section	
	out = list()
		out$Period = join( format( range(index(bt$equity)), '%b%Y'), ' - ')
		
		out$Cagr = compute.cagr(bt$equity)
		out$Sharpe = compute.sharpe(bt$ret)
		out$DVR = compute.DVR(bt)
		out$Volatility = compute.risk(bt$ret)
				
		out$MaxDD = compute.max.drawdown(bt$equity)
		out$AvgDD = compute.avg.drawdown(bt$equity)
		
		if( !is.null(trade.summary) ) {
			out$Profit.Factor = trade.summary$stats['profitfactor', 'All']
		}

		out$VaR = compute.var(bt$ret)
		out$CVaR = compute.cvar(bt$ret)
		
		out$Exposure = compute.exposure(bt$weight)						
	out.all$System = lapply(out, function(x) if(is.double(x)) round(100*x,1) else x)
			
	
	
	
	
	# Trade Section	
	if( !is.null(bt$trade.summary) ) trade.summary = bt$trade.summary
		
	out = list()
	if( !is.null(trade.summary) ) {
		out$Win.Percent = trade.summary$stats['win.prob', 'All']
		out$Avg.Trade = trade.summary$stats['avg.pnl', 'All']
		out$Avg.Win = trade.summary$stats['win.avg.pnl', 'All']
		out$Avg.Loss = trade.summary$stats['loss.avg.pnl', 'All']
			
		out = lapply(out, function(x) if(is.double(x)) round(100*x,1) else x)		
		
		out$Best.Trade = max(as.double(trade.summary$trades[, 'return']))
		out$Worst.Trade = min(as.double(trade.summary$trades[, 'return']))
		
		out$WinLoss.Ratio = round( -trade.summary$stats['win.avg.pnl', 'All']/trade.summary$stats['loss.avg.pnl', 'All'] , 2)
		out$Avg.Len = round(trade.summary$stats['len', 'All'],2)
		out$Num.Trades = trade.summary$stats['ntrades', 'All']			
	}
	out.all$Trade = out
		
	# Period Section
	out = list()
		out$Win.Percent.Day = sum(bt$ret > 0, na.rm = T) / len(bt$ret)
		out$Best.Day = bt$best
		out$Worst.Day = bt$worst
		
		month.ends = endpoints(bt$equity, 'months')
		mret = ROC(bt$equity[month.ends,], type = 'discrete')
		out$Win.Percent.Month = sum(mret > 0, na.rm = T) / len(mret)
		out$Best.Month = max(mret, na.rm = T)
		out$Worst.Month = min(mret, na.rm = T)
		
		year.ends = endpoints(bt$equity, 'years')
		mret = ROC(bt$equity[year.ends,], type = 'discrete')
		out$Win.Percent.Year = sum(mret > 0, na.rm = T) / len(mret)
		out$Best.Year = max(mret, na.rm = T)
		out$Worst.Year = min(mret, na.rm = T)
	out.all$Period = lapply(out, function(x) if(is.double(x)) round(100*x,1) else x)
	
	return(out.all)
}
	
###############################################################################
# Plot strategy perfromance side by side		
###############################################################################
plotbt.strategy.sidebyside <- function
( 
	... , 
	perfromance.metric = spl('System,Trade,Period') 
) 
{
	models = variable.number.arguments( ... )
	out = list()
	
	for( i in 1:len(models) ) {
		out[[ names(models)[i] ]] = bt.detail.summary(models[[ i ]])[[ perfromance.metric[1] ]]
	}
	plot.table( list2matrix(out, keep.names=F), smain = perfromance.metric[1] )
}


###############################################################################
# Plot equity curves for eact strategy(model)
###############################################################################
plotbt <- function
(
	...,				# variable arguments
	dates = NULL,		# dates subset
	plottype = spl('line,12M'),
	xfun=function(x) { x$equity },
	main = NULL,
	plotX = T,
	log = '',
	LeftMargin = 0 
) 
{    		
	models = variable.number.arguments( ... )					
	plottype = plottype[1]
   	n = length(models)    	

   	# get data
   	temp = list()
   	for( i in 1:n ) {
		msg = try( match.fun(xfun)( models[[i]] ) , silent = TRUE)
		if (class(msg)[1] != 'try-error') {
			temp[[i]] = msg
		}
	}

	# prepare plot
	nlag = len(last(temp[[1]], '12 months'))
   	yrange=c();   	   	
   	for( i in 1:n ) {
   		itemp = temp[[i]]
   		
   		if(!is.null(dates)) {
   			itemp = itemp[dates]
   			if(itemp[1] != 0) itemp = itemp / as.double(itemp[1])
   		}
   		
   		if( plottype == '12M' ) {
   			itemp = 100 * (itemp / mlag(itemp, nlag ) - 1)
   		}
   		temp[[i]] = itemp
   		
		yrange = range(yrange, itemp ,na.rm = T)		
   	}
   	  	
   	# plot
	plota(temp[[1]], main = main, plotX = plotX, type = 'l', col = 1, 
		ylim = yrange,log = log, LeftMargin = LeftMargin)
		
	if( n > 1 ) {
		for( i in 2:n ) plota.lines(temp[[i]], col = i)
	}
	
	if( plottype == '12M' ) legend('topright', legend = '12 Month Rolling', bty = 'n')
	plota.legend(names(models), paste('', 1:n, sep=''), temp)	
}

###############################################################################
# Plot Transition Map
###############################################################################
plotbt.transition.map <- function(weight) 
{
	par(mar=c(2, 4, 1, 1), cex = 0.8, cex.main=0.8,cex.sub=0.8,cex.axis=0.8,cex.lab=0.8)
	icols=rainbow(ncol(weight), start=0, end=.9)	
	
	weight[is.na(weight)] = 0	
	plota.stacked(index(weight), weight, col = icols)	
}
	
###############################################################################
# Plot Pie Chart for holdings
###############################################################################
plotbt.holdings <- function
(
	weight, 
	smain = format(index(last(weight)), '%d-%b-%Y') 
) 
{
	par(mar=c(2, 2, 2, 2), cex = 0.8, cex.main=0.8,cex.sub=0.8,cex.axis=0.8,cex.lab=0.8)
	icols=rainbow(ncol(weight), start=0, end=.9)
	
	temp = 100 * as.vector(last(weight))
	atemp = abs(temp)	
	
	if(sum(atemp)>0) {
		pie(atemp, labels = paste(round(temp,0), '% ', colnames(weight), sep=''), 
			col = icols, cex =0.8,
			main = paste('Allocation for ', smain, sep='')
		)
	}
}

###############################################################################
# Plot Pie Chart for holdings throught out time
###############################################################################
plotbt.holdings.time <- function(weight) 
{
	weight = as.matrix( apply(abs(weight), 2, sum, na.rm = T) )
	plotbt.holdings( t(weight) / sum(abs(weight), na.rm = T), smain = 'in time')
}


###############################################################################
# Plot monthly return table
###############################################################################
plotbt.monthly.table <- function(equity) 
{
	dates = index(equity)
	equity = coredata(equity)

	# find period ends
	month.ends = unique(c(endpoints(dates, 'months'), len(dates)))
		month.ends = month.ends[month.ends>0]
	year.ends =  unique(c(endpoints(dates[month.ends], 'years'), len(month.ends)))
		year.ends = year.ends[year.ends>0]
		year.ends = month.ends[year.ends]
	nr = len(year.ends) + 1
		
	# create plot matrix
	temp = matrix( double(), nr, 12 + 2)
		rownames(temp) = c(date.year(dates[year.ends]), 'Avg')
		colnames(temp) = spl('Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec,Year,MaxDD')
	
	# compute yearly profit and drawdown
	index = unique( c(1, year.ends) )
	for(iyear in 2:len(index)) {
		iequity = equity[ index[(iyear-1)] : index[iyear] ]
		iequity = ifna( ifna.prev(iequity), 0)
		
		temp[(iyear-1), 'Year'] = last(iequity, 1) / iequity[1] -1
		temp[(iyear-1), 'MaxDD'] = min(iequity / cummax(iequity) - 1, na.rm = T)
	}

	# compute monthly profit
	index = unique( c(1, month.ends) )
		monthly.returns = diff(equity[index]) / equity[index[-len(index)]]
		
		index = date.month(range(dates[index[-1]]))
		monthly.returns = c( rep(NA, index[1]-1), monthly.returns, rep(NA, 12-index[2]) )
		temp[1:(nr - 1), 1:12] = matrix(monthly.returns, ncol=12, byrow = T)
			
	# compute averages
	temp = ifna(temp, NA)
	temp[nr,] = apply(temp[-nr,], 2, mean, na.rm = T)		

	#higlight
	highlight = temp
		highlight[] = iif(temp > 0, 'lightgreen', iif(temp < 0, 'red', 'white'))
		highlight[nr,] = iif(temp[nr,] > 0, 'green', iif(temp[nr,] < 0, 'orange', 'white'))
		highlight[,13] = iif(temp[,13] > 0, 'green', iif(temp[,13] < 0, 'orange', 'white'))
		highlight[,14] = 'yellow'
	
	# plot
	temp[] = plota.format(100 * temp, 1, '', '')
	plot.table(temp, highlight = highlight)
	
	return(temp)
}	
	
###############################################################################
# Helper functions
###############################################################################
#http://tolstoy.newcastle.edu.au/R/help/06/05/28060.html    
variable.number.arguments <- function( ... ) 
{
	out = list( ... )
	if( is.list(out[[1]][[1]]) ) return( out[[1]] )
	
	names( out ) = as.character(substitute(c(...))[-1])  	
	return ( out )
}	



###############################################################################
# Convert list of lists to matrix
###############################################################################
list2matrix <- function
(
	ilist,
	keep.names = TRUE
) 
{   
	if ( is.list( ilist[[1]] ) ) {
		inc = 1
		if( keep.names ) inc = 2
		
		out = matrix('', nr = max(unlist(lapply(ilist, len))), nc = inc * len(ilist) )
		colnames(out) = rep('', inc * len(ilist))
		
		for( i in 1:len(ilist) ) {
			nr = len(ilist[[i]])
			colnames(out)[inc * i] = names(ilist)[i]
			
			if(nr > 0){ 
				if( keep.names ) {
					out[1:nr,(2*i-1)] = names(ilist[[i]])
				} else {
					rownames(out) = names(ilist[[i]])
				}
				out[1:nr,inc*i] = unlist(ilist[[i]])
			}
		}
		return(out)
	} else {
		return( as.matrix(unlist(ilist)) )
	}
}

