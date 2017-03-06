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
# Examples for the R/Finance Presentation
# Copyright (C) 2012  Michael Kapler
#
# For more information please visit my blog at www.SystematicInvestor.wordpress.com
# or drop me a line at TheSystematicInvestor at gmail
###############################################################################









#############################################################################
# Seasonality Analysis - TIME patterns
# R/Finance 2012
###############################################################################
seasonality.test <- function() 
{
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load.packages('quantmod')
	ticker = 'WMT'
	
	data = getSymbols(ticker, src = 'yahoo', from = '1970-01-01', auto.assign = F)
		data = adjustOHLC(data, use.Adjusted=T)
		
	data = data['1980::2012:04:07']
		
	#*****************************************************************
	# Look at the Month of the Year Seasonality
	#****************************************************************** 
png(filename = 'plot.month.year.seasonality.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')										
	month.year.seasonality(data, ticker)
dev.off()	

	#*****************************************************************
	# Look at What seasonally happens in the first 20 days of May
	#****************************************************************** 
	# Find first day of May: it is one day after the last day of April
	month.ends = endpoints(data, 'months')	
		month.ends = month.ends[month.ends > 0 & month.ends < nrow(data)]
		index = which(format(index(data), '%b')[month.ends] == 'Apr')
	
png(filename = 'plot.time.seasonality.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
	layout(1)
	time.seasonality(data, 1 + month.ends[index], 20, ticker)
dev.off()	
}





#############################################################################
# Pattern Matching - PRICE patterns
# R/Finance 2012
###############################################################################
pattern.test <- function() 
{
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load.packages('quantmod')
	ticker = 'SPY'
	
	data = getSymbols(ticker, src = 'yahoo', from = '1970-01-01', auto.assign = F)
		data = adjustOHLC(data, use.Adjusted=T)
	
	data = data['::2012:04:07']
	#*****************************************************************
	# Find historical Matches similar to the last 90 days of price history
	#****************************************************************** 
png(filename = 'plot1.time.series.pattern.matching.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')			
	matches = bt.matching.find(Cl(data), main = ticker,	n.query=90, plot=TRUE)	
dev.off()		
		
png(filename = 'plot2.time.series.pattern.matching.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')		
	out = bt.matching.overlay(matches, plot=TRUE)	
dev.off()		

	#*****************************************************************
	# Find Classical Techical Patterns, based on
	# Pattern Matching. Based on Foundations of Technical Analysis
	# by A.W. LO, H. MAMAYSKY, J. WANG	
	#****************************************************************** 
png(filename = 'plot.pattern.matching.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')		
	plot.patterns(data, 190, ticker)
dev.off()		
}
	










#############################################################################
# Month of the Year Seasonality
#' @export 
#############################################################################
month.year.seasonality <- function
(
	data,	# xts time series data
	ticker,
	lookback.len = 20*252	# last 20 years of data
) 
{
	data = last(data, lookback.len)
		nperiods = nrow(data)
	
	#*****************************************************************
	# Compute monthly returns
	#****************************************************************** 
	# find month ends
	month.ends = endpoints(data, 'months')
		month.ends = month.ends[month.ends > 0]

	prices = Cl(data)[month.ends]
	ret = prices / mlag(prices) - 1
		ret = ret[-1]

	ret.by.month = create.monthly.table(ret)	
	
	#*****************************************************************
	# Plot
	#****************************************************************** 
	data_list = lapply(apply(ret.by.month, 2, list), '[[', 1)
	group.seasonality(data_list, paste(ticker, 'Monthly', join(format(range(index(data)), '%d-%b-%Y'), ' to\n')))

}    

#' @export 
group.seasonality <- function
(
	data_list,	# data list for each group
	smain,
	...		
) 
{
	#*****************************************************************
	# Compute group stats
	#****************************************************************** 
	out = compute.stats( data_list,
		list(Sharpe=function(x) mean(x,na.rm=T)/sd(x,na.rm=T),
			'% Positive'=function(x) sum(x > 0,na.rm=T)/sum(!is.na(x)),
			Min=function(x) min(x,na.rm=T),
			Max=function(x) max(x,na.rm=T),
			Avg=function(x) mean(x,na.rm=T),
			Med=function(x) median(x,na.rm=T),
			StDev=function(x) sd(x,na.rm=T)
			)
		)
	
	#*****************************************************************
	# Plot
	#****************************************************************** 
	layout(mat=matrix(1:4, 2, 2, byrow=FALSE)) 	
	par(mar=c(4, 3, 2, 2))
	col = spl('lightgray,red')
		
	stats.names = spl('Sharpe,% Positive,Min,Avg')
	for(i in stats.names) {
		barplot(100*out[i,], names.arg = colnames(out), 
			col=iif(out[i,] > 0, col[1], col[2]), 
			main=iif(i == stats.names[1], paste(smain,' ', i, sep=''), i), 
			border = 'darkgray',las=2, ...)
		grid(NA,NULL)
		abline(h=0, col='black')		
	}				
}	






#############################################################################
# Time seasonally for given periods
#' @export 
###############################################################################
time.seasonality <- function
(
	data,			# xts time series data
	period.starts,	# locations
	period.len,		# number of trading days to examine
	ticker
) 
{
	nperiods = len(period.starts)
	dates = index(data)
		ndates = len(dates)

	#*****************************************************************
	# Compute returns, construct trading.days matrix
	#****************************************************************** 
	prices = Cl(data)
	ret = prices / mlag(prices) - 1		
		ret = c( as.double(ret), rep(NA, period.len))

	# 1:period.len by period.starts matrix
	trading.days = sapply(period.starts, function(i) ret[i : (i + period.len - 1)])
		
	#*****************************************************************
	# Compute stats
	#****************************************************************** 	
	# exclude periods that have insufficient data
	periods.index = 1:nperiods
		temp = count(trading.days)
	periods.index = periods.index[temp > 0.9 * median(temp)]
	
	
	# last period
	last.period = trading.days[, nperiods]
		last.period = 100 * ( cumprod(1 + last.period) - 1 )

	# average period
	avg.period = apply(trading.days[, periods.index], 1, mean, na.rm=T)
		avg.period = 100 * ( cumprod(1 + avg.period) - 1 )
		
	# ranges				
	temp = 100*(apply(1 + trading.days[, periods.index], 2, cumprod) - 1)	
	quantiles = apply(temp, 1, quantile, probs = c(75, 25)/100, na.rm=T)		
	
	#*****************************************************************
	# Create Plot
	#****************************************************************** 		
	cols = spl('blue,red,gray')
		
	par(mar=c(4,4,2,1))
	plot(avg.period, type='n', xaxt = 'n', xlim=c(1,period.len),
		ylim=range(avg.period, last.period, quantiles, na.rm=T),
		main = ticker, xlab = 'Trading Days', ylab = 'Avg % Profit/Loss')
				
	grid()
	axis(1, 1:period.len)		
		
	lines(quantiles[1,], type='l', lwd=2, col=cols[3])
	lines(quantiles[2,], type='l', lwd=2, col=cols[3])
		
	lines(last.period, type='b', lwd=2, col=cols[2], bg=cols[2], pch=24)
	lines(avg.period, type='b', lwd=2, col=cols[1], bg=cols[1], pch=22)
		
	first.year = format(dates[period.starts][periods.index[1]], '%Y')
	last.year = format(dates[period.starts][last(periods.index)], '%Y')
	last.period.start.date = format(dates[period.starts[nperiods]], '%d %b %Y')
	last.period.end.date = format(dates[ndates], '%d %b %Y')
			
	if( (period.starts[nperiods] + period.len - 1) < ndates ) {
		last.period.end.date = format(dates[period.starts[nperiods] + period.len - 1], '%d %b %Y')
	}
		
	plota.legend(c(paste('Avgerage for', first.year, '-', last.year),
			paste(last.period.start.date, '-', last.period.end.date),
			'Top 25% / Bot 25%'), cols)	
}




#############################################################################
# Find and Plot Classical Techical Patterns
#' @export 
#############################################################################
plot.patterns <- function
(
	data,	# xts time series data
	n,		# lookback period to search for the patterns
	ticker,	
	patterns = pattern.db()	# database with patterns
) 
{
	#*****************************************************************
	# Find Extrema
	#****************************************************************** 
	load.packages('sm') 
	
	sample = last(data, n)	
	
	obj = find.extrema( Cl(sample) )	
		mhat = obj$mhat
		mhat.extrema.loc = obj$mhat.extrema.loc
		data.extrema.loc = obj$data.extrema.loc
		n.index = len(data.extrema.loc)

	#*****************************************************************
	# Plot	
	#****************************************************************** 
		plota.control$col.border = 'gray'
	plota(sample, type='hl',col='gray')	
		plota.lines(mhat, col='magenta', lwd=2)			
		plota.lines(sample, col='blue')				
		
	if(n.index > 0) {
		plota.lines(sample[data.extrema.loc], type='p', col='blue', lwd=3, pch=19)		
		out = find.patterns(obj, patterns = patterns, silent=F, plot=T)  
	}
			
	plota.legend(c(paste(ticker, join(format(range(index(sample)), '%d%b%Y'), ' - ')),
				'Close,Kernel,Pattern(s)'), 
				'gray,blue,magenta,orange')				
				
}


#############################################################################
# Find maxima and minima using Kernel estimate
#' @export 
#############################################################################
find.extrema <- function(
	x	# time series
) 
{
	if(is.xts(x)) {
		y = as.vector( Cl(x) )
	} else {
		y = x
	}		
	n = len(y)
	t = 1:n
	
	# Fit kernel
	# stat.epfl.ch/files/content/sites/stat/files/users/MdC/notes_3.pdf
	h = h.select(t, y, method = 'cv')
		temp = sm.regression(t, y, h=h, display = 'none')
	mhat = approx(temp$eval.points, temp$estimate, t, method='linear')$y

	# page 15
	# find locations of local maxima and minima in mhat
	temp = diff(sign(diff(mhat)))
	loc = which( temp != 0 ) + 1
		loc.dir = -sign(temp[(loc - 1)])
	
	# check
	# temp = cbind(mhat[(loc - 1)],mhat[(loc)],mhat[(loc + 1)])
	# cbind(round(temp,2), loc.dir, apply(temp, 1, which.max), apply(temp, 1, which.min))
	
		
	# page 16
	# find locations of local maxima and minima in original data with in +1/-1 local maxima and minima in mhat
	temp = c( y[1], y, y[n] )
	temp = cbind(temp[loc], temp[(loc + 1)], temp[(loc + 2)])
		max.index = loc + apply(temp, 1, which.max) - 2
		min.index = loc + apply(temp, 1, which.min) - 2
	data.loc = iif(loc.dir > 0, max.index, min.index)
	data.loc = iif(data.loc < 1, 1, iif(data.loc > n, n, data.loc))

	if(is.xts(x)) mhat = make.xts(mhat, index(x))
	
	return(list(data = y, mhat = mhat, extrema.dir = loc.dir,
		mhat.extrema.loc = loc, data.extrema.loc = data.loc))
}


#############################################################################
# Find Classical Techincal patterns
#' @export 
#############################################################################
find.patterns <- function
(
	obj, 	# extrema points
	patterns = pattern.db(), 
	silent=T, 
	plot=T
) 
{
	data = obj$data
	mhat = obj$mhat
	extrema.dir = obj$extrema.dir
	data.extrema.loc = obj$data.extrema.loc
	n.index = len(data.extrema.loc)

	if(is.xts(mhat)) {
		dates = index4xts(obj$mhat)
	} else {
		dates = 1:len(data)
	}		
	
	# Semi-transparent orange color
	col = col.add.alpha('orange', alpha=150)
	
		
	out = out.rownames = c()
	
	# search for patterns
	for(i in 1:n.index) {
	
		for(pattern in patterns) {
		
			# check same sign
			if( pattern$start * extrema.dir[i] > 0 ) {
			
				# check that there is suffcient number of extrema to complete pattern
				if( i + pattern$len - 1 <= n.index ) {
				
					# create enviroment to check pattern: E1,E2,...,En; t1,t2,...,tn
					envir.data = c(data[data.extrema.loc][i:(i + pattern$len - 1)], 
									data.extrema.loc[i:(i + pattern$len - 1)])									
						names(envir.data) = c(paste('E', 1:pattern$len, sep=''), 
												paste('t', 1:pattern$len, sep=''))
					envir.data = as.list(envir.data)					
					
					# double top/bottom patterns require all extrema [we will exclude the first point(E1/t1)]
					#envir.data$E = data[data.extrema.loc][i:n.index]
					envir.data$E = data[data.extrema.loc][-c(1:i)]
					envir.data$t = data.extrema.loc[-c(1:i)]
					
					# check if pattern was found
					if( eval(pattern$formula, envir = envir.data) ) {
						if(!silent) cat('Found', pattern$name, 'at', i, '\n')
						
						if(plot & !is.null(pattern$plot)) {						
							temp = dates[data.extrema.loc[i:(i + pattern$len - 1)]]									
								names(temp) = paste('d', 1:pattern$len, sep='')
							envir.data = c( envir.data, temp )
							envir.data$d = dates[data.extrema.loc[-c(1:i)]]
							envir.data$col = col
							
							eval(pattern$plot, envir = envir.data)
						}
						
						# record 
						out.rownames = c(out.rownames, pattern$name)
						out = rbind(out, c(data.extrema.loc[i], 
											iif(is.null(pattern$last.point),
												data.extrema.loc[(i + pattern$len - 1)], 
												eval(pattern$last.point, envir = envir.data)
												)))
					}
				}
			}		
		}	
	}
	
	if(len(out)>0) {
		colnames(out) = spl('start,end')
		rownames(out) = out.rownames
	}
	return(out)
}


#############################################################################
# Find historical patterns in the data over a rolling window
#' @export 
#############################################################################
find.all.patterns.window <- function() 
{
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load.packages('quantmod')
	ticker = 'SPY'
	
	data = getSymbols(ticker, src = 'yahoo', from = '1970-01-01', auto.assign = F)
		data = adjustOHLC(data, use.Adjusted=T)

	data = data['2010::']
	#*****************************************************************
	# Search for all patterns over a rolling window
	#****************************************************************** 
	load.packages('sm') 
	history = as.vector(coredata(Cl(data)))
	window.len = 90
	patterns = pattern.db()
	
	found.patterns = c()
	
	for(t in window.len : (len(history)-1)) {
		sample = history[(t - window.len + 1):t]		
		obj = find.extrema( sample )	
		
		if(len(obj$data.extrema.loc) > 0) {
			out =  find.patterns(obj, patterns = patterns, silent=F, plot=F)  
			
			if(len(out)>0) found.patterns = rbind(found.patterns,cbind(t,out,t-window.len+out))			
		}
		if( t %% 10 == 0) cat(t, 'out of', len(history), '\n')
	}
	colnames(found.patterns) = spl('t,start,end,tstart,tend')
	
	#*****************************************************************
	# Examine found patterns
	#****************************************************************** 	
	# check what patterns are not found
	setdiff(unique(names(patterns)), unique(rownames(found.patterns)))

	# number of matches for each pattern	
	frequency = tapply(rep(1,nrow(found.patterns)), rownames(found.patterns), sum)
	barplot(frequency)
	
	# determine starting Time for a pattern
	index = which(rownames(found.patterns)=='HS')
	found.patterns[ index[1:10],]			
	
	# plot
	pattern.index = index[1]
	t = found.patterns[pattern.index, 't'];	
	plot.patterns(data[1:t,], window.len, ticker, patterns)	

		
	# plot start/end of this pattern
	sample = data[(t - window.len + 1):t]		
		start.pattern = found.patterns[pattern.index, 'start']
		end.pattern = found.patterns[pattern.index, 'end']		
	abline(v = index(sample)[start.pattern]);
	abline(v = index(sample)[end.pattern]);
	
	index(sample)[start.pattern]	
	index(data[(t - window.len + start.pattern),])
	
}


#############################################################################
# Compute conditional returns for each pattern over a rolling window
#############################################################################
bt.patterns.test <- function() 
{
	#*****************************************************************
	# Load historical data
	#****************************************************************** 
	load.packages('quantmod')
	ticker = 'SPY'
	
	data = getSymbols(ticker, src = 'yahoo', from = '1970-01-01', auto.assign = F)
		data = adjustOHLC(data, use.Adjusted=T)

	#data = data['2010::']

	#*****************************************************************
	# Setup
	#****************************************************************** 
	# page 14-15, L = 35 and d = 3, rolling the length of the window at L + d,
	# The parameter d controls for the fact that in practice we do not observe a
	# realization of a given pattern as soon as it has completed. Instead, we assume
	# that there may be a lag between the pattern completion and the time
	# of pattern detection. To account for this lag, we require that the final extremum
	# that completes a pattern occurs on day t + L - 1; hence d is the number
	# of days following the completion of a pattern that must pass before the pattern
	# is detected.	
	#In particular, we compute postpattern returns starting from the end of trading
	#day t + L + d, that is, one day after the pattern has completed. For
	#example, if we determine that a head-and-shoulder pattern has completed
	# on day t + L - 1 (having used prices from time t through time t + L + d - 1),
	#we compute the conditional one-day gross return as Z1=Yt+L+d+1/Yt+L+d.	
	
	#*****************************************************************
	# Search for all patterns over a rolling window
	#****************************************************************** 
	load.packages('sm') 
	history = as.vector(coredata(Cl(data)))
	
	window.L = 35
	window.d = 3
	window.len = window.L + window.d

	patterns = pattern.db()
	
	found.patterns = c()
	
	for(t in window.len : (len(history)-1)) {
		ret = history[(t+1)]/history[t]-1
		
		sample = history[(t - window.len + 1):t]		
		obj = find.extrema( sample )	
		
		if(len(obj$data.extrema.loc) > 0) {
			out =  find.patterns(obj, patterns = patterns, silent=F, plot=F)  
			
			if(len(out)>0) found.patterns = rbind(found.patterns,cbind(t,out,t-window.len+out, ret))
		}
		if( t %% 10 == 0) cat(t, 'out of', len(history), '\n')
	}
	colnames(found.patterns) = spl('t,start,end,tstart,tend,ret')
	
	#*****************************************************************
	# Clean found patterns
	#****************************************************************** 	
	# remove patterns that finished after window.L
	found.patterns = found.patterns[found.patterns[,'end'] <= window.L,]
		
	# remove the patterns found multiple times, only keep first one
	pattern.names = unique(rownames(found.patterns))
	all.patterns = c()
	for(name in pattern.names) {
		index = which(rownames(found.patterns) == name)
		temp = NA * found.patterns[index,]
		
		i.count = 0
		i.start = 1
		while(i.start < len(index)) {
			i.count = i.count + 1
			temp[i.count,] = found.patterns[index[i.start],]
			subindex = which(found.patterns[index,'tstart'] > temp[i.count,'tend'])			
						
			if(len(subindex) > 0) {
				i.start = subindex[1]
			} else break		
		} 
		all.patterns = rbind(all.patterns, temp[1:i.count,])		
	}
	
	#*****************************************************************
	# Plot
	#****************************************************************** 	
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')	
	# number of matches for each pattern	
	frequency = tapply(rep(1,nrow(all.patterns)), rownames(all.patterns), sum)
	layout(1)
	barplot.with.labels(frequency/100, 'Frequency for each Pattern')
dev.off()
	

	
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')	
	# pattern seasonality
	all.patterns[,'ret'] = history[(all.patterns[,'t']+20)] / history[all.patterns[,'t']] - 1
	data_list = tapply(all.patterns[,'ret'], rownames(all.patterns), list)
	group.seasonality(data_list, '20 days after Pattern')
dev.off()	


png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')	
	# time pattern seasonality
	layout(1)
	name = 'BBOT'
	index = which(rownames(all.patterns) == name)	
	time.seasonality(data, all.patterns[index,'t'], 20, name)	
dev.off()





	#t.test(out[,'ret'])$p.value
	#tapply(out[,'ret'], rownames(out), function(x) t.test(x)$p.value)
	
}



###############################################################################
# Pattern Matching
#
# Based on Foundations of Technical Analysis: Computational Algorithms, Statistical Inference, and Empirical Implementation
# by A.W. LO, H. MAMAYSKY, J. WANG
###############################################################################
# http://thepatternsite.com/
#' @export 
###############################################################################
pattern.db <- function() 
{
	# page 12
	patterns = list()

	#*****************************************************************
	# You can reference E1,E2,...,En and t1,t2,...,tn in pattern formula
	#****************************************************************** 	
	# Head-and-shoulders (HS)
	#****************************************************************** 	
	pattern = list()
	pattern$len = 5
	pattern$start = 'max'
	pattern$formula = expression(
		# E3 > E1, E3 > E5
		E3 > E1 &
		E3 > E5 &
		
		# E1 and E5 are within 1.5 percent of their average
		abs(E1 - (E1+E5)/2) < 1.5/100 * (E1+E5)/2 &
		abs(E5 - (E1+E5)/2) < 1.5/100 * (E1+E5)/2 &
		
		# E2 and E4 are within 1.5 percent of their average
		abs(E2 - (E2+E4)/2) < 1.5/100 * (E2+E4)/2 &
		abs(E4 - (E2+E4)/2) < 1.5/100 * (E2+E4)/2
		)
	pattern$plot = expression({
		lines(c(d1,d2,d3,d4,d5), c(E1,E2,E3,E4,E5), lwd=10, col=col)
			text(d3, E3, 'HS', adj=c(0.5,-0.5), xpd=TRUE)		
		})								
	patterns$HS = pattern		

	#*****************************************************************
	# Inverted Head-and-shoulders (IHS)
	#****************************************************************** 	
	pattern = list()
	pattern$len = 5
	pattern$start = 'min'
	pattern$formula = expression(
		# E3 < E1, E3 < E5
		E3 < E1 &
		E3 < E5 &
		
		# E1 and E5 are within 1.5 percent of their average
		abs(E1 - (E1+E5)/2) < 1.5/100 * (E1+E5)/2 &
		abs(E5 - (E1+E5)/2) < 1.5/100 * (E1+E5)/2 &
		
		# E2 and E4 are within 1.5 percent of their average
		abs(E2 - (E2+E4)/2) < 1.5/100 * (E2+E4)/2 &
		abs(E4 - (E2+E4)/2) < 1.5/100 * (E2+E4)/2
		)
	pattern$plot = expression({
		lines(c(d1,d2,d3,d4,d5), c(E1,E2,E3,E4,E5), lwd=10, col=col)
			text(d3, E3, 'IHS', adj=c(0.5,1), xpd=TRUE)		
		})						
	patterns$IHS = pattern		
	
	#*****************************************************************
	# Broadening tops (BTOP)
	#****************************************************************** 		
	pattern = list()
	pattern$len = 5
	pattern$start = 'max'
	pattern$formula = expression(
		# E1 < E3 < E5
		E1 < E3 &
		E3 < E5 &		
		E2 > E4
		)
	pattern$plot = expression({
		beta = ols(cbind(1,c(t1,t3,t5)),c(E1,E3,E5))$coefficients
		lines(c(d1,d3,d5), beta[1] + beta[2]*c(t1,t3,t5), lwd=10, col=col)	
		lines(c(d2,d4), c(E2,E4), lwd=10, col=col)
			text(d3, min(E2,E4), 'BTOP', adj=c(0.5,1), xpd=TRUE)
		})				
	patterns$BTOP = pattern		
		
	#*****************************************************************
	# Broadening bottoms (BBOT)
	#****************************************************************** 		
	pattern = list()
	pattern$len = 5
	pattern$start = 'min'
	pattern$formula = expression(
		# E1 > E3 > E5
		E1 > E3 &
		E3 > E5 &		
		E2 < E4
		)		
	pattern$plot = expression({
		beta = ols(cbind(1,c(t1,t3,t5)),c(E1,E3,E5))$coefficients
		lines(c(d1,d3,d5), beta[1] + beta[2]*c(t1,t3,t5), lwd=10, col=col)	
		lines(c(d2,d4), c(E2,E4), lwd=10, col=col)
			text(d3, max(E2,E4), 'BBOT', adj=c(0.5,0), xpd=TRUE)
		})		
	patterns$BBOT = pattern		

	#*****************************************************************
	# Triangle tops (TTOP)
	#****************************************************************** 		
	pattern = list()
	pattern$len = 5
	pattern$start = 'max'
	pattern$formula = expression(
		# E1 > E3 > E5
		E1 > E3 &
		E3 > E5 &		
		E2 < E4
		)
	pattern$plot = expression({
		beta = ols(cbind(1,c(t1,t3,t5)),c(E1,E3,E5))$coefficients
		lines(c(d1,d3,d5), beta[1] + beta[2]*c(t1,t3,t5), lwd=10, col=col)
		lines(c(d2,d4), c(E2,E4), lwd=10, col=col)
			text(d3, min(E2,E4), 'TTOP', adj=c(0.5,1), xpd=TRUE)
		})						
	patterns$TTOP = pattern		
	
	#*****************************************************************
	# Triangle bottoms (TBOT)
	#****************************************************************** 		
	pattern = list()
	pattern$len = 5
	pattern$start = 'min'
	pattern$formula = expression(
		# E1 < E3 < E5
		E1 < E3 &
		E3 < E5 &		
		E2 > E4
		)
	pattern$plot = expression({
		beta = ols(cbind(1,c(t1,t3,t5)),c(E1,E3,E5))$coefficients		
		lines(c(d1,d3,d5), beta[1] + beta[2]*c(t1,t3,t5), lwd=10, col=col)
		lines(c(d2,d4), c(E2,E4), lwd=10, col=col)
			text(d3, max(E2,E4), 'TBOT', adj=c(0.5,0), xpd=TRUE)
		})				
	patterns$TBOT = pattern		
	
	#*****************************************************************
	# Rectangle tops (RTOP)
	#****************************************************************** 		
	pattern = list()
	pattern$len = 5
	pattern$start = 'max'
	pattern$formula = expression({
		avg.top = (E1+E3+E5)/3
		avg.bop = (E2+E4)/2
		
		# tops E1,E3,E5 are within 0.75 percent of their average
		abs(E1 - avg.top) < 0.75/100 * avg.top &
		abs(E3 - avg.top) < 0.75/100 * avg.top &
		abs(E5 - avg.top) < 0.75/100 * avg.top &
		
		# bottoms E2,E4 are within 0.75 percent of their average
		abs(E2 - avg.bop) < 0.75/100 * avg.bop &
		abs(E4 - avg.bop) < 0.75/100 * avg.bop &
				
		# lowest top > highest bottom
		min(E1,E3,E5) > max(E2,E4)
		})
	pattern$plot = expression({
		avg.top = (E1+E3+E5)/3
		avg.bop = (E2+E4)/2

		lines(c(d1,d3,d5), rep(avg.top,3), lwd=10, col=col)
		lines(c(d2,d4), rep(avg.bop,2), lwd=10, col=col)
			text(d3, min(E2,E4), 'RTOP', adj=c(0.5,-0.5), xpd=TRUE)
		})						
	patterns$RTOP = pattern		
	
	#*****************************************************************
	# Rectangle bottoms (RBOT)
	#****************************************************************** 		
	pattern = list()
	pattern$len = 5
	pattern$start = 'min'
	pattern$formula = expression({
		avg.top = (E2+E4)/2
		avg.bop = (E1+E3+E5)/3
	
		# tops E2,E4 are within 0.75 percent of their average
		abs(E2 - avg.top) < 0.75/100 * avg.top &
		abs(E4 - avg.top) < 0.75/100 * avg.top &
		
		# bottoms E1,E3,E5 are within 0.75 percent of their average		
		abs(E1 - avg.bop) < 0.75/100 * avg.bop &
		abs(E3 - avg.bop) < 0.75/100 * avg.bop &
		abs(E5 - avg.bop) < 0.75/100 * avg.bop &
				
		# lowest top > highest bottom
		min(E2,E4) > max(E1,E3,E5)
		})
	pattern$plot = expression({
		avg.top = (E2+E4)/2
		avg.bop = (E1+E3+E5)/3
	
		lines(c(d1,d3,d5), rep(avg.bop,3), lwd=10, col=col)
		lines(c(d2,d4), rep(avg.top,2), lwd=10, col=col)
			text(d3, max(E2,E4), 'RBOT', adj=c(0.5,0), xpd=TRUE)
		})						
	patterns$RBOT = pattern		
		
	#*****************************************************************
	# Analyzing Chart Patterns: Double Top And Double Bottom
	# http://www.investopedia.com/university/charts/charts4.asp
	#*****************************************************************
	# Double tops (DTOP), note in E and t first one is excluded
	#****************************************************************** 		
	pattern = list()
	pattern$len = 3
	pattern$start = 'max'
	pattern$formula = expression({
		# Ea = max(E), ta = t[which.max(E)]
		second.top = max(E)
		second.top.t = t[which.max(E)]
		avg = (E1 + second.top)/2

		# E1 and Ea are within 1.5 percent of their average
		abs(E1         - avg) < 1.5/100 * avg &
		abs(second.top - avg) < 1.5/100 * avg &
		
		# ta - t1 > 22
		second.top.t - t1 > 22
		})
	pattern$plot = expression({
		second.top = max(E)
		second.top.d = d[which.max(E)]
		avg = (E1 + second.top)/2
		
		points(c(d1, second.top.d), c(E1, second.top), pch=2, lwd=2) 
		lines(c(d1, second.top.d), rep(avg, 2), lwd=10, col=col)
			text(d2, avg, 'DTOP', adj=c(0.5,-0.5), xpd=TRUE)
		})
	pattern$last.point = expression(t[which.max(E)])
	patterns$DTOP = pattern		

	#*****************************************************************
	# Double bottoms (DBOT)
	#****************************************************************** 		
	pattern = list()
	pattern$len = 3
	pattern$start = 'min'
	pattern$formula = expression(
		# E1 and Ea = min(E) are within 1.5 percent of their average
		abs(E1 -         (E1+min(E))/2) < 1.5/100 * (E1+min(E))/2 &
		abs(max(E[-1]) - (E1+min(E))/2) < 1.5/100 * (E1+min(E))/2 &
		
		# ta - t1 > 22, ta = t[which.min(E)]
		t[which.min(E)] - t1 > 22
		)
	pattern$plot = expression({
		second.bot = min(E)
		second.bot.d = d[which.min(E)]
		avg = (E1 + second.bot)/2
		
		points(c(d1, second.bot.d), c(E1, second.bot), pch=2, lwd=2) 
		lines(c(d1, second.bot.d), rep(avg, 2), lwd=10, col=col)
			text(d2, avg, 'DBOT', adj=c(0.5,1), xpd=TRUE)
		})	
	pattern$last.point = expression(t[which.min(E)])
	patterns$DBOT = pattern		
	
	
	# add name and convert start to +1/-1
	for(i in 1:len(patterns)) {
		patterns[[i]]$name = names(patterns)[i]
		patterns[[i]]$start = iif(patterns[[i]]$start == 'max', 1, -1)	
	}

	return(patterns)	
}




