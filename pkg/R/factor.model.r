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
# Collection of Utilities to Biuld and Analyze Multiple Factor Model
# Copyright (C) 2012  Michael Kapler
#
# For more information please visit my blog at www.SystematicInvestor.wordpress.com
# or drop me a line at TheSystematicInvestor at gmail
###############################################################################



###############################################################################
# Count Consecutive Changes
#' @export 
###############################################################################
consecutive.changes <- function
(
	data, 		# data series
	positive=T	# count positive consecutive changes
) 
{ 
	if(positive) dir = diff(data) > 0 else dir = diff(data) < 0
					
	temp = cumsum(iif(dir, 1, 0))
	temp - ifna.prev(iif(dir, NA, coredata(temp)))
}

###############################################################################
# Create plot of factors average correlations and returns
###############################################################################
# http://stackoverflow.com/questions/4310727/what-is-rs-multidimensional-equivalent-of-rbind-and-cbind
# apply(temp, 3, rbind)
# http://r.789695.n4.nabble.com/Collapse-an-array-td850008.html
#' @export 
factor.avgcor <- function(data, next.month.ret, name) { 
	load.packages('abind')
	# create matrix
	temp = abind(data, along = 3)
		temp = abind(next.month.ret, temp, along = 3)
		dimnames(temp)[[3]][1] = 'Ret'
	
	# plot
	temp = t(compute.avgcor(temp, 'spearman')[,-1])
		temp[] = plota.format(100 * temp, 0, '', '%')
		plot.table(temp, smain=paste(name,'Correlation',sep=' \n '))
}
			
###############################################################################
# Compute average correlations
#' @export 
###############################################################################
compute.avgcor <- function
(
	data, 		# matrix with data: [rows,cols,factors]
	method = c('pearson', 'kendall', 'spearman')
)
{
	nr = dim(data)[1]
	nc = dim(data)[3]
	corm = matrix(NA,nc,nc)
		colnames(corm) = rownames(corm) = dimnames(data)[[3]]
		
	for( i in 1:(nc-1) ) {
		for( j in (i+1):nc ) {
			corm[i,j] = mean( as.double( sapply(1:nr, function(t) 
				try(cor(data[t,,i], data[t,,j], use = 'complete.obs', method[1]),TRUE)			
				)), na.rm=T)
		}
	}
	return(corm)
}	

###############################################################################
# Compute Market Cap weighted mean
#' @export 
###############################################################################
cap.weighted.mean <- function
(
	data, 	# factor
	capitalization	# market capitalization
) 
{
	capitalization = capitalization * (!is.na(data))
	weight = capitalization / rowSums(capitalization,na.rm=T)	
	rowSums(data * weight,na.rm=T)	
}	

###############################################################################
# Compute factor mean for each sector
#' @export 
###############################################################################
sector.mean <- function
(
	data, 	# factor
	sectors	# sectors
) 
{ 
	out = data * NA
	for(sector in levels(sectors)) {
		index = (sector == sectors)
		out[,index] = ifna(apply(data[,index, drop=F], 1, mean, na.rm=T),NA)
	}
	return(out)
}	
	

###############################################################################
# Create quantiles
# http://en.wikipedia.org/wiki/Quantile
# rank each month stocks according to E/P factor
# create quantiles, and record their performance next month
#' @export 
###############################################################################
compute.quantiles <- function
(
	data, 			# factor
	next.month.ret, # future returns
	smain='', 		# title for plot
	n.quantiles=5, 	# number of quantiles
	plot=T			# flag to create plot
) 
{ 
	n = ncol(data)
	nperiods = nrow(data)
	
	data = coredata(ifna(data,NA))
	next.month.ret = coredata(ifna(next.month.ret,NA))
	
	temp = matrix(NA, nperiods, n.quantiles)
	hist.factor.quantiles = hist.ret.quantiles = temp
	
	temp = matrix(NA, nperiods, n)
	quantiles = weights = ranking = temp
	
	#index = which(rowSums(!is.na(data * next.month.ret)) > n/2)
	#index = which(rowSums(!is.na(data)) > n/2)
	index = which(rowSums(!is.na(data)) >= n.quantiles)
	for(t in index) {
		factor = data[t,]
		ret = next.month.ret[t,]
		
		ranking[t,] = rank(factor, na.last = 'keep','first')
		t.ranking = ceiling(n.quantiles * ranking[t,] / count(factor))
	
		quantiles[t,] = t.ranking
		weights[t,] = 1/tapply(rep(1,n), t.ranking, sum)[t.ranking]
			
		hist.factor.quantiles[t,] = tapply(factor, t.ranking, mean)
		hist.ret.quantiles[t,] = tapply(ret, t.ranking, mean)
	}
	
	# create plot
	if(plot) {
		par(mar=c(4,4,2,1)) 	 	 	
		temp = 100*apply(hist.ret.quantiles,2,mean,na.rm=T)
 		barplot(temp, names.arg=paste(1:n.quantiles), ylab='%', 
 			main=paste(smain, ', spread =',round(temp[n.quantiles]-temp[1],2), '%'))
 	}
 		
 	return(list(quantiles=quantiles, weights=weights, ranking=ranking, 
 		hist.factor.quantiles = hist.factor.quantiles, hist.ret.quantiles = hist.ret.quantiles))
}


###############################################################################
# Create Average factor
#' @export 
###############################################################################
add.avg.factor <- function
(
	data	# factors
) 
{ 
	# compute the overall factor
	temp = abind(data, along = 3)
	data$AVG = data[[1]]
	data$AVG[] = ifna(apply(temp, c(1,2), mean, na.rm=T),NA)
	return(data)	
}

	
###############################################################################
# Convert factor to Z scores, normalize using market capitalization average
#' @export 
###############################################################################
normalize.mkval <- function
(
	data,	# factors
	MKVAL	# capitalization
) 
{ 
	# normalize (convert to z scores) cross sectionaly all factors
	for(i in names(data)) {
		#data[[i]] = (data[[i]] - apply(data[[i]], 1, mean, na.rm=T)) / apply(data[[i]], 1, sd, na.rm=T)
		data[[i]] = (data[[i]] - cap.weighted.mean(data[[i]], MKVAL)) / 
							apply(data[[i]], 1, sd, na.rm=T)
	}
	return(data)	
}
	

###############################################################################
# Convert factor to Z scores, only keep the ranks
#' @export 
###############################################################################
normal.transform <- function(data) 
{	
	rk=rank(data, na.last='keep', ties.method = 'first')
	n = count(data)
	x = qnorm((1:n) / (n+1))
	return(x[rk])
}

#' @export
normalize.normal <- function
(
	data	# factors
)
{ 
	# normalize (convert to z scores) cross sectionaly all factors
	for(i in names(data)) {
		data[[i]][] = t(apply(data[[i]], 1, normal.transform))
	}
	return(data)	
}
	

###############################################################################
# Plot Quantiles
#' @export 
###############################################################################
plot.quantiles <- function
(
	data, 			# factors
	next.month.ret, # future one month returns
	smain=''		# title
) 
{ 
	layout(matrix(1:(2*ceiling(len(data)/2)), nc=2))
	sapply(1:len(data), function(i)
		compute.quantiles(data[[i]], next.month.ret, paste(names(data)[i],smain))
	)	
}

###############################################################################
# Plot Backtest Quantiles and spread (Q5-Q1)
#' @export 
###############################################################################
plot.bt.quantiles <- function
(
	factors,		# factors
	next.month.ret, # future one month returns
	smain='',		# title
	data		 	# data 
) 
{ 
	out = compute.quantiles(factors, next.month.ret, plot=F)	
		
	prices = data$prices
		prices = bt.apply.matrix(prices, function(x) ifna.prev(x))
		
	# find month ends
	month.ends = endpoints(prices, 'months')

	# create strategies that invest in each qutile
	models = list()
	
	for(i in 1:5) {
		data$weight[] = NA
			data$weight[month.ends,] = iif(out$quantiles == i, out$weights, 0)
			capital = 100000
			data$weight[] = (capital / prices) * (data$weight)	
		models[[paste('Q',i,sep='')]] = bt.run(data, type='share', capital=capital)
	}
	
	# spread
	data$weight[] = NA
		data$weight[month.ends,] = iif(out$quantiles == 5, out$weights, 
									iif(out$quantiles == 1, -out$weights, 0))
		capital = 100000
		data$weight[] = (capital / prices) * (data$weight)
	models$Q5_Q1 = bt.run(data, type='share', capital=capital)
	
	#*****************************************************************
	# Create Report
	#****************************************************************** 	
	plotbt(models, plotX = T, log = 'y', LeftMargin = 3, main=smain)	    	
		mtext('Cumulative Performance', side = 2, line = 1)
}
		
		

###############################################################################
# Plot Factors details
#' @export 
###############################################################################
plot.factors <- function
(
	data, 			# factors
	name, 			# name of factor group
	next.month.ret	# future one month returns
)
{ 
	x = as.vector(t(data))
	y = as.vector(t(next.month.ret))
		x = ifna(x,NA)
		y = ifna(y,NA)
		index = !is.na(x) & !is.na(y)
		x = x[index]
		y = y[index]	
			 	
	cor.p = round(100*cor(x, y, use = 'complete.obs', method = 'pearson'),1)
	cor.s = round(100*cor(x, y, use = 'complete.obs', method = 'spearman'),1)
			
	# Plot
	layout(1:2)
	plot(x, pch=20)
		
	par(mar=c(4,4,2,1)) 	 	 	
	plot(x, y, pch=20, xlab=name, ylab='Next Month Return')
		abline(lm(y ~ x), col='blue', lwd=2)
		plota.legend(paste('Pearson =',cor.p,',Spearman =', cor.s))
}		 		
		 		
	