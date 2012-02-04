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
# Create plot of factors average correlations and returns
###############################################################################
# http://stackoverflow.com/questions/4310727/what-is-rs-multidimensional-equivalent-of-rbind-and-cbind
# apply(temp, 3, rbind)
# http://r.789695.n4.nabble.com/Collapse-an-array-td850008.html
factor.avgcor <- function(data, next.month.ret) { 
	load.packages('abind')
	# create matrix
	temp = abind(data, along = 3)
		temp = abind(next.month.ret, temp, along = 3)
		dimnames(temp)[[3]][1] = 'Ret'
	
	# plot
	temp = t(compute.avgcor(temp, 'spearman')[,-1])
		temp[] = plota.format(100 * temp, 0, '', '%')
		plot.table(temp, smain='Correlation')
}
			
###############################################################################
# Compute average correlations
###############################################################################
compute.avgcor <- function
(
	data, 		# matrix with data: [rows,cols,factors]
	method = c("pearson", "kendall", "spearman")
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
###############################################################################
cap.weighted.mean <- function(data, capitalization) {
	capitalization = capitalization * (!is.na(data))
	weight = capitalization / rowSums(capitalization,na.rm=T)	
	rowSums(data * weight,na.rm=T)	
}	

###############################################################################
# Compute factor mean for each sector
###############################################################################
sector.mean <- function(data, sectors) { 
	out = data * NA
	for(sector in levels(sectors)) {
		index = (sector == sectors)
		out[,index] = ifna(apply(data[,index, drop=F], 1, mean, na.rm=T),NA)
	}
	return(out)
}	
	

###############################################################################
# Create quintiles
# http://en.wikipedia.org/wiki/Quantile
# rank each month stocks according to E/P factor
# create quintiles, and record their performance next month
###############################################################################
compute.quintiles <- function(data, next.month.ret, smain='', plot=T) { 
	n = ncol(data)
	nperiods = nrow(data)
	
	data = coredata(ifna(data,NA))
	next.month.ret = coredata(ifna(next.month.ret,NA))
	
	temp = matrix(NA, nperiods, 5)
	hist.factor.quintiles = hist.ret.quintiles = temp
	
	temp = matrix(NA, nperiods, n)
	quintiles = weights = temp
	
	index = which(rowSums(!is.na(data * next.month.ret)) > n/2)
	for(t in index) {
		factor = data[t,]
		ret = next.month.ret[t,]
		
		ranking = ceiling(5 * rank(factor, na.last = 'keep') / count(factor))
	
		quintiles[t,] = ranking
		weights[t,] = 1/tapply(rep(1,n), ranking, sum)[ranking]
			
		hist.factor.quintiles[t,] = tapply(factor, ranking, mean)
		hist.ret.quintiles[t,] = tapply(ret, ranking, mean)
	}
	
	if(plot) {
		par(mar=c(4,4,2,1)) 	 	 	
		temp = 100*apply(hist.ret.quintiles,2,mean,na.rm=T)
 		barplot(temp, names.arg=paste(1:5), ylab='%', 
 			main=paste(smain, ', spread =',round(temp[5]-temp[1],2), '%'))
 	}
 		
 	return(list(quintiles=quintiles, weights=weights))
}


