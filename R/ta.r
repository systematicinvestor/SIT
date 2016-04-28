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
# Technical Analysis Functions
# Copyright (C) 2011  Michael Kapler
#
# For more information please visit my blog at www.SystematicInvestor.wordpress.com
# or drop me a line at TheSystematicInvestor at gmail
###############################################################################




###############################################################################
# Backfill NA's with last non NA value, similar to na.locf(y,na.rm=FALSE)	
# http://r.789695.n4.nabble.com/Vector-replace-0-elements-without-using-a-loop-td2
############################################################################### 

###############################################################################
#' @export 
###############################################################################
ifna.prev <- function(y)
{ 	
	y1 = !is.na(y)
	
	# in case y starts with NA
	y1[1]=T
	
	return( y[cummax( (1:length(y)) * y1 )]	)
}

# index of non NAs filled from left to right
###############################################################################
#' @export 
###############################################################################
ifna.prevx <- function(y) { 	
	y1 = !is.na(y)
	
	# in case y starts with NA
	y1[1]=T
	
	return( cummax( (1:length(y)) * y1 ) )
}

# index of non NAs filled from right to left
###############################################################################
#' @export 
###############################################################################
ifna.prevx.rev <- function(y) {
	y1 = !is.na(y)
	
	y1[length(y)] = T
	y1[!y1] = Inf
	
    rev(cummin(rev((1:length(y)) * y1)))
}

# test
ifna.prevx.test <- function() {
	y = c(NA,1,1,NA,2,2,NA,NA)

	y[ifna.prevx(y)]
	y[ifna.prevx.rev(y)]
}

###############################################################################
# Cross - gives a "1" or true on the day that ARRAY1 crosses above ARRAY2. Otherwise the result is "0".
# To find out when ARRAY1 crosses below ARRAY2, use the formula cross(ARRAY2, ARRAY1)
# http://www.amibroker.com/guide/afl/afl_view.php?id=34
#' @export 
###############################################################################
cross <- function( array1, array2, eq=F ) {
	iif(eq, array1 >= array2, array1 > array2) & iif(len(array1) > 1, mlag(array1), array1) < iif(len(array2) > 1, mlag(array2), array2)
}

#' @export 
cross.up <- function( array1, array2 ) { cross( array1, array2 ) }


#' @export 
cross.dn <- function( array1, array2 ) { cross( array2, array1 ) }

#' @export 
cross.up.eq <- function( array1, array2 ) { cross( array1, array2, T ) }

#' @export 
cross.dn.eq <- function( array1, array2 ) { cross( array2, array1, T ) }


###############################################################################
# Percentile Rank over given window, works for both single column and matrix
# If data is matrix, lookup value in the first column across all columns
#' @export 
############################################################################### 
percent.rank <- function
(
	data, 	# data
	n=252	# window length
)
{	
	# simple percent rank function
    pctRank <- function(x,i) sum(x[i,1] >= x[(i- (n-1) ):i,])    
        
    out = data
    
    # Apply the percent rank function to the coredata of our results
    data = coredata(data)
    	if( is.null(dim(data)) ) dim(data) = c(len(data),1)
    rng = n:len(data)
    
    out[] = c( rep(NA,(n-1)), sapply(rng, function(i) pctRank(data, i) / n) )

    return(out)
}


###############################################################################
# Percentile Rank over given window, multiple arrays version
#' @export 
############################################################################### 
percent.rankM <- function
(
	..., 	# data
	n = 252	# window length
)
{	
	data = variable.number.arguments( ... )
		
    out = data[[1]]
	
	for(j in 1:len(data)) data[[j]] = coredata(data[[j]])
	
	rank.data = data[[ len(data) ]]
	
	# simple percent rank function
    pctRank <- function(x,i) sum(rank.data[i] >= x[(i- (n-1) ):i])    
 
    
    # Apply the percent rank function to the coredata of our results
    rng = n:len(rank.data)
    
    out[] = 0
    for(j in 1:len(data))
    	out[] = out[] + c( rep(NA,(n-1)), sapply(rng, function(i) pctRank(data[[j]], i) / n) )

    return(out/len(data))
}

###############################################################################
# DV2 indicator (DVB)
# http://blog.fosstrading.com/2009/07/david-varadis-rsi2-alternative.html
# http://davesbrain.blogs.com/mindmoneymarkets/2010/08/dvib-combo-.html
#' @export 
############################################################################### 
DV <- function
(
	HLC, 			# HLC data
	n=2, 			# window length
	bounded=FALSE	# flag to compute percentrank
)
{
	# Calculate each day's high/low mean
  	hlMean = rowMeans( HLC[,-3] )
 
  	# Calculate the running Mean of the Close divided by the high/low mean
  	res = runMean( HLC[,3] / hlMean, n ) - 1
 
  	# If we want the bounded DV...
  	if(bounded) res = percent.rank(res, 252)

  	return(res)
}

###############################################################################
# DVI indicator
# http://cssanalytics.wordpress.com/2010/07/29/dvi-and-spy-performance/
# http://marketsci.wordpress.com/2010/07/29/exploring-the-dvi-indicator-extreme-readings/
# http://marketsci.wordpress.com/2010/07/27/css-analytics%E2%80%99-dvi-indicator-revealed/
# http://dvindicators.cssanalytics.com/community/?vasthtmlaction=viewtopic&t=47.0
# http://quantingdutchman.wordpress.com/2010/07/28/dvi-indicator-for-amibroker/
# http://davesbrain.blogs.com/mindmoneymarkets/2010/08/dvib-combo-.html
#' @export 
############################################################################### 
DVI <- function
(
	x, 				# prices
	n=250 			# window length
)
{
	# Calculate return
	ColumnC = ( x / runMean(x,3) ) - 1	
	ColumnD = ( runMean( ColumnC , 5 ) + ( runMean( ColumnC , 100 ) / 10 ) ) / 2
	ColumnE = runMean( ColumnD , 5 )

	ColumnF = iif( x > mlag(x) , 1 , -1 )
	ColumnG = ( runSum( ColumnF , 10 ) + ( runSum( ColumnF , 100 ) / 10 ) ) / 2
	ColumnH = runMean( ColumnG , 2 )

	DVI.magnitude = percent.rank( ColumnE , n )
	DVI.stretch = percent.rank( ColumnH, n )
	DVI = ( 0.8 * DVI.magnitude ) + ( 0.2 * DVI.stretch )

	return(list(DVI=DVI, DVI.magnitude=DVI.magnitude, DVI.stretch=DVI.stretch))
}


###############################################################################
# TSI indicator
# http://engineering-returns.com/tsi/
#' @export 
############################################################################### 
TSI <- function
(
	HLC, 	# HLC data
	n=10	# window length
)
{
	HLC = apply(HLC, 2, ifna.prev)

	ratio = ( HLC[,3] - mlag(HLC[,3], n) ) / ATR( HLC , n )[, "atr"]
	out = SMA( SMA( ratio , n ), 100 )
	return(out)
}



###############################################################################
# Ulcer Index: alternative to Standard Devation
# http://en.wikipedia.org/wiki/Ulcer_index
# http://stockcharts.com/school/doku.php?id=chart_school:technical_indicators:ulcer_index
#' @export 
############################################################################### 
ulcer.index <- function
(
	x, 		# prices
	n=14	# window length
)
{
	#sqrt(runSum((100*( x - runMax(x,n) ) / runMax(x,n))^2, n) / n)
	sqrt(runSum(( x / runMax(x,n) -1 )^2, n) / n)	
}


###############################################################################    
# Rolling EV Ratio: A Trend Indicator or Performance Measurement Statistic
# cumulative W% (up periods/total periods) x W/L ratio (sum of wins/sum of losses)            
# http://cssanalytics.wordpress.com/2010/06/02/rolling-ev-ratio-a-trend-indicator-or-performance-measurement-statistic/    
# http://davesbrain.blogs.com/mindmoneymarkets/2010/07/will-mean-reversion-bounce-back.html
#' @export 
###############################################################################
ev.ratio <- function
(
	data, 	# data
	n = 252	# window length
)
{
	ret = coredata(data / mlag(data) - 1)
		
    rng = n:len(data)    
	    
    out = data
   	out[] = c( rep(NA,(n-1)), sapply(rng, 
   		function(i) {
			r = ret[(i- (n-1) ):i]
    		-sum(r > 0) / n * sum(r[r > 0]) / sum(r[r < 0])
    	}))
	return(out)
}


###############################################################################
# Sample rotation Strategies
############################################################################### 
# Select top N for each period
# http://www.etfscreen.com/sectorstrategy.php
#' @export 
############################################################################### 
ntop <- function
(
	data,		# matrix with observations
	topn = 1, 	# top n
	dirMaxMin = TRUE
) 
{
	# work with matrix
	temp = coredata(data)
	if(is.logical(temp)) temp[] = iif(!temp,NA,temp)
	
	# equal weight all assets special case
	if(topn == ncol(data)) {
		index = is.na(temp)
			temp[index] = 0
			temp[!index] = 1
				
		out = data
		out[] = ifna(temp / rowSums(temp),0)		
		return( out )
	}
	
	index.n = rowSums(!is.na(temp))
	for( i in 1:nrow(data) ) {
		if( index.n[i] > 0 ) {
			o = sort.list(temp[i,], na.last = TRUE, decreasing = dirMaxMin)
			temp[i,] = 0
			n = min(topn, index.n[i])
			temp[i,o[1:n]] = 1/n
		} else temp[i,] = 0
	}
	
	# work with xts
	out = data
	out[] = temp		
	out
}


#' @export 
ntop.helper <- function
(
	x, 		# matrix with observations
	n=1, 	# top n
	dirMaxMin = TRUE
) 
{
	x = as.vector(x)
	index.n = sum(!is.na(x))	
		
	if( index.n > 0 ) {
		o = sort.list(x, na.last=TRUE, decreasing = dirMaxMin)
		x[] = 0	
		n = min(n, index.n)
		x[o[1:n]] = 1/n
	} else x[] = 0
	
	x
}	

ntop.speed.test <- function()
{
	#to.monthly(IEF, indexAt='endof')
	#IEF = adjustOHLC(IEF, use.Adjusted=TRUE)
	
	
	
	load.packages('quantmod')	
	tickers = spl('XLY,XLP,XLE,XLF,XLV,XLI,XLB,XLK,XLU,IWB,IWD,IWF,IWM,IWN,IWO,IWP,IWR,IWS,IWV,IWW,IWZ')	

	data <- new.env()
	getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
	bt.prep(data, align='keep.all', dates='1970::2011')

	prices = data$prices  
	n = len(tickers)  

	a = coredata(prices)
	b = a
	c = a
	
tic(12)
	for( i in 1:nrow(b) ) {
		b[i,] = ntop.helper(b[i,], n, T)
	}
toc(12)

	# working directly with xts is alot slower
tic(12)
	d = prices
	for( i in 1:nrow(c) ) {
		d[i,] = ntop.helper(c[i,], n, T)
	}
toc(12)

	range(b-d)	

	

}

############################################################################### 
# Select top N for each period, and keep them till they drop below keepn rank
# http://www.etfscreen.com/sectorstrategy.php
#' @export 
############################################################################### 
ntop.keep <- function
(
	data, 		# matrix with observations
	topn = 1, 	# top n
	keepn = 1, 	# keep n
	dirMaxMin = TRUE
) 
{
	# work with matrix
	temp = coredata(data)
	if(is.logical(temp)) temp[] = iif(!temp,NA,temp)
	index.n = rowSums(!is.na(temp))
		
	for( i in 1:nrow(temp) ) {
		if( index.n[i] > 0 ) {
			x = temp[i,]
			o = sort.list(x, na.last = TRUE, decreasing = dirMaxMin)
			x[] = 0
			n = min(topn, index.n[i])
			x[o[1:n]] = 1
		
			# keepn logic
			if( i >= 2 ) {
				y = temp[(i-1),]		# prev period selection
				n1 = min(keepn, index.n[i])
				y[-o[1:n1]] = 0		# remove all not in top keepn
				
				index1 = y != 0
					index1.n = sum(index1)	
				if( index1.n > 0 ) {
					x[] = 0
					x[index1] = 1
					
					for( j in 1:n ) {
						if( sum(x) == topn ) break
						x[o[j]] = 1
					}
				}
			}
			temp[i,] = x/sum(x)	
		} else temp[i,] = 0		
	}
	
	# work with xts
	out = data
	out[] = temp		
	out
}


############################################################################### 
# Rank observations in each row
#' @export 
############################################################################### 
br.rank <- function(x)
{	
	t(apply(coredata(-x), 1, rank, na.last='keep'))
}

#' @export 
bt.rank <- function(x, dirMaxMin = TRUE, do.sort = F )
{	
res = NA * x
res[] = {
	if(!do.sort) {
		if(dirMaxMin)
			t(apply(coredata(-x), 1, rank, na.last='keep'))
		else
			t(apply(coredata(x), 1, rank, na.last='keep'))
	} else {
		index = 1:ncol(x)
		x = coredata(x)
		out = t(apply(x, 1, function(y) { 
			temp = sort.list(y, na.last = TRUE, decreasing = dirMaxMin)
			temp[temp] = index
			temp
			}
		)) 
		out[is.na(x)] = NA
		out
	}
}
res	
}	

############################################################################### 
# SuperSmoother filter 2013 John F. Ehlers
# http://www.stockspotter.com/files/PredictiveIndicators.pdf
#' @export 
############################################################################### 
super.smoother.filter <- function(x) {
    a1 = exp( -1.414 * pi / 10.0 )
    b1 = 2.0 * a1 * cos( (1.414*180.0/10.0) * pi / 180.0 )
    c2 = b1
    c3 = -a1 * a1
    c1 = 1.0 - c2 - c3

    x = c1 * (x + mlag(x)) / 2
        x[1] = x[2]
 
    out = x * NA
        out[] = filter(x, c(c2, c3), method='recursive', init=c(0,0))
    out
}

# out = 0*x
# for(i in 3:len(x))
#    out[i] = c1 * (x[i] + x[(i-1)])/2 + c2* out[(i-1)]+ c3* out[(i-2)]     


# Roofing filter 2013 John F. Ehlers
#' @export 
roofing.filter <- function(x) {
    # Highpass filter cyclic components whose periods are shorter than 48 bars
    alpha1 = (cos((0.707*360 / 48) * pi / 180.0 ) + sin((0.707*360 / 48) * pi / 180.0 ) - 1) / cos((0.707*360 / 48) * pi / 180.0 )
    
    x = (1 - alpha1 / 2)*(1 - alpha1 / 2)*( x - 2*mlag(x) + mlag(x,2))
        x[1] = x[2] = x[3]
    
    HP = x * NA
    HP[] = filter(x, c(2*(1 - alpha1), - (1 - alpha1)*(1 - alpha1)), method='recursive', init=c(0,0))
    
    super.smoother.filter(HP)
}

# My Stochastic Indicator 2013 John F. Ehlers
#' @export 
roofing.stochastic.indicator  <- function(x, lookback = 20) {
    Filt = roofing.filter(x)
    
    HighestC = runMax(Filt, lookback)
        HighestC[1:lookback] = as.double(HighestC[lookback])
    LowestC = runMin(Filt, lookback)
        LowestC[1:lookback] = as.double(LowestC[lookback])
    Stoc = (Filt - LowestC) / (HighestC - LowestC)

    super.smoother.filter(Stoc)
}

# Quantile over moving window
#' @export 
runQuantile = function(x, k, probs) {
	load.packages('caTools')
	temp = rep.col(x * NA, len(probs))
	temp[k:len(x),] = runquantile(as.vector(coredata(x)), k, probs, endrule='trim')    
	temp
}
