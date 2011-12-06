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
ifna.prev <- function(y)
{ 	
	y1 = !is.na(y)
	
	# in case y starts with NA
	y1[1]=T
	
	return( y[cummax( (1:length(y)) * y1 )]	)
}

###############################################################################
# Percentile Rank over given window
############################################################################### 
percent.rank <- function
(
	data, 	# data
	n=252	# window length
)
{	
	# simple percent rank function
    pctRank <- function(x,i) sum(x[i] >= x[(i- (n-1) ):i])    
 
       
    out = data
    
    # Apply the percent rank function to the coredata of our results
    data = coredata(data)
    rng = n:len(data)
    
    out[] = c( rep(NA,(n-1)), sapply(rng, function(i) pctRank(data, i) / n) )

    return(out)
}


###############################################################################
# DV2 indicator
# http://blog.fosstrading.com/2009/07/david-varadis-rsi2-alternative.html
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
# TSI indicator
# http://engineering-returns.com/tsi/
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
# Sample rotation Strategies
############################################################################### 
# Select top N for each period
# http://www.etfscreen.com/sectorstrategy.php
############################################################################### 
ntop <- function
(
	x, 		# matrix with observations
	n=1, 	# top n
	dirMaxMin = TRUE
) 
{
	out = x 
	out[] = t( apply( coredata(x) , 1, ntop.helper, n) )
	return( out )
}

ntop.helper <- function
(
	x, 		# matrix with observations
	n=1, 	# top n
	dirMaxMin = TRUE
) 
{
	o = sort.list(x, na.last=TRUE, decreasing = dirMaxMin)
	index = which(!is.na(x))
	x[] = 0
		
	if(len(index)>0) {
		n = min(n,len(index))
		x[o[1:n]] = 1/n
	}
	
	return(x) 
}	

############################################################################### 
# Select top N for each period, and keep them till they drop below keepn rank
# http://www.etfscreen.com/sectorstrategy.php
############################################################################### 
ntop.keep <- function
(
	data, 		# matrix with observations
	topn = 1, 	# top n
	keepn = 1, 	# keep n
	dirMaxMin = TRUE
) 
{
	out = data
	out[] = NA
	
	for( i in 1:nrow(data) ) {
		x = coredata(data[i,])
		o = sort.list(x, na.last = TRUE, decreasing = dirMaxMin)
		index = which(!is.na(x))
		x[] = NA
			
		if(len(index)>0) {
			n = min(topn, len(index))
			x[o[1:n]] = 1
		
			# keepn logic
			if( i>=2 ) {
				y = coredata(out[(i-1),])		# prev period selection
				n1 = min(keepn,len(index))
				y[-o[1:n1]] = NA	# remove all not in top keepn
				
				index1 = which(!is.na(y))
				if(len(index1)>0) {
					x[] = NA
					x[index1] = 1
					
					for( j in 1:n ) {
						if( sum(x,na.rm=T) == topn ) break
						x[o[j]] = 1
					}
				}
			}
		}		
		out[i,] = x/sum(x,na.rm=T)	
	}
	out[is.na(out)] = 0
	
	return( out )
}


