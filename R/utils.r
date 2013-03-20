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
# Collection of General Utilities
# Copyright (C) 2012  Michael Kapler
#
# For more information please visit my blog at www.SystematicInvestor.wordpress.com
# or drop me a line at TheSystematicInvestor at gmail
###############################################################################


###############################################################################
# Convenience Utilities
###############################################################################
#' Split string into tokens using delim
#'
#' This function will split given string into tokens using delim
#'
#' @param s input string
#' @param delim delimiter, \strong{defaults to ","}
#'
#' @return array of tokens
#'
#' @examples
#' \dontrun{ 
#' spl('a,b,c')
#' }
#' @export 
###############################################################################
spl <- function 
(
	s,			# input string
	delim = ','	# delimiter
)
{ 
	return(unlist(strsplit(s,delim))); 
}

###############################################################################
#' Join vector of strings into one string using delim
#'
#' This function will join vector of strings into one string using delim
#'
#' @param v vector of strings
#' @param delim delimiter, \strong{defaults to ","}
#'
#' @return resulting string
#'
#' @examples
#' \dontrun{ 
#' join(c('a','b','c'), ',')
#' }
#' @export 
###############################################################################
join <- function
(
	v, 			# vector of strings
	delim = ''	# delimiter
)
{ 
	return(paste(v,collapse=delim)); 
}

###############################################################################
#' Remnove any leading and trailing spaces
#'
#' This function will remnove any leading and trailing spaces
#'
#' @param s string
#'
#' @return resulting string
#'
#' @examples
#' \dontrun{ 
#' trim('  a b c  ')
#' }
#' @export 
###############################################################################
trim <- function
(
	s	# string
)
{
  s = sub(pattern = '^ +', replacement = '', x = s)
  s = sub(pattern = ' +$', replacement = '', x = s)
  return(s)
}

###############################################################################
#' Shortcut for length function
#'
#' This function is a shortcut for length function
#'
#' @param x vector / string / matrix
#'
#' @return number of elements in x
#'
#' @examples
#' \dontrun{ 
#' len(1:10)
#' }
#' @export 
###############################################################################
len <- function
(
	x	# vector
)
{
	return(length(x)) 
}

###############################################################################
#' Faster version of ifelse function
#'
#' This function is a faster version of ifelse function
#'
#' @param cond true / false condition
#' @param truepart resulting value(s) if condition is true
#' @param falsepart resulting value(s) if condition is false
#'
#' @return number of elements in x
#'
#' @examples
#' \dontrun{ 
#' iif(1:10 > 5, 1, 1:10)
#' }
#' @export 
###############################################################################
iif <- function
(
	cond,		# condition
	truepart,	# true part
	falsepart	# false part
)
{
	if(len(cond) == 1) { if(cond) truepart else falsepart }
	else {  
		if(length(falsepart) == 1) {
			temp = falsepart
			falsepart = cond
			falsepart[] = temp
		}
		
		if(length(truepart) == 1) 
			falsepart[cond] = truepart 
		else {
			cond = ifna(cond,F)
			falsepart[cond] = truepart[cond]
		}
			
		#falsepart[!is.na(cond)] = temp

		return(falsepart);
	}
} 

###############################################################################
#' Replace NA, NaN, Inf values
#'
#' This function will replace all NA, NaN, Inf with given values
#'
#' @param x data to check for NA, NaN, Inf
#' @param y values(s) to be used in place of NA, NaN, Inf
#'
#' @return updated data
#'
#' @examples
#' \dontrun{ 
#' ifna(c(1,NA,2,Inf,3), 4)
#' }
#' @export 
###############################################################################
ifna <- function
(
	x,	# check x for NA, NaN, Inf
	y	# if found replace with y
) { 	
	return(iif(is.na(x) | is.nan(x) | is.infinite(x), y, x))
}


###############################################################################
#' Replace NULL values
#'
#' This function will replace all NULL with given value
#'
#' @param x data to check for NULL
#' @param y values to be used in place of NULL
#'
#' @return updated data
#'
#' @examples
#' \dontrun{ 
#' temp = list()
#' temp$val1 = ifnull(temp$val1, 4)
#' }
#' @export 
############################################################################### 
ifnull <- function
(
	x,	# check x for NULL
	y	# if found replace with y
) { 	
	return(iif(is.null(x), y, x))
}



###############################################################################
#' Faster version of rep fucntion
#'
#' This function is a faster version of rep fucntion
#'
#' @param x data to be repeated
#' @param times number of times to repeat the data
#'
#' @return new data
#'
#' @examples
#' \dontrun{ 
#' fast.rep(c(1,NA,2,Inf,3), 4)
#' }
#' @export 
###############################################################################
fast.rep <- function(x, times) { 
	length(x) = times
	x[] = x[1]		
	x
}

fast.rep.test.speed <- function() {
	#install.packages('rbenchmark_0.3.tar.gz', repos = NULL, type="source")

	test1 <- function() {
		rep(101,10000)
	}
	test2 <- function() {
		fast.rep(101,10000)
	}
		
	library(rbenchmark)
	 benchmark(
	     test1(), 
	     test2(),
	     columns = c("test", "replications", "elapsed", "relative"),
	     order = "relative",
	     replications = 10000
	 )
}	


###############################################################################
#' Count number of non NA elements
#'
#' This function will count number of non NA elements in the given matrix
#'
#' @param x data matrix
#' @param side margin along which to count
#'
#' @return counts
#'
#' @examples
#' \dontrun{ 
#' count(matrix(c(1,NA,2,3),2,2))
#' }
#' @export 
###############################################################################
count <- function(
	x,			# matrix with data
	side = 2	# margin along which to count
)
{
	if( is.null(dim(x)) ) { 
		sum( !is.na(x) ) 
	} else { 
		apply(!is.na(x), side, sum) 
	}
}  

###############################################################################
#' Running Count over given window
#'
#' This function will count number of non NA elements over given window
#'
#' @param x data matrix
#' @param window.len window length
#'
#' @return counts
#'
#' @examples
#' \dontrun{ 
#' run.count(matrix(1:9,3,3),2)
#' }
#' @export 
###############################################################################
run.count <- function
(
	x, 			# vector with data
	window.len	# window length
)
{ 
	n    = length(x) 
	xcount = cumsum( !is.na(x) )
	ycount = xcount[-c(1 : (k-1))] - c(0, xcount[-c((n-k+1) : n)])
	return( c( xcount[1:(k-1)], ycount))
}

###############################################################################
#' Dates Functions
#'
#' @param dates collection of dates
#'
#' @return transformed dates
#'
#' @examples
#' \dontrun{ 
#' date.dayofweek(Sys.Date())
#' }
#' @export 
#' @rdname DateFunctions
###############################################################################
date.dayofweek <- function(dates) 
{	
	return(as.double(format(dates, '%w')))
}

#' @export 
#' @rdname DateFunctions
date.day <- function(dates) 
{	
	return(as.double(format(dates, '%d')))
}

#' @export 
#' @rdname DateFunctions
date.week <- function(dates) 
{	
	return(as.double(format(dates, '%U')))
}
 
#' @export 
#' @rdname DateFunctions
date.month <- function(dates) 
{	
	return(as.double(format(dates, '%m')))
}

#' @export 
#' @rdname DateFunctions
date.year <- function(dates) 
{	
	return(as.double(format(dates, '%Y')))
}


###############################################################################
#' Dates Index Functions
#'
#' @param dates collection of dates
#'
#' @return location of the week/month/year ends
#'
#' @examples
#' \dontrun{ 
#' date.week.ends(seq(Sys.Date()-100, Sys.Date(), 1))
#' }
#' @export 
#' @rdname DateFunctionsIndex
###############################################################################
date.week.ends <- function(dates) 
{	
	return( unique(c(which(diff( 100*date.year(dates) + date.week(dates) ) != 0), len(dates))) )
}

#' @export 
#' @rdname DateFunctionsIndex
date.month.ends <- function(dates) 
{	
	return( unique(c(which(diff( 100*date.year(dates) + date.month(dates) ) != 0), len(dates))) )
}

#' @export 
#' @rdname DateFunctionsIndex
date.year.ends <- function(dates) 
{	
	return( unique(c(which(diff( date.year(dates) ) != 0), len(dates))) )
}


###############################################################################
#' Map given time series to monthly
#'
#' If frequency of observations in the given time series is less than monthly,
#' i.e. quaterly or annually, properly align this time series to monthly
#'
#' @param equity time series
#'
#' @return xts object 
#'
#' @examples
#' \dontrun{ 
#' map2monthly(equity) 
#' }
#' @export 
###############################################################################
map2monthly <- function(equity) 
{
	#a = coredata(Cl(to.monthly(equal.weight$equity)))

	if(compute.annual.factor(equity) >= 12) return(equity)
	
	dates = index(equity)
	equity = coredata(equity)

	temp = as.Date(c('', 10000*date.year(dates) + 100*date.month(dates) + 1), '%Y%m%d')[-1]
	new.dates = seq(temp[1], last(temp), by = 'month')		
	
	map = match( 100*date.year(dates) + date.month(dates), 100*date.year(new.dates) + date.month(new.dates) ) 
	temp = rep(NA, len(new.dates))
	temp[map] = equity
	
	#range(a - temp)
	
	return( make.xts( ifna.prev(temp), new.dates) )
}


###############################################################################
#' Create monthly table
#'
#' Transform given monthly time series into matrix with Months as columns and Years as rows
#'
#' @param monthly.data monthly time series
#'
#' @return matrix with Months as columns and Years as rows
#'
#' @examples
#' \dontrun{ 
#' create.monthly.table(monthly.ret)
#' }
#' @export 
###############################################################################
create.monthly.table <- function(monthly.data) 
{
	nperiods = nrow(monthly.data)
	
	years = date.year(index(monthly.data[c(1,nperiods)]))
		years = years[1] : years[2]

	# create monthly matrix
	temp = matrix( double(), len(years), 12)
		rownames(temp) = years
		colnames(temp) = spl('Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec')
	
	# align months
	index = date.month(index(monthly.data[c(1,nperiods)]))
	temp[] = matrix( c( rep(NA, index[1]-1), monthly.data, rep(NA, 12-index[2]) ), ncol=12, byrow = T)
		
	return(temp)
}
		

###############################################################################
#' Compute the expiration date of stock options (3rd Friday of the month)
#'
#' @param year year
#' @param month month
#'
#' @return date for the third Friday of the given month and year
#'
#' @references 
#' \url{http://bytes.com/topic/python/answers/161147-find-day-week-month-year}
#'
#' \url{http://www.mysmp.com/options/options-expiration-week.html}
#' The week beginning on Monday prior to the Saturday of options expiration is referred to as options expiration week. 
#' Since the markets are closed on Saturday, the third Friday of each month represents options expiration.
#' If the third Friday of the month is a holiday, all trading dates are moved forward; meaning that Thursday will be the last trading day to exercise options.
#'
#' \url{http://www.cboe.com/TradTool/ExpirationCalendar.aspx}
#'
#' @examples
#' \dontrun{ 
#' third.friday.month(2012,1)
#' }
#' @export 
###############################################################################
third.friday.month <- function(year, month)
{
	day = date.dayofweek( as.Date(c('', 10000*year + 100*month + 1), '%Y%m%d')[-1] )
	day = c(20,19,18,17,16,15,21)[1 + day]
	return(as.Date(c('', 10000*year + 100*month + day), '%Y%m%d')[-1])
}


###############################################################################
#' Determine the index of subset of dates in the time series
#'
#' @param x xts time series
#' @param dates string represnting subset of dates i.e. '2010::2012'
#'
#' @return index of subset of dates in the time series
#'
#' @examples
#' \dontrun{ 
#' dates2index(data$prices, '2010::2012') 
#' }
#' @export 
###############################################################################
dates2index <- function( x, dates = 1:nrow(x) ) {
	dates.index = dates
	if(!is.numeric(dates)) {
		temp = x[,1]
		temp[] = 1:nrow(temp)
		dates.index = as.numeric(temp[dates])
	}
	return(dates.index)
} 


###############################################################################
#' Load Packages that are available and install ones that are not available
#'
#' This function a convience wrapper for install.packages() function
#'
#' @param packages names of the packages separated by comma
#' @param repos default repository
#' @param dependencies type of dependencies to install
#' @param ... additional parameters for the \code{\link{install.packages}} function
#'
#' @return nothing
#'
#' @examples
#' \dontrun{ 
#' load.packages('quantmod')
#' }
#' @export 
############################################################################### 
load.packages <- function
(
	packages, 							# names of the packages separated by comma
	repos = "http://cran.r-project.org",# default repository
	dependencies = c("Depends", "Imports"),	# install dependencies
	...									# other parameters to install.packages
)
{
	packages = spl(packages)
	for( ipackage in packages ) {
		if(!require(ipackage, quietly=TRUE, character.only = TRUE)) {
			install.packages(ipackage, repos=repos, dependencies=dependencies, ...) 
			
			if(!require(ipackage, quietly=TRUE, character.only = TRUE)) {
				stop("package", sQuote(ipackage), 'is needed.  Stopping')
			}
		}
	}
}


###############################################################################
#' Begin Timing
#'
#' @param identifier name for this timing session
#'
#' @return nothing
#'
#' @examples
#' \dontrun{ 
#' tic(1)
#' }
#' @export 
#' @rdname TimingFunctions
############################################################################### 
tic <- function
(
	identifier	# integer value
)
{
	assign(paste('saved.time', identifier, sep=''), proc.time()[3], envir = .GlobalEnv)
}


###############################################################################
#' End Timing and report elapsed time
#'
#' @param identifier name for this timing session
#'
#' @return elapsed time
#'
#' @examples
#' \dontrun{ 
#' toc(1)
#' }
#' @export 
#' @rdname TimingFunctions
############################################################################### 
toc <- function
(
	identifier	# integer value
)
{
	if( exists(paste('saved.time', identifier, sep=''), envir = .GlobalEnv) ) {
	    prevTime = get(paste('saved.time', identifier, sep=''), envir = .GlobalEnv)
    	diffTimeSecs = proc.time()[3] - prevTime
    	cat('Elapsed time is', round(diffTimeSecs, 2), 'seconds\n')
    } else {
    	cat('Toc error\n')
    }    
    return (paste('Elapsed time is', round(diffTimeSecs,2), 'seconds', sep=' '))
}

test.tic.toc <- function()
{
	tic(10)
	for( i in 1 : 100 ) {
		temp = runif(100)
	}
	toc(10)
}







###############################################################################
#' Lag matrix or vector
#'
#' This function shifts elemnts in a vector or a mtrix by a given lag.
#' For example: mlag(x,1) - use yesterday's values and
#'  mlag(x,-1) - use tomorrow's values
#'
#' @param x vector / matrix
#' @param nlag number of lags, \strong{defaults to 1}
#'
#' @return modified object
#'
#' @examples
#' \dontrun{ 
#' mlag(1:10)
#' }
#' @export 
###############################################################################
mlag <- function
(
	m,			# matrix or vector
	nlag = 1	# number of lags
)
{ 
	# vector
	if( is.null(dim(m)) ) { 
		n = len(m)
		if(nlag > 0) {
			m[(nlag+1):n] = m[1:(n-nlag)]
			m[1:nlag] = NA
		} else if(nlag < 0) {
			m[1:(n+nlag)] = m[(1-nlag):n]
			m[(n+nlag+1):n] = NA
		} 	
		
	# matrix	
	} else {
		n = nrow(m)
		if(nlag > 0) {
			m[(nlag+1):n,] = m[1:(n-nlag),]
			m[1:nlag,] = NA
		} else if(nlag < 0) {
			m[1:(n+nlag),] = m[(1-nlag):n,]
			m[(n+nlag+1):n,] = NA
		} 
	}
	return(m);
}


###############################################################################
#' Replicate and tile a given vector
#'
#' @param v vector
#' @param n number of copies along rows
#' @param m number of copies along columns
#'
#' @return new matrix
#' 
#' @references 
#' \url{http://www.mathworks.com/help/techdoc/ref/repmat.html}
#'
#' @examples
#' \dontrun{ 
#' repmat(1:10,1,2)
#' }
#' @export 
###############################################################################
repmat <- function
(
	v,	# vector
	n,	# number of copies along rows
	m	# number of copies along columns
)
{
	kronecker( matrix(1, n, m), v )
}


###############################################################################
#' Repeat Rows
#'
#' @param m vector (row)
#' @param nr number of copies along rows
#'
#' @return new matrix
#' 
#' @examples
#' \dontrun{ 
#' matrix(1:3, nr=5, nc=3, byrow=T)
#' repRow(1:3, 5)
#' }
#' @export 
###############################################################################
repRow <- function
(
	m, # vector (row)
	nr # number of copies along rows
)
{
	matrix(m, nr=nr, nc=len(m), byrow=T)
}


###############################################################################
#' Repeat Rows
#'
#' @param m vector (column)
#' @param nc number of copies along columns
#'
#' @return new matrix
#' 
#' @examples
#' \dontrun{ 
#' matrix(1:5, nr=5, nc=3, byrow=F)
#' repCol(1:5, 3)
#' }
#' @export 
###############################################################################
repCol <- function
(
	m,	# vector (column)
	nc	# number of copies along columns
)
{
	matrix(m, nr=len(m), nc=nc, byrow=F)
}




###############################################################################
#' Find location: row, col in the matrix, given index of of observation
#'
#' @param data matrix
#' @param i index of observations
#' @param details flag to provide details, \strong{defaults to FALSE}
#'
#' @return new matrix
#' 
#' @examples
#' \dontrun{ 
#' data = matrix(1:16,4,4)
#' lookup.index(data, which(data > 4))
#' }
#' @export 
# play with following example: update 1 %% 4	
###############################################################################
lookup.index <- function
(
	data, 	# matrix
	i, 		# index of observations
	details = F	# flag to return additional details
) 
{
	n = nrow(data)
	irow = ((i - 1) %% n) + 1	
	icol = ((i - 1) %/% n) +1 
	if(details)
		list(irow=irow,icol=icol,obs=data[irow,icol],obsr=data[max(0,irow-5):min(nrow(data),irow+5),icol])
	else
		list(irow=irow,icol=icol)
}


###############################################################################
#' Convert beta (slope of reggression line) to degrees
#'
#' @param beta slope of regression line
#'
#' @return angle
#' 
#' @references 
#' \url{http://r.789695.n4.nabble.com/slope-calculation-td858652.html	}
#'
#' @examples
#' \dontrun{ 
#' beta.degree(1)
#' }
#' @export 
###############################################################################
beta.degree <- function(beta) 
{ 
	atan(beta)*360/(2*pi) 
}


###############################################################################
# XTS helper functions
###############################################################################

# must set timezone before any calls to xts
Sys.setenv(TZ = 'GMT')
#Sys.setenv(TZ = 'EST')

###############################################################################
#' The timezone is set to 'GMT' by defult
#'
#' The reason for setting the default timezone is because the following code 
#' produces different results if the timezone is NOT set and if timezone has a value.
#' 
#' @examples
#' \dontrun{ 
# 
#' # We want to set the timezone, so that following code produces expected results
#' Sys.getenv('TZ')
#' test = as.POSIXct('2012-10-31', format='%Y-%m-%d')
#'	as.numeric(test)
#'	as.numeric(as.POSIXct(as.Date(test)))
#' as.numeric(as.POSIXct(as.Date(test))) - as.numeric(test)
#' test == as.POSIXct(as.Date(test))
#'
#' # Set Time Zone
#' Sys.setenv(TZ = 'GMT')
#' Sys.getenv('TZ')
#' test = as.POSIXct('2012-10-31', format='%Y-%m-%d')
#'	as.numeric(test)
#'	as.numeric(as.POSIXct(as.Date(test)))
#' as.numeric(as.POSIXct(as.Date(test))) - as.numeric(test)
#' test == as.POSIXct(as.Date(test))
#'
#' }
#' @export 
#' @rdname XTSFunctions
###############################################################################
XTSFunctions <- function() {}


###############################################################################
#' Create \code{\link{xts}} object, faster version of \code{\link{xts}} fucntion
#'
#' @param x vector / matrix / data frame
#' @param order.by dates that correspond to rows of x
#'
#' @return \code{\link{xts}} object
#' 
#' @examples
#' \dontrun{ 
#' make.xts(1:101,seq(Sys.Date()-100, Sys.Date(), 1))
#' }
#' @export 
###############################################################################
make.xts <- function
(
	x,			# data
	order.by	# date
)
{
	#Sys.setenv(TZ = 'GMT')
	tzone = Sys.getenv('TZ')
	
    orderBy = class(order.by)
    index = as.numeric(as.POSIXct(order.by, tz = tzone))
    if( is.null(dim(x)) ) dim(x) = c(len(x), 1)
    x = as.matrix(x)

    x = structure(.Data = x, 
    	index = structure(index, tzone = tzone, tclass = orderBy), 
    	class = c('xts', 'zoo'), .indexCLASS = orderBy, tclass=orderBy, .indexTZ = tzone, tzone=tzone)
	return( x )
}


###############################################################################
#' Write \code{\link{xts}} object to file
#'
#' @param x \code{\link{xts}} object
#' @param filename file name
#' @param append flag to inidicate if file is overwritten or appended, \strong{defaults to FALSE}
#' @param ... additional paramaeters to the \code{\link{format}} function
#'
#' @return nothing
#' 
#' @examples
#' \dontrun{ 
#' write.xts(make.xts(1:101,seq(Sys.Date()-100, Sys.Date(), 1)), 'temp.csv')
#' }
#' @export 
###############################################################################
write.xts <- function
(
	x,			# XTS object
	filename,	# file name
	append = FALSE,	
	...
)
{
	cat('Date', file = filename, append = append)

	write.table(x, sep=',',  row.names = format(index(x), ...), 
		col.names = NA, file = filename, append = T, quote = F)
	#write.csv(x, row.names = format(index(x)), filename)	
}


###############################################################################
#' Read \code{\link{xts}} object from file
#'
#' @param filename file name
#' @param date.fn function to preprocess string dates, \strong{defaults to \code{\link{paste}} - i.e. no preprocessing}
#' @param index.class class of the date object, \strong{defaults to 'Date'}
#' @param ... additional paramaeters to the \code{\link{as.POSIXct}} function
#'
#' @return \code{\link{xts}} object
#' 
#' @examples
#' \dontrun{ 
#' write.xts(make.xts(1:101,seq(Sys.Date()-100, Sys.Date(), 1)), 'temp.csv')
#' read.xts('temp.csv')
#' }
#' @export 
###############################################################################
read.xts <- function
(
	filename,	# file name
	date.fn = paste,
	index.class = 'Date',
	...
)
{
	out = read.csv(filename, stringsAsFactors=F)
	#return( make.xts(out[,-1,drop=F], as.Date(out[,1], ...)) )
	out = make.xts(out[,-1,drop=F], as.POSIXct(match.fun(date.fn)(out[,1]), tz = Sys.getenv('TZ'), ...))
		indexClass(out) = index.class
	return( out )

# Example code from	getSymbols.yahoo (quantmod): as.POSIXct is used to avoid Dates conversion problems
# fr = xts(1, as.POSIXct('2012-10-31', tz = Sys.getenv("TZ"), format='%Y-%m-%d'),  src = "yahoo", updated = Sys.time())
# indexClass(fr) = "Date"	
}


###############################################################################
#' Fast alternative to index() function for \code{\link{xts}} object
#'
#' NOTE index.xts is the same name as the index function in the XTS package
#'
#' @param x \code{\link{xts}} object
#'
#' @return dates
#' 
#' @examples
#' \dontrun{ 
#' index.xts(make.xts(1:101,seq(Sys.Date()-100, Sys.Date(), 1)))
#' }
#' @export 
###############################################################################
# maybe rename to bt.index.xts
index.xts <- function
(
	x			# XTS object
)
{
	temp = attr(x, 'index')
	class(temp) = c('POSIXct', 'POSIXt')
	
    type = attr(x, '.indexCLASS')[1]
    if( type == 'Date' || type == 'yearmon' || type == 'yearqtr')
		temp = as.Date(temp)
	return(temp)
}


# other variants that are not currently used
# this function is used in plota for X axis
index4xts <- function
(
	x			# XTS object
)
{
	temp = attr(x, 'index')
	class(temp)='POSIXct' 
	
	return(temp)
}

index2date.time <- function(temp) {
	class(temp)='POSIXct' 
	
	if( attr(x, '.indexCLASS')[1] == 'Date') {	
		as.Date(temp)
	} else {
		as.POSIXct(temp, tz = Sys.getenv('TZ'))
	}
}


###############################################################################
#' File name Functions
#'
#' @param x file name
#'
#' @return part of the file name
#'
#' @examples
#' \dontrun{ 
#' get.extension('test.csv')
#' }
#' @export 
#' @rdname FilenameFunctions
###############################################################################
get.extension <- function(x) 
{ 
	trim( tail(spl(x,'\\.'),1) ) 
}	

#' @export 
#' @rdname FilenameFunctions
get.full.filename <- function(x) 
{ 
	trim( tail(spl(gsub('\\\\','/',x),'/'),1) ) 
}

#' @export 
#' @rdname FilenameFunctions
get.filename <- function(x) 
{ 
	temp = spl(get.full.filename(x),'\\.')
	join(temp[-len(temp)])
}


###############################################################################
#' Helper function to read historical stock prices saved by Seasonality tool
#'
#' @param Symbols vector of symbols
#' @param env enviroment to store prices, \strong{defaults to .GlobalEnv}
#' @param auto.assign flag to auto assign symbols, \strong{defaults to TRUE}
#' @param stock.folder stock folder, \strong{defaults to 'c:/temp/Seasonality/stocks'}
#' @param stock.date.format stock date format, \strong{defaults to '\%Y-\%m-\%d'}
#' @param ... other parameters for getSymbols function
#'
#' @return nothing is auto.assign = TRUE, prices are stored in the env enviroment
#' if auto.assign = FALSE, returns first symbol
#' 
#' @references 
#' \url{http://stackoverflow.com/questions/8970823/how-to-load-csv-data-file-into-r-for-use-with-quantmod}
#'
#' @examples
#' \dontrun{ 
#' data <- new.env()
#' getSymbols.sit(spl('SPY,IBM'), env = data, auto.assign = T)
#' }
#' @export 
######################################################################x#########
getSymbols.sit <- function
(
	Symbols, 
	env = .GlobalEnv, 
	auto.assign = TRUE, 
	stock.folder = 'c:/temp/Seasonality/stocks',
	stock.date.format = '%Y-%m-%d',
	...
) 
{
	require(quantmod)	
	
	# http://stackoverflow.com/questions/8970823/how-to-load-csv-data-file-into-r-for-use-with-quantmod
	for(i in 1:len(Symbols)) {
		s = Symbols[i]
		
		temp = list()
		temp[[ s ]] = list(src='csv', format=stock.date.format, dir=stock.folder)
		setSymbolLookup(temp)
		
		temp = quantmod::getSymbols(s, env = env, auto.assign = auto.assign)		
		if (!auto.assign) {
			cat(s, format(range(index(temp)), '%d-%b-%Y'), '\n', sep='\t')	
			return(temp)
		}
		if(!is.null(env[[ s ]]))
			cat(i, 'out of', len(Symbols), 'Reading', s, format(range(index(env[[ s ]])), '%d-%b-%Y'), '\n', sep='\t')	
		else
			cat(i, 'out of', len(Symbols), 'Missing', s, '\n', sep='\t')	
	}
}


###############################################################################
#' Normilize all timeseries to start at one
#'
#' @param x \code{\link{xts}} time series 
#'
#' @return scaled \code{\link{xts}} time series, so that each timeseries starts at one
#'
#' @examples
#' \dontrun{ 
#' plota.matplot(scale.one(data$prices))
#' }
#' @export 
############################################################################### 
# scale.one <- function(x) x / repRow(as.numeric(x[1,]), nrow(x))	
scale.one <- function(x) {
	index = 1:nrow(x)
	x / repRow(apply(x, 2, function(v) v[index[!is.na(v)][1]]), nrow(x))
}


###############################################################################
#' Compute correlations
#'
#' @param data matrix with data
#' @param method method used to compute correlations, please see \code{\link{cor}} for more details
#'
#' @return correlation matrix
#'
#' @export 
############################################################################### 
compute.cor <- function
(
	data, 		# matrix with data
	method = c("pearson", "kendall", "spearman")
)
{
	cor(data, use='complete.obs',method=method[1])
}


#all possible combinations of list elements
# expand.grid(a=1:10,b=2:3,KEEP.OUT.ATTRS=F)



###############################################################################
# Log (feedback) functions
###############################################################################
log.fn <- function(p.start=0, p.end=1) {
	p.start = p.start
  	p.end = p.end
	function(..., percent=NULL) { 
		cat(..., iif(is.null(percent),'',paste(', percent = ', round(100 * (p.start + percent * (p.end - p.start)), 1), '%', sep='')), '\n') 
	}
}

log.fn.msg <- function(msg, log = log.fn()) {
	log = log
    msg = msg
    function(..., percent=NULL) { log(paste(msg, ...), percent=percent) }
}  

    