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
  s,      # input string
  delim = ',' # delimiter
)
{ 
  unlist(strsplit(s,delim))
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
  v,      # vector of strings
  delim = ''  # delimiter
)
{ 
  paste(v,collapse=delim) 
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
  s # string
)
{
  s = sub(pattern = '^\\s+', replacement = '', x = s)
  sub(pattern = '\\s+$', replacement = '', x = s)
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
  x # vector
)
{
  length(x)
}

###############################################################################
#' Shortcut for list creation function
#'
#' This function is a shortcut for list creation function
#'
#' @param ... members of list
#'
#' @return list
#'
#' @examples
#' \dontrun{ 
#' a = 1
#' lst(a,b=2)
#' }
#' @export 
###############################################################################
lst <- function(
	... 
) 
{
	values = list( ... )
	if(len(values) == 0) return(values)

	values.names = names(values)
	names = as.character(substitute(c(...))[-1])		
		
	if( is.null(values.names) ) 
		names(values) = names
	else		
		names(values) = iif(nchar(values.names) > 0, values.names, names)
	values
}	

#' @export
vars2list <- function(...) {
	warning('vars2list is depreciated as of Feb 29, 2016 please use lst function instead')
	lst(...)
}

#' @export
variable.number.arguments <- function(...) {
	out = lst(...)
	if( is.list(out[[1]]) && is.list(out[[1]][[1]]) ) 
		out[[1]]
	else	
		out
}	



###############################################################################
#' Shortcut for new.env function
#'
#' This function is a convenience shortcut for new.env function
#' adding ability to work with environment the same way you work with list
#'
#' @param ... members of new environment
#'
#' @return new environment
#'
#' @examples
#' \dontrun{ 
#' whitespace = ' '
#' env(a=10,b=3,whitespace)
#' env(a=10,b=3,10)
#' }
#' @export
#' @rdname EnvironmentFunctions
###############################################################################
env <- function
(
	...,
	hash = TRUE, 
	parent = parent.frame(), 
	size = 29L
) 
{
	temp = new.env(hash = hash, parent = parent, size = size)
	values = lst(...)
	if(len(values) == 0) return(temp)
	
	# copy environment
	if(len(values) == 1 && is.environment(values[[1]]))
		list2vars(values[[1]], temp)
	else	
		list2vars(values, temp)
	temp
}

###############################################################################
#' Shortcut for rm function to delete items from given environment
#'
#' @export
#' @rdname EnvironmentFunctions
###############################################################################
env.del = function(names, env) {
	warning('env.del is depreciated as of Apr 25, 2016 please use env.rm function instead')
	env.rm(names, env)
}

#' @export
env.rm = function(names, env) {
	missing = setdiff(names, ls(env))
	if( len(missing) > 0)
		warning('Following names are missing in environment:', missing, '\n, names available in environment:', ls(env))
	rm(list=intersect(names, ls(env)), envir=env)
}


###############################################################################
# Variables to List and back
#
# can be useful for debugging:
#
# gall <<- lst(lookbacks, n.lag, hist.returns, index, hist.all, n.lookback)					
# list2vars(gall)
#
# options(warn=2)
# options(warn=1)
#
# test.env = environment()
# save(test.env, file='test.env.Rdata')
#
# load('test.env.Rdata')
# list2vars(test.env)
# list2vars(test.env, environment()) 
# similar to checkpoint package at CRAN
#
###############################################################################
# assign(n, data[[n]], env)
#' @export 
list2vars <- function(data, env = parent.frame()) {
	for(n in ls(data, all.names=T))
		env[[n]] = data[[n]]
}

# usage
# debug.save()
# stop()
# debug.load()
#' @export 
debug.save = function() {		
	gall <<- parent.frame()
}

#' @export 
debug.load = function() {
	list2vars(gall,  parent.frame())
}


###############################################################################
# Check default argumnets
#' @export 
###############################################################################
check.args = function(default.args, args=NULL) {
	if(is.null(args)) return(default.args)
	
	for(n in setdiff(ls(default.args, all.names=T), ls(args, all.names=T)))
		args[[n]] = default.args[[n]]
			
	args
}


###############################################################################
#' Shortcut for all.equal function
#'
#' This function is a convenience shortcut for all.equal function
#' adding ability to work with multiple variables
#'
#' @param ... variables to compare
#'
#' @return matrix with results of comparison
#'
#' all.equal(r.cor, c.cor)
#' all(abs(r.cor - c.cor) < 1e-10, na.rm=T)
#'
#' @export
###############################################################################
test.equality = function(..., eps = 1e-10, na.rm=T, type=c('first', 'all')) {
	values = list(...)
	n = len(values) 
	if(n == 0) return
	
	values.names = names(values)
	names = as.character(substitute(c(...))[-1])		
	names = iif(nchar(values.names) > 0, values.names, names)

	out = c()
	if(type[1] == 'all')
		for(i in 1:(n-1))
			for(j in (i+1):n)
				out = rbind(out, c(names[i], names[j], all(abs(values[[i]] - values[[j]]) < 1e-10, na.rm=na.rm)))
	else
		for(i in 2:n)
			out = rbind(out, c(names[1], names[i], all(abs(values[[1]] - values[[i]]) < 1e-10, na.rm=na.rm)))

	colnames(out) = spl('item1, item2,equal')
	out
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
  cond,   # condition
  truepart, # true part
  falsepart # false part
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
	  
      if(requireNamespace('xts', quietly = T) && xts::is.xts(truepart))
			falsepart[cond] = coredata(truepart)[cond]
      else
        falsepart[cond] = truepart[cond]
    }
      
    #falsepart[!is.na(cond)] = temp

    falsepart
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
  x,  # check x for NA, NaN, Inf
  y # if found replace with y
) 
{   
  return(iif(is.na(x) | is.nan(x) | is.infinite(x), y, x))
}

#' @export 
fast.na.omit <- function
(
  x
) 
{
  x[!is.na(x)]
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
  x,  # check x for NULL
  y # if found replace with y
) {   
  return(iif(is.null(x), y, x))
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
  x,      # matrix with data
  side = 2  # margin along which to count
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
  x,      # vector with data
  window.len  # window length
)
{ 
  n    = length(x) 
  xcount = cumsum( !is.na(x) )
  ycount = xcount[-c(1 : (k-1))] - c(0, xcount[-c((n-k+1) : n)])
  return( c( xcount[1:(k-1)], ycount))
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
  packages,               # names of the packages separated by comma
  repos = "http://cran.r-project.org",# default repository
  dependencies = c("Depends", "Imports"), # install dependencies
  ...                 # other parameters to install.packages
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
  identifier  # integer value
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
  identifier  # integer value
)
{
  if( exists(paste('saved.time', identifier, sep=''), envir = .GlobalEnv) ) {
      prevTime = get(paste('saved.time', identifier, sep=''), envir = .GlobalEnv)
      diffTimeSecs = proc.time()[3] - prevTime
      print(paste('Elapsed time is', round(diffTimeSecs, 2), 'seconds\n'))
    } else {
      print('Toc error\n')
    }
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
  m,      # matrix or vector
  nlag = 1  # number of lags
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
#' Last elements of matrix or vector
#'
#' @param m vector / matrix
#' @param n number of elements from the end, \strong{defaults to 1}
#'
#' @return last n elements
#'
#' @examples
#' \dontrun{ 
#' mlast(1:10)
#' }
#' @export 
###############################################################################
mlast = function(m, nlast=1) {
	n = iif(is.null(dim(m)), len(m), nrow(m))
    if(nlast >= n)
		m
    else
		iif(is.null(dim(m)), m[(n-nlast+1):n], m[(n-nlast+1):n,,drop=F])
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
  v,  # vector
  n,  # number of copies along rows
  m # number of copies along columns
)
{
  if( is.null(dim(v)) ) v = matrix(v,1)
  kronecker( matrix(1, n, m), v )
}

###############################################################################
#' Convience shortcut for as.vector function
#'
#' @param x object
#'
#' @return new vector
#' 
#' @export 
###############################################################################
vec <- function
(
	x
)
{
	if( !is.null(dim(x)) && len(dim(x)) != 1) 
		dim(x) = len(x)
	x
}

###############################################################################
#' Convience shortcut for matrix function
#'
#' @param x object
#' @param col flag to inidcate 1 column if x is a vector; otherwise 1 row
#'
#' @return new matrix
#' 
#' @export 
###############################################################################
mat <- function
(
	x,
	col = T	
)
{
	if( is.null(dim(x)) || len(dim(x)) != 2) {
		n = names(x)
		if( col ) {
			dim(x) = c(len(x), 1)
			if( !is.null(n) ) rownames(x) = n
		} else {
			dim(x) = c(1, len(x))
			if( !is.null(n) ) colnames(x) = n
		}
	}	
	x
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
#' rep.row(1:3, 5)
#' }
#' @export 
###############################################################################
rep.row <- function
(
  m, # vector (row)
  nr # number of copies along rows
)
{
  if(nr == 1) matrix(m, 1) 
  else matrix(m, nr=nr, nc=len(m), byrow=T)
}

###############################################################################
#do.call(cbind, lapply(1:nc,function(i) m))
#' Repeat Columns
#'
#' @param m vector (column)
#' @param nc number of copies along columns
#'
#' @return new matrix
#' 
#' @examples
#' \dontrun{ 
#' matrix(1:5, nr=5, nc=3, byrow=F)
#' rep.col(1:5, 3)
#' }
#' @export 
###############################################################################
rep.col <- function
(
  m,  # vector (column)
  nc  # number of copies along columns
)
{
  if(nc == 1) m
  else {
    if(is.xts(m))
      make.xts(matrix(coredata(m), nr=len(m), nc=nc, byrow=F), index(m))
    else
      matrix(m, nr=len(m), nc=nc, byrow=F)
  }  
}


# move column name to first row
#' @export 
col.name2row = function(x, move.name=T) {
  if(move.name && !is.null(colnames(x))) {
    x = rbind(colnames(x), x)
    colnames(x) = NULL
    x
  } else {
    colnames(x) = x[1,]
    x[-1,]
  }
}


# move row name to first column
#' @export 
row.name2col = function(x, move.name=T) {
  if(move.name && !is.null(rownames(x))) {
    x = cbind(rownames(x), x)
    rownames(x) = NULL
    x
  } else {
    rownames(x) = x[,1]
    x[,-1]
  }
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
  data,   # matrix
  i,    # index of observations
  details = F # flag to return additional details
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
#' \url{http://r.789695.n4.nabble.com/slope-calculation-td858652.html }
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
# Formatting helper functions
###############################################################################
#' @export 
to.nice = function(out,nround=2,sprefix='',eprefix='') {
  if( !is.null(dim(out)) ) {    
    temp = matrix('', nrow(out),ncol(out))
    rownames(temp) = iif(is.xts(out), paste(index(out)),rownames(out))
      colnames(temp) = colnames(out)
      temp.n = apply(out,2,as.double)
      index = is.na(temp.n)
    
    temp[] = paste(sprefix,format(round( temp.n ,nround),big.mark=",", scientific=FALSE),eprefix ,sep='')
      temp[index] = coredata(out)[index]
    temp
  } else {
    temp.n = as.double(out)
    index = is.na(temp.n)
  
    temp = paste(sprefix,format(round( temp.n ,nround),big.mark=",", scientific=FALSE),eprefix ,sep='')
      temp[index] = out[index]
    temp    
  }
}

#' @export 
to.percent = function(x, nround=2) to.nice(100*x,nround,'','%')

#' @export 
to.cash = function(x, nround=2) to.nice(x,nround,'$')



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
#'  as.numeric(test)
#'  as.numeric(as.POSIXct(as.Date(test)))
#' as.numeric(as.POSIXct(as.Date(test))) - as.numeric(test)
#' test == as.POSIXct(as.Date(test))
#'
#' # Set Time Zone
#' Sys.setenv(TZ = 'GMT')
#' Sys.getenv('TZ')
#' test = as.POSIXct('2012-10-31', format='%Y-%m-%d')
#'  as.numeric(test)
#'  as.numeric(as.POSIXct(as.Date(test)))
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
  x,      # data
  order.by  # date
)
{
  #Sys.setenv(TZ = 'GMT')
  tzone = Sys.getenv('TZ')
  
    orderBy = class(order.by)
    index = as.numeric(as.POSIXct(order.by, tz = tzone))
    
    # need to handle case for one row; i.e. len(orderBy) == 1
    if( is.null(dim(x)) ) {
      if( len(order.by) == 1 )
        x = t(as.matrix(x))
      else
        dim(x) = c(len(x), 1)
    }
    x = as.matrix(x)

    x = structure(.Data = x, 
      index = structure(index, tzone = tzone, tclass = orderBy), 
      class = c('xts', 'zoo'), .indexCLASS = orderBy, tclass=orderBy, .indexTZ = tzone, tzone=tzone)
      
    if (!is.null(attributes(x)$dimnames[[1]]))
        dimnames(x) <- dimnames(x)              
    x
}


# convience function: take list of xts objects with 1 column and combine them into
# one xts object with column names beign names in the input list
#' @export 
as.xts.list <- function(data) { for(n in names(data)) colnames(data[[n]])=n; do.call(cbind, data)}


###############################################################################
#' Convert \code{\link{xts}} object to \code{\link{ts}} object
#'
#' @param x \code{\link{ts}} object
#'
#' @return \code{\link{xts}} object
#' 
#' use as.xts(ts) to convert back
#' 
#' @export 
###############################################################################
xts2ts = function(x) {  
  annual.factor = compute.annual.factor(x)

  map = c(date.day, date.week, date.month, date.quarter)
    names(map) = trim(spl('252, 52, 12, 4'))
  date.fn = map[[paste(annual.factor)]]

  first.date = index(first(x))
  last.date = index(last(x))
  
  start = date.year(first.date)
  end = date.year(last.date)
  if( !is.null(date.fn) ) {
    start = c(start, date.fn(first.date)) 
    end = c(end, date.fn(last.date)) 
  } 
    
  ts(coredata(x[,1]), start = start, end = end, deltat = 1 / annual.factor)   
}


###############################################################################
#' Reverse order of \code{\link{xts}} object
#'
#' @param x \code{\link{xts}} object
#'
#' @return \code{\link{xts}} object
#' 
#' @export 
###############################################################################
flip.xts <- function(x)
{
  dates = index(x)
  dates.index = nrow(x):1
  out = make.xts(coredata(x)[dates.index,], dates[dates.index])
    indexClass(out) = indexClass(x)
  return( out )
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
  x,      # XTS object
  filename, # file name
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
  x,  # file name or data matrix
  date.fn = paste,
  index.class = 'Date',
  decreasing = FALSE,
  sep = ',',
  date.column = 1,
  skip = 0L,
  ...
)
{
load.packages('data.table')
if (is.matrix(x) || (is.data.frame(x) && !is.data.table(x)) ) {
  data = x
  dates = as.matrix(data[,date.column,drop=F])
  
  index = which(sapply(data,class) != 'character')
    index = index[ index > date.column ]  
  data  = data[,index,drop=F]
  #data  = data[,-date.column,drop=F]
} else {
  filename = x  
if(!is.data.table(x)) {
  # set autostart
  out = fread(filename, stringsAsFactors=F, sep=sep, autostart=2, skip=skip)
    setnames(out,gsub(' ', '_', trim(colnames(out)))) 
} else out = x  

  
#   first.column.expr = parse(text = colnames(out)[date.column])
    #rest.columns.expr = parse(text = paste('list(', paste(colnames(out)[-(1:date.column)],collapse=','),')'))
  # remove non-numeric columns
    rest.columns.expr = parse(text = paste('list(', paste(names(which(sapply(out,class)[-(1:date.column)] != 'character')),collapse=','),')'))    
# dates = out[,eval(first.column.expr)]
# data = out[, eval(rest.columns.expr)]

  dates = as.matrix(out[,date.column,with=FALSE])   
  index = which(sapply(out,class) != 'character')
    index = index[ index > date.column ]  
  data =  as.matrix(out[,index,with=FALSE]+0) 
}   
  dates = as.POSIXct(match.fun(date.fn)(dates), tz = Sys.getenv('TZ'), ...)
    dates.index = iif(is.null(decreasing), 1:nrow(data), order(dates, decreasing = decreasing) )
  out = make.xts(data[dates.index,,drop=F], dates[dates.index])
    indexClass(out) = index.class
  return( out )
}  


# A few other variations to read data
read.xts.old <- function
(
  filename, # file name
  date.fn = paste,
  index.class = 'Date',
  decreasing = FALSE,
  ...
)
{
  out = read.csv(filename, stringsAsFactors=F)
  #return( make.xts(out[,-1,drop=F], as.Date(out[,1], ...)) )
  
  dates = as.POSIXct(match.fun(date.fn)(out[,1]), tz = Sys.getenv('TZ'), ...)
    dates.index = order(dates, decreasing = decreasing)
  out = make.xts(out[dates.index,-1,drop=F], dates[dates.index])
    indexClass(out) = index.class
  return( out )

# Example code from getSymbols.yahoo (quantmod): as.POSIXct is used to avoid Dates conversion problems
# fr = xts(1, as.POSIXct('2012-10-31', tz = Sys.getenv("TZ"), format='%Y-%m-%d'),  src = "yahoo", updated = Sys.time())
# indexClass(fr) = "Date" 
}

read.xts.yahoo.old <- function
(
  filename, # file name
  date.fn = paste,
  index.class = 'Date',
  decreasing = FALSE,
  ...
)
{
  temp = scan(filename, what=list('',double(0), double(0),double(0),double(0),double(0),double(0)), skip=1, sep=',', quiet =T)  
  
  dates = as.POSIXct(match.fun(date.fn)(temp[[1]]), tz = Sys.getenv('TZ'), ...) 
    dates.index = order(dates, decreasing = decreasing)
    
  out = matrix(double(1),len(dates), 6)
      colnames(out) = spl('Open,High,Low,Close,Volume,Adjusted')
  out[,1] = temp[[2]] 
    out[,2] = temp[[3]]
    out[,3] = temp[[4]]
    out[,4] = temp[[5]] 
    out[,5] = temp[[6]]
    out[,6] = temp[[7]]
    
    out = make.xts(out[dates.index,],  dates[dates.index])
    indexClass(out) = index.class
  return( out )
}

read.xts.test <- function() {
  load.packages('rbenchmark')

  filename = 'c:/stocks/SPY.csv'

  test1 <- function() {
    out = read.csv(filename, stringsAsFactors=F)
  }
  test2 <- function() {
    out1 = fread(filename, stringsAsFactors=F)
  }
  test3 <- function() {
    out2 = scan(filename, what=list('',double(0), double(0),double(0),double(0),double(0),double(0)), skip=1, sep=',', quiet =T)
  }
      
  library(rbenchmark)
   benchmark(
       test1(), 
       test2(),
       test3(),
       columns = c("test", "replications", "elapsed", "relative"),
       order = "relative",
       replications = 20
   )


  test1 <- function() {
    out = read.xts(filename, format = '%Y-%m-%d')
  }
  test2 <- function() {
    out1 = read.xts.old(filename, format = '%Y-%m-%d')
  }
  test3 <- function() {
    out2 = read.xts.yahoo.old(filename, format = '%Y-%m-%d')
  }
    
  library(rbenchmark)
   benchmark(
       test1(), 
       test2(),
       test3(),
       columns = c("test", "replications", "elapsed", "relative"),
       order = "relative",
       replications = 20
   )
   
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
  x     # XTS object
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
  x     # XTS object
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
#' 
#' data = textConnection('
#' date,Close
#' 2013-03-18,    154.97
#' 2013-03-19,    154.61
#' 2013-03-20,    155.69
#' 2013-03-21,    154.36
#' 2013-03-22,    155.60
#' 2013-03-25,    154.95')
#' 
#' x = read.xts(data)
#' dates2index(x, '2013-03-19')
#' 
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
#' Determine location of given names in column names of data
#'
#' @param names names to find
#' @param data xts/matrix/data.frame that column names to search
#' @param return.index optional parameter that indicates whether to return names or indexes of names matched
#'
#' @return name/index if names is just one element; otherwise list of found matches for each name
#'
#' @export 
###############################################################################
find.names <- function(names, data, return.index = T) 
{ 
  names = spl(names)
  all.names = colnames(data)
  out = list()
  for(n in names) {
    loc = grep(n, all.names, ignore.case = TRUE)
  
    # special logic for Close and Adjusted  
    if(len(loc) == 0 && ncol(data) == 1 && 
      (grepl(n,'close',ignore.case = TRUE) || grepl(n,'adjusted',ignore.case = TRUE))
      ) loc = 1
    
    if(len(loc) > 0) out[[n]] = iif(return.index, loc, all.names[loc])
  }
    
  iif(len(names) == 1 && len(out) == 1, out[[1]][1], out)
}


###############################################################################
#' Create stock like \code{\link{xts}} object from one column time series
#'
#' @param out \code{\link{xts}} time series 
#' @param column column index to use, \strong{defaults to 1} 
#'
#' @return stock like \code{\link{xts}} object
#'
#' @export 
############################################################################### 
make.stock.xts <- function(out, column=1) {
  # try to be smart about mapping columns in out
  names = spl('Open,High,Low,Close,Volume,Adjusted')
  names.index = find.names(names, out)
  
  temp= list()
  for(n in names)
    if( !is.null(names.index[[n]]) )
      temp[[n]] = out[,names.index[[n]][1]]
  
  if(is.null(temp$Close) && is.null(temp$Adjusted))
    temp$Close = temp$Adjusted = out[,column]
  if(is.null(temp$Adjusted)) temp$Adjusted = temp$Close
  if(is.null(temp$Close)) temp$Close = temp$Adjusted
  
  if(is.null(temp$Open)) temp$Open = temp$Close
  if(is.null(temp$High)) temp$High = temp$Close
  if(is.null(temp$Low)) temp$Low = temp$Close
  if(is.null(temp$Volume)) temp$Volume = 0
  
  out = cbind(temp$Open,temp$High,temp$Low,temp$Close,temp$Volume,temp$Adjusted)
    colnames(out) = names
  out
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
# scale.one <- function(x) x / rep.row(as.numeric(x[1,]), nrow(x))  
scale.one <- function
(
  x, 
  overlay = F, 
  main.index = which(!is.na(x[1,]))[1] 
) 
{
  index = 1:nrow(x)
  if( overlay )
    x / rep.row(apply(x, 2, 
        function(v) {
          i = index[!is.na(v)][1]
          v[i] / as.double(x[i,main.index])
        }
    ), nrow(x))
  else
    x / rep.row(apply(x, 2, function(v) v[index[!is.na(v)][1]]), nrow(x))
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


# Aside : all possible combinations of list elements
# expand.grid(a=1:10,b=2:3,KEEP.OUT.ATTRS=F)

# get first element in every offset: sapply(offsets, '[', 1)


#' @export 
parse.expr = function(expr) {
	if (is.character(expr))
		expr = spl(expr)
	expr = gsub('<newline>','\n', expr)
	expr = trim(spl(gsub('\n', ',', join(expr, ','))))
	
	expr = expr[nchar(expr) > 0 & substring(expr, 1, 1) != "#"]
	sapply(expr, function(x) spl(x,'#')[1])
}

#' @export 
map.symbols = function(Symbols) {
  	Symbols = toupper(Symbols)
    
	# split
	map = list()
	for(s in Symbols) {  
		temp = spl(s, "=")
		if ( len(temp) > 1 ) { 
			name = temp[1]
			values = trim(spl(temp[2], '\\+'))
      
			# handle = [a] + b
			value1 = values[1]
				value1.name = grepl('\\[', value1)      
			value1 = gsub('\\]','',gsub('\\[','',value1))
			value1 = trim(spl(value1,';'))
      
			values = values[-1]
      
			for(n in trim(spl(name,';')))
				map[[ n  ]] = c(value1[1], values)
        
			# handle = [a] + b
			if( len(value1) > 1 || value1.name)
				for(n in value1)
					map[[ n  ]] = c(n, values)      
		} else {
			temp = spl(temp, '\\+')
			name = temp[1]
			values = trim(temp[-1])
			
			for(n in trim(spl(name,';')))
				map[[ n  ]] = c(n, values)
		}
    
		#name = iif(len(spl(s, '=')) > 1, spl(s, '=')[1], spl(s, '\\+')[1])
		#values = spl(iif(len(spl(s, '=')) > 1, spl(s, '=')[2], s), '\\+')
		#map[[trim(name)]] = trim(values)   
	}
	map
}


###############################################################################
#' Helper function to extend functionality of getSymbols
#'
#' Syntax to specify tickers:
#' * Basic : XLY
#' * Rename: BOND=TLT
#' * Extend: XLB+RYBIX
#' * Mix above: XLB=XLB+RYBIX+FSDPX+FSCHX+PRNEX+DREVX
#' Symbols = spl('XLY, BOND=TLT,XLY+RYBIX,XLB=XLB+RYBIX+FSDPX+FSCHX+PRNEX+DREVX')

#' tickers=spl('XLB+RYBIX+FSDPX+FSCHX+PRNEX+DREVX,
#' XLE+RYEIX+VGENX+FSENX+PRNEX+DREVX,
#' XLF+FIDSX+SCDGX+DREVX,
#' XLI+FSCGX+VFINX+FEQIX+DREVX,
#' XLK+RYTIX+KTCBX+FSPTX+FTCHX+FTRNX+DREVX,
#' XLP+FDFAX+FSPHX+OARDX+DREVX,
#' XLU+FSUTX+DREVX,
#' XLV+VGHCX+VFINX+DREVX,
#' XLY+FSRPX+DREVX,
#' BOND+IEI+VFIUX+VFITX+FSTGX+FGOVX+STVSX+FGMNX+FKUSX')
#' 
#' data <- new.env()
#'   getSymbols.extra(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
#' bt.start.dates(data)
#' 
#' @export 
################################################################################
getSymbols.extra <- function 
(
  Symbols = NULL, 
  env = parent.frame(), 
  getSymbols.fn = getSymbols,
  raw.data = new.env(),   # extra pre-loaded raw data
  set.symbolnames = F,
  auto.assign = T,
  try.extend = T,  
  ...
) 
{
	Symbols = parse.expr(Symbols)
	if(len(Symbols) < 1) return(Symbols)
  
	map = map.symbols(Symbols)
	
	Symbols = unique(unlist(map))
  
	# find overlap with raw.data
	Symbols = setdiff(Symbols, ls(raw.data, all.names=T))

	# download
	data = new.env()
	if(len(Symbols) > 0) match.fun(getSymbols.fn)(Symbols, env=data, auto.assign = T, ...)
	for(n in ls(raw.data, all.names=T)) data[[n]] = raw.data[[n]]
  
	# reconstruct, please note getSymbols replaces ^ symbols
	if (set.symbolnames) env$symbolnames = names(map)
	
  for(s in names(map)) {
    env[[ s ]] = data[[ gsub('\\^', '', map[[ s ]][1]) ]]
   if(try.extend)    
    if( len(map[[ s ]]) > 1)
      for(i in 2:len(map[[ s ]]))
        if(is.null(data[[ gsub('\\^', '', map[[ s ]][i]) ]]))
          cat('Not Downloaded, main =', s, 'missing' , gsub('\\^', '', map[[ s ]][i]), '\n', sep='\t')    
        else
          env[[ s ]] = extend.data(env[[ s ]], data[[ gsub('\\^', '', map[[ s ]][i]) ]], scale=T)
          
          
    if (!auto.assign)
          return(env[[ s ]])      
  } 
}



getSymbols.extra.test <- function() 
{
  # Syntax to specify tickers:
  # * Basic : RWX
  # * Rename: REIT=RWX
  # * Extend: RWX+VNQ
  # * Mix above: REIT.LONG=RWX+VNQ+VGSIX  
  tickers = spl('REIT=RWX, RWX+VNQ, REIT.LONG=RWX+VNQ+VGSIX')
  data <- new.env()
    getSymbols.extra(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
  bt.start.dates(data)
  
  data$symbolnames = spl('REIT.LONG,RWX,REIT')
    for(i in data$symbolnames) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
  bt.prep(data, align='keep.all', fill.gaps = T)  
   
  plota.matplot(data$prices)
  
  # Use extrenal data
  raw.data <- new.env()
    raw.data$GOLD = bundes.bank.data.gold()
  
  tickers = spl('GLD, GLD.LONG=GLD+GOLD')
  data <- new.env()
    getSymbols.extra(tickers, src = 'yahoo', from = '1980-01-01', env = data, raw.data = raw.data, auto.assign = T)
  bt.start.dates(data)
  data$symbolnames = spl('GLD.LONG,GLD')
      for(i in data$symbolnames) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
  bt.prep(data, align='keep.all', fill.gaps = T)  
   
   plota.matplot(data$prices) 
} 


###############################################################################
#' Helper function to add Intraday prices
#' @export 
################################################################################
getSymbols.intraday <- function 
(
  Symbols = NULL, 
  env = parent.frame(), 
  getSymbols.fn = getSymbols,
  auto.assign = T,  
  ...
) 
{
  if(len(Symbols) > 0) {
    match.fun(getSymbols.fn)(Symbols, env = env, auto.assign = auto.assign, ...)

    # same logic as in getSymbols.extra
	Symbols = parse.expr(Symbols)
	if(len(Symbols) < 1) return(Symbols) 
	map = map.symbols(Symbols)
		map = sapply(map, first)
	
	data.today = getQuote.yahoo.today(unique(map))
		data.today[data.today == 'N/A'] = NA
		lookup = 1:nrow(data.today)
		names(lookup) = toupper(trim(data.today$Symbol))
	
	data.today = data.today[lookup[map],]
		data.today$Symbol = names(map)
		
    bt.append.today(env, data.today)
  }
}


# test for getSymbols.intraday function
getSymbols.intraday.test <- function() { 
	tickers = '
	LQD + VWESX
	DBC + CRB
	VTI +VTSMX # (or SPY)
	ICF + VGSIX # (or IYR)
	CASH = SHY
	'
	
	data.intraday  = env()
	
	getSymbols.extra(tickers, src = 'yahoo', from = '2012-01-01', env = data.intraday,
		raw.data = data.proxy.raw, set.symbolnames = T, auto.assign = T,
		getSymbols.fn = getSymbols.intraday)
	
	last(data.intraday$CASH,5)
	last(data.intraday$VTI,5)
	
	data = env()
	
	getSymbols.extra(tickers, src = 'yahoo', from = '2012-01-01', env = data,
		raw.data = data.proxy.raw, set.symbolnames = T, auto.assign = T)
	
	last(data$CASH,5)
	last(data$VTI,5)
}






###############################################################################
#' Work with expressions
#' as.expression(quote({x=2+y}))
#' @export 
################################################################################
convert2expr = function(expr) {
	if(class(substitute(expr)) == '{') {
		if(F) {
			return(as.expression(substitute(expr)))
		} else {
			temp = deparse(substitute(expr))
			return(parse(text = temp[-c(1,length(temp))]))
		}
	}
	
	if(is.character(expr)) return(parse(text = expr))
	
	expr
}

convert2expr.test = function() {
	convert2expr({x=2+y})
	convert2expr({x=2+y; a=b})
	convert2expr({
		x=2+y
		a=b
	})
	
	convert2expr(expression(x=2+y))
	convert2expr(expression(x=2+y,a=b))
	convert2expr('x=2+y')
	convert2expr('x=2+y; a=b')
	convert2expr('
		x=2+y
		a=b
	')
	
	a = convert2expr({x=2+y})
	expr.symbols(a)
}




remove.operators = function(tokens) { 
	tokens = unique(trim( tokens[-grep('[=\\+\\-\\*/><\\(\\)\\{\\}]',tokens)] )) 
	tokens[nchar(tokens) > 0 & tokens != 'expression' & tokens != 'convert2expr']
}

# http://adv-r.had.co.nz/Expressions.html
# bizzare!!!
# all.names(parse(text='x=2+y'))
#[1] "=" "x" "+" "y"
# all.names(expression(x=2+y))
#[1] "+" "y"
#
# names(as.pairlist(expression(a+b, c=d)))
# all.names(expression(a+b, c=d))
#' @export 
expr.symbols = function(expr) {
	# use substitute to avoid expr evaluation
	#cat(class(substitute(expr)), mode(substitute(expr)), is.expression(substitute(expr)), '\n')
	
	# need quote to avoid evaluation, all.names(quote({
	if(mode(substitute(expr)) == 'call')
		return(remove.operators(
			c(names(as.pairlist(substitute(expr))),
			all.names(substitute(expr)))
		))		
	
	# if name no need to substitute
	if(mode(substitute(expr)) == 'name')
		if(is.expression(expr)) {	
			return(remove.operators(
				c(names(as.pairlist(expr)),
				all.names(expr))
			))		
		} else {
			return(remove.operators(
				all.names(expr)
		))
		}
	
		
	if(is.character(expr)) 
		return(remove.operators(
			all.names(parse(text=expr))
		))

	expr
}

expr.symbols.test = function() {

	expr.symbols({
		x = 2 + y+z
		a=2+b
	})
	
	
	expr.symbols({x=y+2})
	
	expr.symbols('x=y+2')
	
	expr.symbols(expression(x=2+y))
	
	expr.symbols(expression(x=2+y, a=b))
	
	a = expression(x=2+y+zzasd)
	expr.symbols(a)
	
}


###############################################################################
# Log (feedback) functions
###############################################################################

###############################################################################
#' @export 
###############################################################################
log.fn <- function(p.start=0, p.end=1) {
  p.start = p.start
    p.end = p.end
  function(..., percent=NULL) { 
    cat(..., iif(is.null(percent),'',paste(', percent = ', round(100 * (p.start + percent * (p.end - p.start)), 1), '%', sep='')), '\n') 
  }
}

###############################################################################
#' @export 
###############################################################################
log.fn.msg <- function(msg, log = log.fn()) {
  log = log
    msg = msg
    function(..., percent=NULL) { log(paste(msg, ...), percent=percent) }
}  

###############################################################################
#' Working with characters
#'
#' http://datadebrief.blogspot.ca/2011/03/ascii-code-table-in-r.html
#'
#' char is 8 bits, so to generate 512 bits random string 
#' rawToChar(as.raw(runif(512/8, 1, 255)))
#'
#' @export 
#' @rdname StringFunctions
############################################################################### 
asc <- function(x) { strtoi(charToRaw(x),16L) }

#' @export 
#' @rdname StringFunctions
chr <- function(n) { rawToChar(as.raw(n)) }

#' @export 
#' @rdname StringFunctions
make.random.string <- function(nbits = 256) { chr( runif(nbits/8, 1, 255) ) }

#' http://ryouready.wordpress.com/2008/12/18/generate-random-string-name/
#' @export 
#' @rdname StringFunctions
random.string <- function(lenght = 12) { join(sample(c(0:9, letters, LETTERS),lenght, replace=TRUE)) }


###############################################################################
#' List function / variables in enviroment
#'
#' http://www.mail-archive.com/r-help@@stat.math.ethz.ch/msg22679.html
#'
#' @export 
#' @rdname ListEnvFunctions
############################################################################### 
ls.f <- function(env=sys.frame(-1))unlist(lapply(ls(env=env),function(x)if(is.function(get(x)))x))

#' @export 
#' @rdname ListEnvFunctions
ls.v <- function(env=sys.frame(-1))unlist(lapply(ls(env=env),function(x)if(!is.function(get(x)))x))




# http://r.789695.n4.nabble.com/Numeric-Characters-in-String-td3817873.html
#' @export 
parse.number <- function(x) {
  as.numeric(gsub('[^0-9\\+-\\.]', '', x) ) 
}

###############################################################################
#' Flexiable utility function to Map value(s) to vector
#'
#' @param value value(s) to use for mapping
#' @param labels names of vector columns
#' @param default default value, \strong{defaults to 0}
#'
#' @return vector
#'
#' Possible scenarios:
#' one number, applied to all assets
#' array, same number of entrys as assets
#' named list, each name corresponds asset
#'	plus have a fallback asset if nothing provided
#'
#' @examples
#' \dontrun{ 
#' map2vector(1, 5)
#' map2vector(c(1,2,3,4,5), 5)
#' map2vector(c(1,2,NA,4,5), 5, 100)
#' map2vector(list(a=1,b=4), 'a,b,c,d,e', 100)
#' map2vector(list(a=1,C=4), 'a,b,c,d,e', 100)
#' map2vector(labels= 'a,b,c,d,e', 100)
#' map2vector({
#'   a=10
#'   b=2
#'   },'a,b,c', 100)
#' }
#' @export 
###############################################################################
map2vector = function(expr, labels, default = 0) {
	if( is.xts(labels) ) labels = names(labels)
	if( is.character(labels) ) labels = spl(labels)
	n = iif( is.numeric(labels), labels, len(labels) )
	
	if( len(expr) == 0 ) return(rep(default, n))
	
	if(mode(substitute(expr)) == 'call') {
		value = rep(default, n)
			names(value) = labels
			
		# idea from within
		e = evalq(environment(), as.list(value))
		eval(substitute(expr), e)
			
		temp = unlist(as.list(e))
		value[names(temp)] = temp
		return(value)
	}
		
	if( is.list(expr) ) {
		out = rep(default, n)
		out[ match(toupper(names(expr)), toupper(labels)) ] = unlist(expr)
		out
	} else		
		ifna(iif( len(expr) == 1, rep(expr, n), expr), default)
}

###############################################################################
#' Reverse mapping
#'
#' @export 
###############################################################################
rev.map = function(map) {
	value = names(map)
		names(value) = map
	value
}


###############################################################################
#' Reverse mapping
#'
#' @rdname FileFunctions
#' @export 
###############################################################################
write.file = function(..., file) cat(..., file=file)


# [Import text file as single character string](http://stackoverflow.com/questions/9068397/import-text-file-as-single-character-string)
#' @rdname FileFunctions
#' @export 
read.file = function(file) readChar(file, file.info(file)$size)


###############################################################################
#' String Buffer class - fast storage for strigns
#' 
#' @examples
#' \dontrun{ 
#' sb = string.buffer()
#' add(sb, 'asbcd')
#' add(sb, '234543')
#' string(sb)
#' close(sb)
#' sb=NULL
#' }
#' @rdname string.buffer
#' @export
###############################################################################
string.buffer = function() structure(list(file = rawConnection(raw(0L), open='w')), class = 'StringBuffer')

#' @rdname string.buffer
#' @export
add = function(x,...,sep,end.sep) UseMethod('add',x)

#' @rdname string.buffer
#' @export
add.StringBuffer = function(x,...,sep=',',end.sep='\n') {
	cat(..., file = x$file, sep = sep)	
	if(nchar(end.sep) > 0) cat(end.sep, file = x$file)
	}

#' @rdname string.buffer
#' @export
string = function(x) UseMethod('string',x)

#' @rdname string.buffer
#' @export
string.StringBuffer = function(x) rawToChar(rawConnectionValue(x$file))

#' @rdname string.buffer
#' @export
close = function(x) UseMethod('close',x)

#' @rdname string.buffer
#' @export
close.StringBuffer = function(x) {close(x$file); x$file = NULL}


# test string.buffer functionality
string.buffer.test = function() {
	# base example
	file = rawConnection(raw(0L), open="w")

	write('asbcd', file)
	write('234543', file)

	res =rawToChar(rawConnectionValue(file))

	close(file)
	file = NULL;
	
	# string.buffer class usage
	sb = string.buffer()
	add(sb, 'asbcd')
	add(sb, '234543')
	string(sb)
	close(sb)
	sb=NULL
		
	#benchmark
   	test.base = function() {
   		s =''
		for(i in 1:10000)
			s = paste(s,'abcdef',sep='')
		nchar(s)
   	}
   	test.string.buffer = function() {
		sb = string.buffer()
		for(i in 1:10000)
			add(sb, 'abcdef', '')
		s = string(sb)	
		sb=NULL
		nchar(s)
   	}
	
  	library(rbenchmark)
	benchmark(
   		test.base(),
   		test.string.buffer(),
       columns = c("test", "replications", "elapsed", "relative"),
       order = "relative",
       replications = 1
	)
}
