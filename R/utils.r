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
# Copyright (C) 2011  Michael Kapler
###############################################################################


###############################################################################
# Convenience Utilities
###############################################################################
# Split string into tokens using delim
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
# Join vector of strings into one string using delim
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
# Remnove any leading and trailing spaces
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
# Get the length of vectors
############################################################################### 
len <- function
(
	x	# vector
)
{
	return(length(x)) 
}

###############################################################################
# Fast version of ifelse
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
		else 
			falsepart[cond] = truepart[cond]
			
		return(falsepart);
	}
} 

###############################################################################
# Load Packages that are available and install ones that are not available.
############################################################################### 
load.packages <- function
(
	packages, 							# names of the packages separated by comma
	repos = "http://cran.r-project.org",# default repository
	dependencies = TRUE,				# install dependencies
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
# Timing Utilities
###############################################################################
# Begin timing
###############################################################################
tic <- function
(
	identifier	# integer value
)
{
	assign(paste('saved.time', identifier, sep=''), proc.time()[3], envir = .GlobalEnv)
}

###############################################################################
# End timing
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
}

###############################################################################
# Test for timing functions
###############################################################################
test.tic.toc <- function()
{
	tic(10)
	for( i in 1 : 100 ) {
		temp = runif(100)
	}
	toc(10)
}

