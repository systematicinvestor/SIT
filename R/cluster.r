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
# Repository of Benchmark Strategies
# Copyright (C) 2014  Michael Kapler
#
# For more information please visit my blog at www.SystematicInvestor.wordpress.com
# or drop me a line at TheSystematicInvestor at gmail
###############################################################################




###############################################################################
#' Helper function to setup cluster
#'
#'	load.packages('parallel')
#'	cl = setup.cluster({source('post.fn.r')}, 'spyRets,sma.lens,data,run.backtest',envir=environment())
#'	out = clusterApplyLB(cl, vol.lens, function(j) { run.backtest(j) } )	
#'	stopCluster(cl)	
#'
#' @export 
###############################################################################
setup.cluster <- function(expr = NULL, varlist = NULL, envir = .GlobalEnv, cores = 1000) {
	#*****************************************************************
	# Setup Parallel
	#*****************************************************************			
	load.packages('parallel')	
	cores = min(cores, detectCores())
		
	# we don't want to execute defult settings	
		Sys.unsetenv("R_PROFILE_USER")	
	cl = makeCluster(cores)	
	
	#*****************************************************************
	# Setup Cluster
	#*****************************************************************							
	temp = clusterEvalQ(cl, {
		# set up each worker.
		library(quantmod)
		library(SIT)
		
		#con = gzcon(file('../sit', 'rb')) 
		#	source(con) 
		#close(con)
		
		NULL
	})	

	# clusterEvalQ <- function (cl = NULL, expr) clusterCall(cl, eval, substitute(expr), env = .GlobalEnv)
	if(!is.null(expr))
		temp = clusterCall(cl, eval, substitute(expr), env = .GlobalEnv)
			
	#*****************************************************************
	# Move Data to Cluster
	#*****************************************************************								 	
	if(!is.null(varlist))
		clusterExport(cl=cl, trim(spl(varlist)),envir=envir) 
		
	cl
}


###############################################################################
#' Parallel Helper Log functions
#'
#' @export 
###############################################################################
clusterApplyLB.log <- function (cl = NULL, log = log.fn(), x, fun, ...) {
    argfun <- function(i) c(list(x[[i]]), list(...))
    dynamicClusterApply.log(cl, log, fun, length(x), argfun)
}

dynamicClusterApply.log <- function (cl = NULL, log = log.fn(), fun, n, argfun) {
    cl <- parallel:::defaultCluster(cl)
    p <- length(cl)
    if (n > 0L && p) {
        submit <- function(node, job) parallel:::sendCall(cl[[node]], fun,
            argfun(job), tag = job)
        for (i in 1:min(n, p)) submit(i, i)
        val <- vector("list", n)
        for (i in 1:n) {
log(i, percent = i / n)
            d <- parallel:::recvOneResult(cl)
            j <- i + min(n, p)
            if (j <= n)
                submit(d$node, j)
            val[d$tag] <- list(d$value)
        }
        parallel:::checkForRemoteErrors(val)
    }
}		