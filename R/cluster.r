###############################################################################
# This software is provided 'as-is', without any express or implied
# warranty. In no event will the authors be held liable for any damages
# arising from the use of this software.
# 
# Permission is granted to anyone to use this software for any purpose,
# including commercial applications, and to alter it and redistribute it
# freely, subject to the following restrictions:
# 
# 1. The origin of this software must not be misrepresented; you must not
#    claim that you wrote the original software. If you use this software
#    in a product, an acknowledgment in the product documentation would be
#    appreciated but is not required.
# 2. Altered source versions must be plainly marked as such, and must not be
#    misrepresented as being the original software.
# 3. This notice may not be removed or altered from any source distribution.
###############################################################################
# Collection of utilities to setup calculations on a cluster.
#
# For more information please email at TheSystematicInvestor at gmail
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