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
# Optimization Functions
# Copyright (C) 2011  Michael Kapler
#
# For more information please visit my blog at www.SystematicInvestor.wordpress.com
# or drop me a line at TheSystematicInvestor at gmail
###############################################################################


###############################################################################
# Solve LP problem, handling negative x
###############################################################################
# Tradional LP problem : http://lpsolve.sourceforge.net/5.5/LPBasics.htm
# maximize     C x
# subject to   A x <= B
#              x >= 0 - NOTE LP assumes all x >= 0
###############################################################################
# http://r.789695.n4.nabble.com/help-using-R-s-linprog-for-LP-td906987.html
# NOTE: Linear Programming (LP) assumes x >= 0
#
# The constraints x >= 0 are used in most linear programming realizations.
# Some bounds from below are needed. The trick to circumvent the restriction
# is as follows:
#
# Assume you know  x >= d  where some or all of the d_i can be negative.
# Replace x with  y = x - d >= 0  and minimize c'y with  Ay <= b - A d !
# Your solution is then  x = y + d , that is
#
# solveLP(cvec, bvec - Amat %*% dvec, Amat)$solution + dvec
#
# For portfolio weights, it is safe to assume that x.i >= -100
#' @export 
###############################################################################
solve.LP.bounds <- function
(
	direction, 		# lp parameters
	objective.in, 	# lp parameters
	const.mat, 		# lp parameters
	const.dir,		# lp parameters
	const.rhs, 		# lp parameters
	binary.vec = 0,	# lp parameters	
	lb = 0,			# vector with lower bounds
	ub = +Inf,		# vector with upper bounds
	default.lb = -100	# the smallest possible value for any variable
)
{
	# number variables
	n = len(objective.in)
	
	if( len(lb) == 1 ) lb = rep(lb, n)
	if( len(ub) == 1 ) ub = rep(ub, n)
	
	lb = ifna(lb, default.lb)
	ub = ifna(ub, +Inf)
	
	lb[ lb < default.lb ] = default.lb
	dvec = lb
	
	# add ub constraints
	index = which( ub < +Inf )	
	if( len(index) > 0 ) {
		const.rhs = c(const.rhs, ub[index])
		const.dir = c(const.dir, rep('<=', len(index)))		
		const.mat = rbind(const.mat, diag(n)[index, ])		
	}
	
	# main logic
	if ( binary.vec[1] == 0 ) {	
		sol = lp( direction, objective.in, const.mat, const.dir, 
				const.rhs - const.mat %*% dvec )
	} else {
		dvec[binary.vec] = 0
		sol = lp( direction, objective.in, const.mat, const.dir, 
				const.rhs - const.mat %*% dvec, binary.vec = binary.vec )
	
	}
	
	# update solution
	sol$solution = sol$solution + dvec		
	sol$value = objective.in %*% sol$solution 	
	
	return( sol )	
}


###############################################################################
# Solve QP problem, handling binary variables using Binary Branch and Bound
#' @export 
###############################################################################
solve.QP.bounds <- function
(
	Dmat,				# solve.QP parameters
	dvec, 				# solve.QP parameters
	Amat, 				# solve.QP parameters
	bvec, 				# solve.QP parameters
	meq=0, 				# solve.QP parameters
	factorized=FALSE, 	# solve.QP parameters
	binary.vec = 0,		# vector with indices of binary variables
	lb = -Inf,			# vector with lower bounds
	ub = +Inf			# vector with upper bounds
)
{
	Amat1 = Amat
	bvec1 = bvec

	# number variables
	n = len(dvec)
	
	if( len(lb) == 1 ) lb = rep(lb, n)
	if( len(ub) == 1 ) ub = rep(ub, n)
	
	lb = ifna(lb, -Inf)
	ub = ifna(ub, +Inf)
	
	
	# add ub constraints, A^T b >= b_0
	index = which( ub < +Inf )	
	if( len(index) > 0 ) {	
		bvec = c(bvec, -ub[index])
		Amat = cbind(Amat, -diag(n)[, index])		
	}
	
	# add lb constraints, A^T b >= b_0
	index = which( lb > -Inf )	
	if( len(index) > 0 ) {	
		bvec = c(bvec, lb[index])
		Amat = cbind(Amat, diag(n)[, index])		
	}
		
	# main logic
	if ( binary.vec[1] == 0 ) {	
		# logic to check for constant variables	
		qp.data.final = solve.QP.remove.equality.constraints(Dmat, dvec, Amat, bvec, meq)	
			Dmat = qp.data.final$Dmat
			dvec = qp.data.final$dvec
			Amat = qp.data.final$Amat
			bvec = qp.data.final$bvec
			meq = qp.data.final$meq

		# solve.QP: min(-d^T w.i + 1/2 w.i^T D w.i) subject to A^T w.i >= b_0			
		sol = try(solve.QP(Dmat, dvec, Amat, bvec, meq, factorized),TRUE)
		
		# lot's of errors take place because covariance matrix is not positive defined
		if(inherits(sol, 'try-error')) {
#cat('solve.QP error\n')	
#print(sol)
			ok = F
			sol = list()				
		} else {
			# check that solve.QP solution satisfies constraints		
			tol = 1e-3
			ok = T
					
			check = sol$solution %*% Amat - bvec
			if(meq > 0) ok = ok & all(abs(check[1:meq]) <= tol)
			ok = ok & all(check[-c(1:meq)] > -tol)
#if(!ok) cat('solve.QP violates constraints\n')	
#if(!ok) print(sol$solution)					
		} 
		
		# try to solve problem one more time using ipop function from kernlab package
		if(!ok) {
			require(kernlab)
#cat('kernlab\n')	
			index.constant.variables = which(!is.na(qp.data.final$solution))
			if( len(index.constant.variables) > 0 ) {
				# Amat1 and bvec1 might be incorrect
				Amat1 = Amat[,1:ncol(Amat1)]
				bvec1 = bvec[1:ncol(Amat1)]
				lb = lb[-index.constant.variables]
				ub = ub[-index.constant.variables]
			}
						
			# ipop: min(c'*x + 1/2 * x' * H * x) subject to b <= A * x <= b + r and l <= x <= u
			sv = ipop(c = matrix(-dvec), H = Dmat, A = t(Amat1),
					b = bvec1, l = ifna(lb,-100), u = ifna(ub,100),
					r = c(rep(0,meq), rep(100, len(bvec1) - meq))
				)
			sol$solution = primal(sv)
		}

		# logic to check for constant variables	
		x = qp.data.final$solution
		x[qp.data.final$var.index] = sol$solution
		sol$solution = x
		
	} else {
		# use Binary Branch and Bound
		qp_data = qp_new(binary.vec, Dmat = Dmat, dvec = dvec, 
						Amat=Amat, bvec=bvec, meq=meq, factorized=factorized)
	
		sol = binary_branch_bound(binary.vec, qp_data, qp_solve, 
				control = bbb_control(silent=T, branchvar='max', searchdir='best' ))
				
		qp_delete(qp_data)		
		
		sol$value = sol$fmin
		sol$solution = sol$xmin
	}
	
	return(sol)
}


###############################################################################
# QP interface for Binary Branch and Bound algorithm
###############################################################################
# control list for binary_branch_bound				
#' @export 
###############################################################################
bbb_control <- function
(
	itermax = 200, 			# maximum number of iterations 
	depthmax = Inf, 		# maximum search depth
	bineps = 1e-4, 			# eps to determine whether a solution is 0/1
	precisioneps = 0, 		# stop condition for incremental improvement in best solution
	silent = T,				# quite or verbose
	branchvar = c('first', 'max','min'),
							# first = first free variable
							# max = variable with max frac part
							# min = variable with min frac part
	proborder = c('0', '1', 'mindiff'),
							# 0 = problems are put onto the stack '0' first, '1' next
							# 1 = problems are put onto the stack '1' first, '0' next
							# mindiff = problems are put onto the stack closest to '0' or '1' first	
	searchdir = c('depth', 'breadth', 'best', 'normbest')
							# depth = depth first
							# breadth = breadth first
							# best = best first
							# normbest = best first with normalized cost	
)
{
	# parse default arguments				
	branchvar = switch(branchvar[1],
						'first' = 0,
						'max' = 1,
						'min' = 2,
						0)
	branchvar = iif( is.null(branchvar),0, branchvar)
					
	proborder = switch(proborder[1],
						'0' = 0,
						'1' = 1,
						'mindiff' = 2,
						0)
	proborder = iif( is.null(proborder),0, proborder)
				
	searchdir = switch(searchdir[1],
						'depth' = 0,
						'breadth' = 1,
						'best' = 2,
						'normbest' = 2,
						0)
	searchdir = iif( is.null(searchdir),0, searchdir)	
	
	control = list(itermax = itermax, depthmax = depthmax, bineps = bineps, precisioneps = precisioneps, silent = silent,
				branchvar = branchvar,
				proborder = proborder,
				searchdir = searchdir)
	return(control)
}

###############################################################################
# Initialize QP
###############################################################################
qp_new <- function
(
	index_binvar, 	# index of binary variables
	Dmat, 			# paramters from solve.QP from quaprog library
	dvec, 
	Amat, 
	bvec, 
	meq = 0, 
	factorized = FALSE
)
{	
	# add 0,1 bounds for the binary variables
		nbinvar = length(index_binvar)
		nx = nrow(Dmat)	
		nbvec = length(bvec)
		
	# Note 0, -1 bounds 0 to 1			
	Amat = cbind( Amat, diag(nx)[,index_binvar], -diag(nx)[,index_binvar] )			
	bvec = c(bvec, rep(0,nx)[index_binvar], rep(1,nx)[index_binvar] )
		lb_bin_index = (1:nbinvar) + nbvec
		ub_bin_index = (1:nbinvar) + nbvec + nbinvar
		
	# create output
	qp_data = new.env()
		qp_data$Dmat = Dmat
		qp_data$dvec = dvec
		qp_data$Amat = Amat
		qp_data$bvec = bvec
		qp_data$meq = meq
		qp_data$factorized = factorized
		qp_data$x0 = rep(0,nx)
		
		qp_data$lb_bin_index = lb_bin_index
		qp_data$ub_bin_index = ub_bin_index
		qp_data$lb = bvec[lb_bin_index]		
		qp_data$ub = bvec[ub_bin_index]		
		
	return(qp_data)
}

###############################################################################
# Delete QP
###############################################################################
qp_delete <- function(qp_data)
{
	rm(list = ls(qp_data,all=TRUE), envir = qp_data)
}

###############################################################################
# Solve QP problem
###############################################################################
qp_solve <- function
(
	qp_data, 
	lb, 
	ub
)
{
	bvec = qp_data$bvec
	bvec[qp_data$lb_bin_index] = lb
	# Note 0, -1 bounds 0 to 1			
	bvec[qp_data$ub_bin_index] = -ub
	

	# NEW logic to remove reduandant equality constraints in QP problem
	qp.data.final = solve.QP.remove.equality.constraints(qp_data$Dmat, qp_data$dvec, 
							qp_data$Amat, bvec, qp_data$meq)		
	# end of NEW logic
		
	
	sol = tryCatch( solve.QP(Dmat=qp.data.final$Dmat, dvec=qp.data.final$dvec, Amat=qp.data.final$Amat, 
						bvec=qp.data.final$bvec, meq=qp.data.final$meq, factorized=qp_data$factorized),
		error=function( err ) FALSE,
	    warning=function( warn ) FALSE )
			
	    
	if( !is.logical( sol ) ) {   
		x = qp.data.final$solution
		x[qp.data.final$var.index] = sol$solution

		return(list( ok = TRUE, x = x, fval = sol$value )) 
	} else {
		return(list( ok = FALSE )) 
	}              	    
}








###############################################################################
# Test QP interface to Binary Branch and Bound algorithm
###############################################################################
mbqp.test <- function()
{
	load.packages('quadprog')

	
	# Test problem
	# min         0.5*x'Q x + b' x
	# subject to  Cx <= d	
	Q = diag(4)
	b	= c( 2, -3, -2, -3)
	C	= matrix(c(-1,  -1,  -1,  -1,
    				10,	5,   3,	4,
    				-1,	0,   0,	0),
    		3,4,byrow = TRUE)
	d = c(-2, 10,  0)

	vlb  = c(-1e10, 0, 0, 0);
	vub  = c( 1e10, 1, 1, 1);

	index_binvar = c(2, 3, 4);

	# Solve.QP
	# min(-d^T b + 1/2 b^T D b) : A^T b >= b_0
	# meq the first meq constraints are treated as equality constraints, all further as inequality constraints (defaults to 0).  
	Dmat = Q
	dvec = -b
	Amat = -t(C)
	bvec = -d

	# add vlb / vub
	n = nrow(Dmat)
	Amat = cbind( Amat, diag(n), -diag(n) )
	bvec = c( bvec, vlb, -vub )

	sol = solve.QP(Dmat=Dmat, dvec=dvec, Amat=Amat, bvec=bvec, meq=0) 
		xsol = sol$solution
		fsol = sol$value
	
	cat('QP.solve fsol =', fsol, 'xsol =', xsol, '\n')

	# Start Branch and Bound 
	sol = solve.QP.bounds(Dmat=Dmat, dvec=dvec, Amat=Amat, bvec=bvec, meq=0, binary.vec = index_binvar) 
		xsol = sol$solution
		fsol = sol$value

	cat('QP.solve Binary Branch and Bound fsol =', fsol, 'xsol =', xsol, '\n')

}

###############################################################################
# Oriingal version is too slow working on a new faster version
###############################################################################
# Logic to remove reduandant equality constraints in QP problem, based on qp_solve
solve.QP.remove.equality.constraints <- function
(
	Dmat,				# solve.QP parameters
	dvec, 				# solve.QP parameters
	Amat, 				# solve.QP parameters
	bvec, 				# solve.QP parameters
	meq=0 				# solve.QP parameters
)
{
	qp.data = list()	
		qp.data$Amat = Amat
		qp.data$bvec = bvec
		qp.data$Dmat = Dmat
		qp.data$dvec = dvec
		qp.data$meq = meq	
	
		
	Amat1 = t(qp.data$Amat)
	bvec1 = qp.data$bvec
	Dmat1 = qp.data$Dmat
	dvec1 = qp.data$dvec
	meq1 = qp.data$meq
		
	
#Amat - var are rows, constr are columns	
#Amat1 - constr are rows, var are columns
# 	Amat = matrix(1:20,nr=4) 
#	Amat1 = t(Amat)
#	(1+0*Amat1[2:4,])/ c(1,2,3)

	# default
	qp.data$solution = rep(NA, ncol(Amat1))
	qp.data$var.index = 1:ncol(Amat1)
	

while(T) {


	# 1. find columns with just one non-zero element
	one.non.zero.index = which( rowSums(Amat1!=0) == 1 )

	if( len(one.non.zero.index) == 0 ) break
	
	# 2. divide these columns, and corresponding bvec by column's abs value	
	temp0 = rowSums(Amat1[one.non.zero.index,])
	temp = abs( temp0 )
	bvec1[one.non.zero.index] = bvec1[one.non.zero.index] / temp
	Amat1[one.non.zero.index,] = Amat1[one.non.zero.index,] / temp
		
	
	
	temp0.index = matrix(1:ncol(Amat1), nr=ncol(Amat1), nc=len(one.non.zero.index))[t(Amat1[one.non.zero.index,]!=0)]
	

#Amat - var are rows, constr are columns	
#Amat1 - constr are rows, var are columns	
	
	
	# 3. look for pairs among one.non.zero.index colums
	# x >= value , -x>= -value => x = value
	equality.constraints = rep(NA, ncol(Amat1))
	lb = ub = rep(NA, ncol(Amat1))
	
	
	# Make sure if there are multiple matches only keep max for lb and min for ub
	
	index = temp0 > 0
	#lb[temp0.index[index]] = bvec1[one.non.zero.index[index]]
	##temp = tapply(bvec1[one.non.zero.index[index]], temp0.index[index], max)
	##lb[ as.double(names(temp)) ] = temp
	temp = order(bvec1[one.non.zero.index[index]], decreasing = FALSE)
	lb[temp0.index[index][temp]] = bvec1[one.non.zero.index[index]][temp]
	
	index = temp0 < 0
	#ub[temp0.index[index]] = -bvec1[one.non.zero.index[index]]
	##temp = tapply(-bvec1[one.non.zero.index[index]], temp0.index[index], min)
	##ub[ as.double(names(temp)) ] = temp	
	temp = order(-bvec1[one.non.zero.index[index]], decreasing = TRUE)
	ub[temp0.index[index][temp]] = -bvec1[one.non.zero.index[index]][temp]
	
#a = rep(0,5)
#a[c(2,2,2)] = c(2,1,0)

	
	# 4. remove variables
	remove.index = which(lb == ub)
	if( len(remove.index) > 0 ) {
		equality.constraints[remove.index] = lb[remove.index]
		
		# remove variables
		Dmat1 = Dmat1[-remove.index, -remove.index,drop=F]
		dvec1 = dvec1[-remove.index]
		bvec1 = bvec1 - Amat1[,remove.index,drop=F] %*% equality.constraints[remove.index]
		Amat1 = Amat1[,-remove.index,drop=F]
		
		qp.data$solution[ qp.data$var.index[remove.index] ] = lb[remove.index]
		qp.data$var.index = which(is.na(qp.data$solution))				

		# check if there are variables left		
		if( ncol(Amat1) > 0 ) {
				
			# remove constraints
			remove.index = which( rowSums(Amat1!=0) == 0 & bvec1 == 0 )
			if(len(remove.index)>0) {
				bvec1 = bvec1[-remove.index]
				Amat1 = Amat1[-remove.index,,drop=F]
					
				# handle euality constraints
				if( meq1 > 0 ) meq1 = meq1 - len(intersect((1:meq1), remove.index))
			}
		} else break
				
						
	} else break
	
	}	# while(T)

	# prepare return object
	qp.data$Amat = t(Amat1)
	qp.data$bvec = bvec1
	qp.data$Dmat = Dmat1
	qp.data$dvec = dvec1
	qp.data$meq = meq1
	
	return(qp.data)
}

	
###############################################################################
# Find and remove equality constraints in QP problem (slow)
###############################################################################
# Logic to remove reduandant equality constraints in QP problem, based on qp_solve
solve.QP.remove.equality.constraints12 <- function
(
	Dmat,				# solve.QP parameters
	dvec, 				# solve.QP parameters
	Amat, 				# solve.QP parameters
	bvec, 				# solve.QP parameters
	meq=0 				# solve.QP parameters
)
{
	qp.data.temp = list()	
		qp.data.temp$Amat = Amat
		qp.data.temp$bvec = bvec
		qp.data.temp$Dmat = Dmat
		qp.data.temp$dvec = dvec
		qp.data.temp$meq = meq	
	
		
	qp.data.temp = remove.equality.constraints.old(qp.data.temp)
	# if no equality constraints found go to optimization
	if( len(qp.data.temp$var.index) == len(qp.data.temp$solution) ) {
		qp.data.final = qp.data.temp
	} else {
		# test for equality constraints one more time
		qp.data.final = remove.equality.constraints.old(qp.data.temp)		
			qp.data.temp$solution[qp.data.temp$var.index] = qp.data.final$solution			
				qp.data.final$solution = qp.data.temp$solution			
			qp.data.final$var.index = qp.data.temp$var.index[qp.data.final$var.index]
	}
		
	return(qp.data.final)
}


remove.equality.constraints.old <- function(qp.data)
{
	#qp.data1 = qp.data.temp
	Amat1 = qp.data$Amat
	bvec1 = qp.data$bvec
	Dmat1 = qp.data$Dmat
	dvec1 = qp.data$dvec
	meq1 = qp.data$meq
		
	# 1. find columns with just one non-zero element
	one.non.zero.index = which( colSums(Amat1!=0) == 1 )
	
	if( len(one.non.zero.index) == 0 ) {	
		equality.constraints = rep(NA, nrow(Amat1))
		qp.data$solution = equality.constraints		
		qp.data$var.index = which(is.na(equality.constraints))
		return(qp.data)
	}
	
	# 2. divide these columns, and corresponding bvec by column's abs value	
	bvec1[one.non.zero.index] = bvec1[one.non.zero.index] / abs( colSums(Amat1[,one.non.zero.index]) )
	Amat1[,one.non.zero.index] = Amat1[,one.non.zero.index] / abs( Amat1[,one.non.zero.index] )
	Amat1[is.na(Amat1)] = 0	

	
	# 3. look for pairs among one.non.zero.index colums
	# x >= value , -x>= -value => x = value
	equality.constraints = rep(NA, nrow(Amat1))
	remove.constraints = rep(NA, ncol(Amat1))
		
	#for( r in 1:nrow(Amat1) ) {
	for( r in which(rowSums(Amat1[,one.non.zero.index]!=0) > 1) ) {	
		temp.index = which( Amat1[,one.non.zero.index][r,] != 0 )
		
		for( r1 in temp.index[-1] ) {
			temp.index1 = colSums(abs(
			rbind(Amat1,bvec1)[,one.non.zero.index[temp.index]] + 
			rbind(Amat1,bvec1)[,one.non.zero.index[r1]])
			)
			
			if( any(temp.index1 == 0 ) ) {
				equality.constraints[r] = 
					bvec1[one.non.zero.index[r1]] / Amat1[r,one.non.zero.index[r1]]
					
				remove.constraints[ one.non.zero.index[r1] ] = 1				
				remove.constraints[ one.non.zero.index[ temp.index[ temp.index1 == 0]]] = 1
				
				break;
			}
		}
	}
	
	
	# 4. remove variables
	remove.index = which(!is.na(equality.constraints))
	if(len(remove.index)>0) {
		# remove variables
		Dmat1 = Dmat1[-remove.index, -remove.index,drop=F]
		dvec1 = dvec1[-remove.index]
		bvec1 = bvec1 - equality.constraints[remove.index] %*% Amat1[remove.index,,drop=F]
		Amat1 = Amat1[-remove.index,,drop=F]
	
		# remove constraints
		#remove.index1 = which( colSums(abs(Amat1)) == 0 & bvec1 == 0)		
		remove.index1 = which(remove.constraints==1)
		if(len(remove.index1)>0) {
			bvec1 = bvec1[-remove.index1]
			Amat1 = Amat1[,-remove.index1,drop=F]
				
			# handle euality constraints
			if( meq1 > 0 ) meq1 = meq1 - len(intersect((1:meq1), remove.index1))
		}
	}
	
	# prepare return object
	qp.data$Amat = Amat1
	qp.data$bvec = bvec1
	qp.data$Dmat = Dmat1
	qp.data$dvec = dvec1
	qp.data$meq = meq1

	qp.data$solution = equality.constraints		
	qp.data$var.index = which(is.na(equality.constraints))
	
	return(qp.data)
}


	
###############################################################################
# Run linear least squares regression with constraints
#' @export 
###############################################################################
lm.constraint <- function
(
	x,
	y,
	constraints = NULL
)
{
	if( is.null(constraints) ) {
		fit = lm.fit(x, y)
		return( ols.summary(x, y, fit$coefficients) )
	} else {
    	temp = cov(cbind(y, x))
		Dmat = temp[-1,-1] 
		dvec = temp[-1,1]
	
		sol = solve.QP.bounds(Dmat = Dmat, dvec = dvec , 
				Amat=constraints$A, bvec=constraints$b, constraints$meq,
				lb = constraints$lb, ub = constraints$ub)
		return( ols.summary(x, y, sol$solution) )
	}
}


###############################################################################
# Run linear least squares regression
#' @export 
###############################################################################
ols <- function
(
	x,
	y,
	computeSummary=F
)
{
	xx = t(x) %*% x
	
	if(is.null(ncol(xx))) { xinv = inv1(xx) 
	} else if(ncol(xx) == 1) { xinv = inv1(xx) 
	} else if(ncol(xx) == 2) { xinv = inv2(xx) 
	} else if(ncol(xx) == 3) { xinv = inv3(xx) 
	} else { xinv = inv(xx) }
	
	coefficients = xinv %*% t(x) %*% y
	if(computeSummary) {
		return( ols.summary(x, y, coefficients, xinv) )
	} else {
		return(list(coefficients = coefficients))
	}
}


#' @export 	
ols.summary <- function
(
	x,
	y,
	coefficients,
	xinv = NULL
)
{
	n = length(y) 
	p = length(coefficients) 
	rdf = n-p

	# error		
   	e = y - x %*% coefficients                        
    ess=sum(e^2)
	mss = sum((y - sum(y)/n)^2)
	r.squared = 1 - ess/mss
	#r.squared = 1 - (var(e)/var(y))
	#adj.r.squared = 1 - (1 - r.squared)*(n - 1)/(n - p - 1)
    
	if( !is.null(xinv) ) {
		s2=ess/(rdf)
		seb=sqrt(diag(s2*xinv))	
		tratio=coefficients/seb	
		
		return(list(coefficients = coefficients, seb = seb,tratio = tratio, r.squared = r.squared))
	} else {
		return(list(coefficients = coefficients, r.squared = r.squared))
	}
}

ols.test <- function() {
	x = matrix( rnorm(4*10), ncol=4)
	y = rnorm(10)

	summary(lm(y ~ x+0))
	ols(x, y, T)
}


###############################################################################
# Compute matrix inverse
###############################################################################
#' @export 
inv <- function(x) { solve(x) }

#' @export 
inv1 <- function(x) { 1/x }

#http://www.mathwords.com/i/inverse_of_a_matrix.htm
#' @export 
inv2 <- function(x) 
{ 
	matrix(c(x[2,2],-x[1,2],-x[2,1],x[1,1]),nrow=2,byrow=T) / (x[1,1]*x[2,2] - x[1,2]*x[2,1]) 
}

#http://www.dr-lex.be/random/matrix_inv.html
#' @export 
inv3 <- function(x) 
{ 
	matrix(c(x[3,3]*x[2,2]-x[3,2]*x[2,3],-(x[3,3]*x[1,2]-x[3,2]*x[1,3]),x[2,3]*x[1,2]-x[2,2]*x[1,3],
	-(x[3,3]*x[2,1]-x[3,1]*x[2,3]),x[3,3]*x[1,1]-x[3,1]*x[1,3],-(x[2,3]*x[1,1]-x[2,1]*x[1,3]),
	x[3,2]*x[2,1]-x[3,1]*x[2,2],-(x[3,2]*x[1,1]-x[3,1]*x[1,2]),x[2,2]*x[1,1]-x[2,1]*x[1,2]),nrow=3,byrow=T) / 
	(x[1,1]*(x[3,3]*x[2,2]-x[3,2]*x[2,3])-x[2,1]*(x[3,3]*x[1,2]-x[3,2]*x[1,3])+x[3,1]*(x[2,3]*x[1,2]-x[2,2]*x[1,3])) 
}
 
inv.test <- function() {
	m=matrix(c(4,3,3,2),nrow=2,byrow=T)
	inv2(m) %*% m
	inv(m) %*% m

	m = matrix(c(1,2,3,4,5,6,7,8,8),ncol=3,byrow=T)
	inv3(m) %*% m
	m %*% inv3(m)
	inv(m) %*% m
		
}


###############################################################################
# Maximum Distance Point on the curve
# http://stackoverflow.com/questions/2018178/finding-the-best-trade-off-point-on-a-curve		
#' @export 
###############################################################################
find.maximum.distance.point <- function
(
	y, 
	x=1:len(y)
)
{		
	allCoord = rbind(vec(y), vec(x))
		
	firstPoint = allCoord[,1]
	lineVec = allCoord[,len(y)] - firstPoint
	lineVecN = lineVec / sqrt(sum(lineVec^2))
		
	vecFromFirst = allCoord - firstPoint
	scalarProduct = lineVecN %*% vecFromFirst
		
	vecFromFirstParallel = t(scalarProduct) %*% lineVecN
	vecToLine = t(vecFromFirst) - vecFromFirstParallel
	distToLine = sqrt(rowSums(vecToLine^2,2))
	which.max(distToLine)
}
	