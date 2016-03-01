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
# Asset Allocation Functions
# Copyright (C) 2011  Michael Kapler
#
# For more information please visit my blog at www.SystematicInvestor.wordpress.com
# or drop me a line at TheSystematicInvestor at gmail
###############################################################################


###############################################################################
# Building constraints for quadprog, solve.QP
# min(-d^T w.i + 1/2 w.i^T D w.i) constraints A^T w.i >= b_0
#  the first meq constraints are treated as equality constraints, 
#  all further as inequality constraints
###############################################################################
# new.constraints - create new constraints structure
#' @export 
###############################################################################
new.constraints <- function
(
	n,			# number of variables
	A = NULL,	# matrix with constraints 
	b = NULL,	# vector b
	type = c('=', '>=', '<='),	# type of constraints
	lb = NA,	# vector with lower bounds
	ub = NA		# vector with upper bounds
)
{
	meq = 0
	if ( is.null(A) || is.na(A) || is.null(b) || is.na(b) ) {
		A = matrix(0, n, 0)
		b = c()
	} else {
		if ( is.null(dim(A)) ) dim(A) = c(len(A), 1)
	
		if ( type[1] == '=' ) meq = len(b)
		if ( type[1] == '<=' ) {
			A = -A
			b = -b
		}
	}
	
	if ( is.null(lb) || is.na(lb) ) lb = rep(NA, n)
	if ( len(lb) != n ) lb = rep(lb[1], n)

	if ( is.null(ub) || is.na(ub) ) ub = rep(NA, n)
	if ( len(ub) != n ) ub = rep(ub[1], n)
		
	
	return( list(n = n, A = A, b = b, meq = meq, lb = lb, ub = ub) )
}

###############################################################################
# add.constraints - add to existing constraints structure
#' @export 
###############################################################################
add.constraints <- function
(
	A,			# matrix with constraints 
	b,			# vector b
	type = c('=', '>=', '<='),	# type of constraints
	constraints	# constraints structure
)
{
	if(is.null(constraints)) constraints = new.constraints(n = nrow(A))
	
	if(is.null(dim(A))) A = matrix(A)
	
	if(len(b) == 1) b = rep(b, ncol(A))
	
	if ( type[1] == '=' ) {
		constraints$A = cbind( A, constraints$A )
		constraints$b = c( b, constraints$b )
		constraints$meq = constraints$meq + len(b)
	}
		
	if ( type[1] == '>=' ) {
		constraints$A = cbind( constraints$A, A )
		constraints$b = c( constraints$b, b )	
	}

	if ( type[1] == '<=' ) {
		constraints$A = cbind( constraints$A, -A )
		constraints$b = c( constraints$b, -b )	
	}
	
	return( constraints )			
}

###############################################################################
# add.variables - add to existing constraints structure
#' @export 
###############################################################################
add.variables <- function
(
	n,			# number of variables to add
	constraints,	# constraints structure
	lb = NA,	# vector with lower bounds
	ub = NA	# vector with upper bounds		
)
{
	constraints$A = rbind( constraints$A, matrix(0, n, len(constraints$b)) )

	if ( is.null(lb) || is.na(lb) ) lb = rep(NA, n)
	if ( len(lb) != n ) lb = rep(lb[1], n)

	if ( is.null(ub) || is.na(ub) ) ub = rep(NA, n)
	if ( len(ub) != n ) ub = rep(ub[1], n)
			
	constraints$lb = c(constraints$lb, lb)
	constraints$ub = c(constraints$ub, ub)
	constraints$n = constraints$n + n
	
	return( constraints )			
}


###############################################################################
# delete.constraints - remove specified constraints from existing constraints structure
#' @export 
###############################################################################
delete.constraints <- function
(
	delete.index,	# index of constraints to delete 
	constraints	# constraints structure	
)
{
	constraints$A = constraints$A[, -delete.index, drop=F]
	constraints$b = constraints$b[ -delete.index]
	constraints$meq = constraints$meq - len(intersect((1:constraints$meq), delete.index))	
	return( constraints )				
}

#' @export 
type.constraints <- function(constraints)
{
	c(rep('=', constraints$meq), rep('>=', len(constraints$b) - constraints$meq))
}


###############################################################################
# create.basic.constraints - create basic constraints
#' @export 
###############################################################################
create.basic.constraints <- function(
	n,
	const.lb = 0, 
	const.ub = 1,
	const.sum = 1
)
{
	if(len(const.lb) == 1) const.lb = rep(const.lb, n)
	if(len(const.ub) == 1) const.ub = rep(const.ub, n)

	# 0 <= x.i <= 1
	constraints = new.constraints(n, lb = const.lb, ub = const.ub)
		constraints = add.constraints(diag(n), type='>=', b=const.lb, constraints)
		constraints = add.constraints(diag(n), type='<=', b=const.ub, constraints)
	
	# SUM x.i = 1
	if(!is.na(const.sum))
		constraints = add.constraints(rep(1, n), type = '=', b=const.sum, constraints)
	
	return(constraints)
}


###############################################################################
# create.basic.constraints - create basic constraints
#' @export 
###############################################################################
merge.constraints <- function(constraints1, constraints2) {
	if(constraints1$n != constraints2$n) stop('merge.constraints: both constraints must be based on same number of assets')
	
	if(constraints2$meq > 0) {
		constraints1$A = cbind( constraints2$A[,1:constraints2$meq], constraints1$A, constraints2$A[,-c(1:constraints2$meq)] )
		constraints1$b = c( constraints2$b[1:constraints2$meq], constraints1$b, constraints2$b[-c(1:constraints2$meq)] )
		constraints1$meq = constraints1$meq + constraints2$meq
	} else {
		constraints1$A = cbind( constraints1$A, constraints2$A)
		constraints1$b = c(constraints1$b, constraints2$b)
	}
	
	constraints1$lb =  pmax(constraints1$lb, constraints2$lb, na.rm=T)
	constraints1$ub =  pmin(constraints1$ub, constraints2$ub, na.rm=T)
	
	constraints1
}



###############################################################################
# General interface to Finding portfolio that Minimizes Given Risk Measure
#' @export 
###############################################################################
min.portfolio <- function
(
	ia,					# input assumptions
	constraints,		# constraints
	add.constraint.fn,
	min.risk.fn	
)
{
	optimize.portfolio(ia, constraints, add.constraint.fn, min.risk.fn)
}

#' @export 
optimize.portfolio <- function
(
	ia,					# input assumptions
	constraints,		# constraints
	add.constraint.fn,
	min.risk.fn,
	direction = 'min',
	full.solution = F
)
{
	# load / check required packages
	load.packages('quadprog,corpcor,lpSolve,kernlab')
	
	
	n = nrow(constraints$A)	
	nt = nrow(ia$hist.returns)
	
	# objective is stored as a last constraint
	constraints = match.fun(add.constraint.fn)(ia, 0, '>=', constraints)				
	
	f.obj = constraints$A[, ncol(constraints$A)]
		constraints = delete.constraints( ncol(constraints$A), constraints)

	# setup constraints
	f.con = constraints$A
	f.dir = c(rep('=', constraints$meq), rep('>=', len(constraints$b) - constraints$meq))
	f.rhs = constraints$b
			
	# find optimal solution	
	x = NA
	
	binary.vec = 0
	if(!is.null(constraints$binary.index)) binary.vec = constraints$binary.index
		
	sol = try(solve.LP.bounds(direction, f.obj, t(f.con), f.dir, f.rhs, 
				lb = constraints$lb, ub = constraints$ub, binary.vec = binary.vec,
				default.lb = -100), TRUE)	
				
	if(!inherits(sol, 'try-error')) {
		x = sol$solution[1:n]
		
		#cat('sol.objval =', sol$objval, '\n')
		
		# to check
		if( F ) {
			f.obj %*% sol$solution  - match.fun(min.risk.fn)(t(x), ia)
		}
	}		

	if( full.solution ) x = sol 
	return( x )
}	

###############################################################################
# Rdonlp2 only works with R version before 2.9
# Rdonlp2 is not avilable for latest version of R
# for more help please visit http://arumat.net/Rdonlp2/
#
# Conditions of use:                                                        
# 1. donlp2 is under the exclusive copyright of P. Spellucci                
#    (e-mail:spellucci@mathematik.tu-darmstadt.de)                          
#    "donlp2" is a reserved name                                            
# 2. donlp2 and its constituent parts come with no warranty, whether ex-    
#    pressed or implied, that it is free of errors or suitable for any      
#    specific purpose.                                                      
#    It must not be used to solve any problem, whose incorrect solution     
#    could result in injury to a person , institution or property.          
#    It is at the users own risk to use donlp2 or parts of it and the       
#    author disclaims all liability for such use.                           
# 3. donlp2 is distributed "as is". In particular, no maintenance, support  
#    or trouble-shooting or subsequent upgrade is implied.                  
# 4. The use of donlp2 must be acknowledged, in any publication which contains                                                               
#    results obtained with it or parts of it. Citation of the authors name  
#    and netlib-source is suitable.                                         
# 5. The free use of donlp2 and parts of it is restricted for research purposes                                                               
#    commercial uses require permission and licensing from P. Spellucci.    
#' @export 
###############################################################################
optimize.portfolio.nlp <- function
(
	ia,					# input assumptions
	constraints,		# constraints
	fn,
	nl.constraints = NULL,	# Non-Linear constraints
	direction = 'min',
	full.solution = F
)
{
	# Rdonlp2 only works with R version before 2.9
	load.packages('Rdonlp2', repos ='http://R-Forge.R-project.org')

	# fnscale(1) - set -1 for maximization instead of minimization.
	if( direction == 'min' ) fnscale = 1 else fnscale = -1
	
	# control structure: as.numeric( sessionInfo()$R.version$minor ) < 9 
	cntl = donlp2Control()
	cntl$silent = T
	cntl$fnscale = fnscale
	cntl$iterma =10000
	cntl$nstep = 100
	cntl$epsx = 1e-10	
	
	# lower/upper bounds
	par.l = constraints$lb
	par.u = constraints$ub
	
	# intial guess
	p = rep(1, nrow(constraints$A))
	if(!is.null(constraints$x0)) {
		# if no NA's in constraints$x0
		if( sum(is.na(constraints$x0)) == 0) p = constraints$x0
	}
		
	# linear constraints
	A = t(constraints$A)
	lin.l = constraints$b
	lin.u = constraints$b
	lin.u[ -c(1:constraints$meq) ] = +Inf

	# find optimal solution	
	x = NA
	
	if( !is.null(nl.constraints) ) {
		sol = donlp2(p, fn, 
					par.lower=par.l, par.upper=par.u, 
					A=A, lin.u=lin.u, lin.l=lin.l, 
					control=cntl,					
					nlin=nl.constraints$constraints,
					nlin.upper=nl.constraints$upper, nlin.lower=nl.constraints$lower					
					)
	} else {
		sol = donlp2(p, fn, 
					par.lower=par.l, par.upper=par.u, 
					A=A, lin.u=lin.u, lin.l=lin.l, 
					control=cntl)
	}
				
	if(!inherits(sol, 'try-error')) {
		x = sol$par
	}		

	if( full.solution ) x = sol 
	return( x )
}	





###############################################################################
# Maximum Loss
# page 34, Comparative Analysis of Linear Portfolio Rebalancing Strategies by Krokhmal, Uryasev, Zrazhevsky  
#
# Let x.i , i= 1,...,n  be weights of instruments in the portfolio.
# Let us suppose that j = 1,...,T scenarios of returns are available 
# ( r.ij denotes return of i -th asset in the scenario j ). 
#
# The Maximum Loss (MaxLoss) function has the form 
#  w
#  such that
#  - [ SUM <over i> r.ij * x.i ] < w, for each j = 1,...,T 
#' @export 
###############################################################################
add.constraint.maxloss <- function
(
	ia,			# input assumptions
	value,		# b value
	type = c('=', '>=', '<='),	# type of constraints
	constraints	# constraints structure
)
{
	n0 = ncol(ia$hist.returns)
	n = nrow(constraints$A)	
	nt = nrow(ia$hist.returns)

	# adjust constraints, add w
	constraints = add.variables(1, constraints)
	
	#  - [ SUM <over i> r.ij * x.i ] < w, for each j = 1,...,T 
	a = rbind( matrix(0, n, nt), 1)
		a[1 : n0, ] = t(ia$hist.returns)
	constraints = add.constraints(a, rep(0, nt), '>=', constraints)

	# objective : maximum loss, w
	constraints = add.constraints(c(rep(0, n), 1), value, type[1], constraints)	
		
	return( constraints )	
}

#' @export 
portfolio.maxloss <- function
(
	weight,		# weight
	ia			# input assumptions
)	
{
	weight = weight[, 1:ia$n, drop=F]
	
	portfolio.returns = weight %*% t(ia$hist.returns)
	return( -apply(portfolio.returns, 1, min) )
}	

#' @export 
min.maxloss.portfolio <- function
(
	ia,				# input assumptions
	constraints		# constraints
)
{
	min.portfolio(ia, constraints, add.constraint.maxloss, portfolio.maxloss)
	
}










###############################################################################
# Mean-Absolute Deviation (MAD)
# page 33, Comparative Analysis of Linear Portfolio Rebalancing Strategies by Krokhmal, Uryasev, Zrazhevsky  
#
# Let x.i , i= 1,...,n  be weights of instruments in the portfolio.
# Let us suppose that j = 1,...,T scenarios of returns are available 
# ( r.ij denotes return of i -th asset in the scenario j ). 
#
# The Mean-Absolute Deviation (MAD) function has the form 
#  1/T * [ SUM <over j> (u+.j + u-.j) ]
#  such that
#  [ SUM <over i> r.ij * x.i ] - 1/T * [ SUM <over j> [ SUM <over i> r.ij * x.i ] ] = u+.j - u-.j , for each j = 1,...,T 
#  u+.j, u-.j >= 0, for each j = 1,...,T 
#' @export 
###############################################################################
add.constraint.mad <- function
(
	ia,			# input assumptions
	value,		# b value
	type = c('=', '>=', '<='),	# type of constraints
	constraints	# constraints structure
)
{
	n0 = ncol(ia$hist.returns)
	n = nrow(constraints$A)	
	nt = nrow(ia$hist.returns)

	# adjust constraints, add u+.j, u-.j
	constraints = add.variables(2 * nt, constraints, lb = 0)
				
	# [ SUM <over i> r.ij * x.i ] - 1/T * [ SUM <over j> [ SUM <over i> r.ij * x.i ] ] = u+.j - u-.j , for each j = 1,...,T 
	a = rbind( matrix(0, n, nt), -diag(nt), diag(nt))
		a[1 : n0, ] = t(ia$hist.returns) - repmat(colMeans(ia$hist.returns), 1, nt)
	constraints = add.constraints(a, rep(0, nt), '=', constraints)			

	# objective : Mean-Absolute Deviation (MAD)
	# 1/T * [ SUM <over j> (u+.j + u-.j) ]
	constraints = add.constraints(c(rep(0, n), (1/nt) * rep(1, 2 * nt)), value, type[1], constraints)	
		
	return( constraints )	
}

#' @export 
portfolio.mad <- function
(
	weight,		# weight
	ia			# input assumptions
)	
{
	if(is.null(dim(weight))) dim(weight) = c(1, len(weight))	
	weight = weight[, 1:ia$n, drop=F]
	
	portfolio.returns = weight %*% t(ia$hist.returns)
	return( apply(portfolio.returns, 1, function(x) mean(abs(x - mean(x))) ) )
}	

#' @export 
min.mad.portfolio <- function
(
	ia,				# input assumptions
	constraints		# constraints
)
{
	min.portfolio(ia, constraints, add.constraint.mad, portfolio.mad)	
}










###############################################################################
# Conditional Value at Risk (CVaR)
# page 30-32, Comparative Analysis of Linear Portfolio Rebalancing Strategies by Krokhmal, Uryasev, Zrazhevsky  
#
# Let x.i , i= 1,...,n  be weights of instruments in the portfolio.
# Let us suppose that j = 1,...,T scenarios of returns are available 
# ( r.ij denotes return of i -th asset in the scenario j ). 
#
# The Conditional Value at Risk (CVaR) function has the form 
#  E + 1/(1-a) * 1/T * [ SUM <over j> w.j ]
#  -E - [ SUM <over i> r.ij * x.i ] < w.j, for each j = 1,...,T 
#  w.j >= 0, for each j = 1,...,T 
#' @export 
###############################################################################
add.constraint.cvar <- function
(
	ia,			# input assumptions
	value,		# b value
	type = c('=', '>=', '<='),	# type of constraints
	constraints	# constraints structure
)
{
	if(is.null(ia$parameters.alpha)) alpha = 0.95 else alpha = ia$parameters.alpha

	n0 = ncol(ia$hist.returns)
	n = nrow(constraints$A)	
	nt = nrow(ia$hist.returns)

	# adjust constraints, add w.j, E
	constraints = add.variables(nt + 1, constraints, lb = c(rep(0,nt),-Inf))
			
	#  -E - [ SUM <over i> r.ij * x.i ] < w.j, for each j = 1,...,T 
	a = rbind( matrix(0, n, nt), diag(nt), 1)
		a[1 : n0, ] = t(ia$hist.returns)
	constraints = add.constraints(a, rep(0, nt), '>=', constraints)			

	# objective : Conditional Value at Risk (CVaR)
	#  E + 1/(1-a) * 1/T * [ SUM <over j> w.j ]
	constraints = add.constraints(c(rep(0, n), (1/(1-alpha))* (1/nt) * rep(1, nt), 1), value, type[1], constraints)	
		
	return( constraints )	
}

# average of portfolio returns that are below portfolio's VaR
#' @export 
portfolio.cvar <- function
(
	weight,		# weight
	ia			# input assumptions	
)	
{
	weight = weight[, 1:ia$n, drop=F]
	if(is.null(ia$parameters.alpha)) alpha = 0.95 else alpha = ia$parameters.alpha
	
	portfolio.returns = weight %*% t(ia$hist.returns)
	return( apply(portfolio.returns, 1, function(x) -compute.cvar(x, 1-alpha) ) )
}	

#' @export 
min.cvar.portfolio <- function
(
	ia,				# input assumptions
	constraints		# constraints
)
{
	min.portfolio(ia, constraints, add.constraint.cvar, portfolio.cvar)
}

###############################################################################
# portfolio.var
#' @export 
###############################################################################
portfolio.var <- function
(
	weight,		# weight
	ia			# input assumptions	
)	
{
	weight = weight[, 1:ia$n, drop=F]
	if(is.null(ia$parameters.alpha)) alpha = 0.95 else alpha = ia$parameters.alpha
	
	portfolio.returns = weight %*% t(ia$hist.returns)
	return( apply(portfolio.returns, 1, function(x) -compute.var(x, 1-alpha) ) )
}	

###############################################################################
# compute.var/cvar - What is the most I can with a 95% level of confidence expect to lose
# http://www.investopedia.com/articles/04/092904.asp
###############################################################################
#compute.var <- function(x, alpha){ return( -quantile(x, probs = (1-alpha)) )}
#compute.cvar <- function(x, alpha) { return( -mean(x[ x < quantile(x, probs = (1-alpha)) ]) )}










###############################################################################
# Conditional Drawdown at Risk (CDaR)
# page 33, Comparative Analysis of Linear Portfolio Rebalancing Strategies by Krokhmal, Uryasev, Zrazhevsky  
# page 15-20, Portfolio Optimization Using Conditional Value-At-Risk and Conditional Drawdown-At-Risk by Enn Kuutan
#
# Let x.i , i= 1,...,n  be weights of instruments in the portfolio.
# Let us suppose that j = 1,...,T scenarios of returns are available 
# ( r.ij denotes return of i -th asset in the scenario j ). 
#
# The Conditional Drawdown at Risk (CDaR) function has the form 
#  E + 1/(1-a) * 1/T * [ SUM <over j> w.j ]
#  u.j - [ SUM <over i> [ SUM <over j> r.ij ] * x.i ] - E < w.j, for each j = 1,...,T 
#  [ SUM <over i> [ SUM <over j> r.ij ] * x.i ] < u.j, for each j = 1,...,T 
#  u.j-1 < u.j, for each j = 1,...,T - portfolio high water mark
#  w.j >= 0, for each j = 1,...,T 
#
# Please note that MaxDD and AvgDD are special cases of CDaR i.e.
# MaxDD is CDaR with alpha = 1
# AvgDD is CDaR with alpha = 0
# DRAWDOWN MEASURE IN PORTFOLIO OPTIMIZATION, ALEXEI CHEKHLOV, STANISLAV URYASEV, MICHAEL ZABARANKIN 
# Proposition 3.4, page 25(13)
# http://www.math.columbia.edu/~chekhlov/IJTheoreticalAppliedFinance.8.1.2005.pdf
#
#' @export 
###############################################################################
add.constraint.cdar <- function
(
	ia,			# input assumptions
	value,		# b value
	type = c('=', '>=', '<='),	# type of constraints
	constraints	# constraints structure
)
{
	if(is.null(ia$parameters.alpha)) alpha = 0.95 else alpha = ia$parameters.alpha

	n0 = ncol(ia$hist.returns)
	n = nrow(constraints$A)	
	nt = nrow(ia$hist.returns)

	# adjust constraints, add w.j, E, u.j
	constraints = add.variables(2*nt + 1, constraints, lb = c(rep(0,nt), rep(-Inf,nt+1)))
			
	#  u.j - [ SUM <over i> [ SUM <over j> r.ij ] * x.i ] - E < w.j, for each j = 1,...,T 
	a = rbind( matrix(0, n, nt), diag(nt), 1, -diag(nt))
		a[1 : n0, ] = t(apply( t(ia$hist.returns), 1, cumsum))	
	constraints = add.constraints(a, rep(0, nt), '>=', constraints)					
			
	#  [ SUM <over i> [ SUM <over j> r.ij ] * x.i ] < u.j, for each j = 1,...,T 
	a = rbind( matrix(0, n, nt), 0*diag(nt), 0, diag(nt))
		a[1 : n0, ] = -t(apply( t(ia$hist.returns), 1, cumsum))
	constraints = add.constraints(a, rep(0, nt), '>=', constraints)
		
	#  u.j-1 < u.j, for each j = 1,...,T - portfolio high water mark is increasing		
	temp = diag(nt);
		temp[-nt,-1]=-diag((nt-1))
		diag(temp) = 1			
	a = rbind( matrix(0, n, nt), 0*diag(nt), 0, temp)
		a = a[,-1]		
	constraints = add.constraints(a, rep(0, (nt-1)), '>=', constraints)

	# objective : Conditional Drawdown at Risk (CDaR)
	#  E + 1/(1-a) * 1/T * [ SUM <over j> w.j ]
	constraints = add.constraints(c(rep(0, n), (1/(1-alpha))* (1/nt) * rep(1, nt), 1, rep(0, nt)), value, type[1], constraints)	
		
	return( constraints )	
}

#' @export 
portfolio.cdar <- function
(
	weight,		# weight
	ia			# input assumptions	
)	
{
	weight = weight[, 1:ia$n, drop=F]
	if(is.null(ia$parameters.alpha)) alpha = 0.95 else alpha = ia$parameters.alpha
	
	portfolio.returns = weight %*% t(ia$hist.returns)
	# use CVaR formula
	return( apply(portfolio.returns, 1, 
			function(x) {
				x = cumsum(x)
				x = x - cummax(x)
				-compute.cvar(x, 1-alpha)
			} 
		))
			
}	

#' @export 
min.cdar.portfolio <- function
(
	ia,				# input assumptions
	constraints		# constraints
)
{
	min.portfolio(ia, constraints, add.constraint.cdar, portfolio.cdar)
}

###############################################################################
# Compute CDaR based on data
#' @export 
###############################################################################
portfolio.cdar.real <- function
(
	weight,		# weight
	ia			# input assumptions	
)	
{
	weight = weight[, 1:ia$n, drop=F]
	if(is.null(ia$parameters.alpha)) alpha = 0.95 else alpha = ia$parameters.alpha
	
	portfolio.returns = weight %*% t(ia$hist.returns)	
	out = rep(0, nrow(weight))
	
	for( i in 1:nrow(weight) ) {	
		portfolio.equity = cumprod(1 + portfolio.returns[i,])
		x = compute.drawdowns(portfolio.equity)
		
		# use CVaR formula
		out[i] = compute.cvar(x, alpha)
	}	
	
	return( out )
}	

###############################################################################
# Compute Portfolio Drawdowns
#' @export 
###############################################################################
compute.drawdowns <- function( portfolio.equity, make.plot = FALSE )	
{
	temp = portfolio.equity / cummax(portfolio.equity) - 1
	temp = c(temp, 0)
	
	drawdown.start = which( temp == 0 & mlag(temp, -1) != 0 )
	drawdown.end = which( temp == 0 & mlag(temp, 1) != 0 )
	
	if(make.plot) {
		plot((1:len(temp)), temp, type='l')
			points((1:len(temp))[drawdown.start] , temp[drawdown.start], col='red')
			points((1:len(temp))[drawdown.end] , temp[drawdown.end], col='blue')
	}
						
	return( apply(cbind(drawdown.start, drawdown.end), 1, 
					function(x){ min(temp[ x[1]:x[2] ], na.rm=T)} )
		)
}	










###############################################################################
# Find portfolio that Minimizes Average Correlation
# Rdonlp2 only works with R version before 2.9
# Rdonlp2 is not avilable for latest version of R
# for more help please visit http://arumat.net/Rdonlp2/
#
# Conditions of use:                                                        
# 1. donlp2 is under the exclusive copyright of P. Spellucci                
#    (e-mail:spellucci@mathematik.tu-darmstadt.de)                          
#    "donlp2" is a reserved name                                            
# 2. donlp2 and its constituent parts come with no warranty, whether ex-    
#    pressed or implied, that it is free of errors or suitable for any      
#    specific purpose.                                                      
#    It must not be used to solve any problem, whose incorrect solution     
#    could result in injury to a person , institution or property.          
#    It is at the users own risk to use donlp2 or parts of it and the       
#    author disclaims all liability for such use.                           
# 3. donlp2 is distributed "as is". In particular, no maintenance, support  
#    or trouble-shooting or subsequent upgrade is implied.                  
# 4. The use of donlp2 must be acknowledged, in any publication which contains                                                               
#    results obtained with it or parts of it. Citation of the authors name  
#    and netlib-source is suitable.                                         
# 5. The free use of donlp2 and parts of it is restricted for research purposes                                                               
#    commercial uses require permission and licensing from P. Spellucci.    
#' @export 
###############################################################################
min.avgcor.portfolio <- function
(
	ia,				# input assumptions
	constraints		# constraints
)
{
	cov = ia$cov[1:ia$n, 1:ia$n]
	s = sqrt(diag(cov))
	
	# avgcor
	fn <- function(x){
		sd_x = sqrt( t(x) %*% cov %*% x )
		mean( ( x %*% cov ) / ( s * sd_x ) )
	}
	
	
	x = optimize.portfolio.nlp(ia, constraints, fn)
	
	return( x )
}

#' @export 
portfolio.avgcor <- function
(
	weight,		# weight
	ia			# input assumptions
)	
{	
	weight = weight[, 1:ia$n, drop=F]
	cov = ia$cov[1:ia$n, 1:ia$n]
	s = sqrt(diag(cov))
		
	
	return( apply(weight, 1, function(x) {
								sd_x = sqrt( t(x) %*% cov %*% x )
								mean( ( x %*% cov ) / ( s * sd_x ) )
							})	)
}	

###############################################################################
# Use Correlation instead of Variance in Find Minimum Risk Portfolio
# (i.e. assume all assets have same risk = 1)
#' @export 
###############################################################################
min.cor.insteadof.cov.portfolio <- function
(
	ia,				# input assumptions
	constraints		# constraints
)
{
	if(is.null(ia$cov.temp)) ia$cov.temp = ia$cov

	sol = solve.QP.bounds(Dmat = ia$correlation, dvec = rep(0, nrow(ia$cov.temp)) , 
		Amat=constraints$A, bvec=constraints$b, constraints$meq,
		lb = constraints$lb, ub = constraints$ub)
	return( sol$solution )
}


###############################################################################
# portfolio.avgcor - average correlation
#' @export 
###############################################################################
portfolio.avgcor.real <- function
(
	weight,		# weight
	ia			# input assumptions
)	
{	
	weight = weight[, 1:ia$n, drop=F]
	
	portfolio.returns = weight %*% t(ia$hist.returns)	
	
	return( apply(portfolio.returns, 1, function(x) mean(cor(ia$hist.returns, x)) ) )
}	










###############################################################################
# Mean-Lower-Semi-Absolute Deviation (M-LSAD)
# page 6, Portfolio Optimization under Lower Partial Risk Measure by H. Konno, H. Waki and A. Yuuki
# http://www.kier.kyoto-u.ac.jp/fe-tokyo/workingpapers/AFE-KyotoU_WP01-e.html
#
# Let x.i , i= 1,...,n  be weights of instruments in the portfolio.
# Let us suppose that j = 1,...,T scenarios of returns are available 
# ( r.ij denotes return of i -th asset in the scenario j ). 
#
# Mean-Lower-Semi-Absolute Deviation (M-LSAD) function has the form 
#  1/T * [ SUM <over j> z.j ]
#  such that
#  - [ SUM <over i> (r.ij - r.i) * x.i ] <= z.j , for each j = 1,...,T 
#  z.j >= 0, for each j = 1,...,T 
#' @export 
###############################################################################
add.constraint.mad.downside <- function
(
	ia,			# input assumptions
	value,		# b value
	type = c('=', '>=', '<='),	# type of constraints
	constraints	# constraints structure
)
{
	n0 = ncol(ia$hist.returns)
	n = nrow(constraints$A)	
	nt = nrow(ia$hist.returns)

	# adjust constraints, add z.j
	constraints = add.variables(nt, constraints, lb = 0)
				
	#  - [ SUM <over i> (r.ij - r.i) * x.i ] <= z.j , for each j = 1,...,T 
	a = rbind( matrix(0, n, nt), diag(nt))
	if(is.null(ia$parameters.mar) || is.na(ia$parameters.mar)) {
		a[1 : n0, ] = t(ia$hist.returns) - repmat(colMeans(ia$hist.returns), 1, nt)
		constraints = add.constraints(a, rep(0, nt), '>=', constraints)			
	} else {
		#  MAR - [ SUM <over i> r.ij * x.i ] <= z.j , for each j = 1,...,T 
		a[1 : n0, ] = t(ia$hist.returns)
		constraints = add.constraints(a, rep(ia$parameters.mar, nt), '>=', constraints)					
	}


	# objective : Mean-Lower-Semi-Absolute Deviation (M-LSAD)
	#  1/T * [ SUM <over j> z.j ]
	constraints = add.constraints(c(rep(0, n), (1/nt) * rep(1, nt)), value, type[1], constraints)	
		
	return( constraints )	
}

#' @export 
portfolio.mad.downside <- function
(
	weight,		# weight
	ia			# input assumptions
)	
{
	weight = weight[, 1:ia$n, drop=F]
	
	portfolio.returns = weight %*% t(ia$hist.returns)
	
	if(is.null(ia$parameters.mar) || is.na(ia$parameters.mar)) {
		return( apply(portfolio.returns, 1, function(x) mean(pmax(mean(x) - x, 0)) ) )
	} else {
		return( apply(portfolio.returns, 1, function(x) mean(pmax(ia$parameters.mar - x, 0)) ) )	
	}
}	

#' @export 
min.mad.downside.portfolio <- function
(
	ia,				# input assumptions
	constraints		# constraints
)
{
	min.portfolio(ia, constraints, add.constraint.mad.downside, portfolio.mad.downside)	
}










###############################################################################
# Mean-Lower Semi-Variance (MV)
# page 6, Portfolio Optimization under Lower Partial Risk Measure by H. Konno, H. Waki and A. Yuuki
# http://www.kier.kyoto-u.ac.jp/fe-tokyo/workingpapers/AFE-KyotoU_WP01-e.html
#
# Same logic as add.constraint.mad.downside, but minimize (z.j)^2
# use quadratic solver
#' @export 
###############################################################################
portfolio.risk.downside <- function
(
	weight,		# weight
	ia			# input assumptions
)	
{
	weight = weight[, 1:ia$n, drop=F]
	
	portfolio.returns = weight %*% t(ia$hist.returns)
	
	if(is.null(ia$parameters.mar) || is.na(ia$parameters.mar)) {
		return( apply(portfolio.returns, 1, function(x) sqrt(mean(pmax(mean(x) - x, 0)^2)) ) )
	} else {
		return( apply(portfolio.returns, 1, function(x) sqrt(mean(pmax(ia$parameters.mar - x, 0)^2)) ) )	
	}
}	

#' @export 
min.risk.downside.portfolio <- function
(
	ia,				# input assumptions
	constraints		# constraints
)
{
	n = nrow(constraints$A)	
	nt = nrow(ia$hist.returns)
	
	# objective is stored as a last constraint
	constraints = add.constraint.mad.downside(ia, 0, '>=', constraints)				
	
	f.obj = constraints$A[, ncol(constraints$A)]
		constraints = delete.constraints( ncol(constraints$A), constraints)
		
	# setup Dmat
	Dmat = diag( len(f.obj) )
	diag(Dmat) = f.obj
	if(!is.positive.definite(Dmat)) {
		Dmat <- make.positive.definite(Dmat)
	}	
	
		
	# find optimal solution	
	x = NA

	binary.vec = 0
	if(!is.null(constraints$binary.index)) binary.vec = constraints$binary.index
	
	sol = try(solve.QP.bounds(Dmat = Dmat, dvec = rep(0, nrow(Dmat)) , 
		Amat=constraints$A, bvec=constraints$b, constraints$meq,
		lb = constraints$lb, ub = constraints$ub, binary.vec = binary.vec),TRUE) 
	
		
	if(!inherits(sol, 'try-error')) {
		x = sol$solution[1:n]
		
		
		# to check
		if( F ) {
			sol$solution %*% Dmat %*% (sol$solution) - portfolio.risk.downside(t(x), ia)^2
		}
	}		
	
	return( x )	
}









###############################################################################
# Gini mean difference
# The mean difference is also known as the absolute mean difference and the Gini mean difference 
# http://en.wikipedia.org/wiki/Mean_difference
#
# The Generation of Mean Gini Efficient Sets by J. Okunev (1991)
# Can be made more efficient by solving for dual
#' @export 
###############################################################################
add.constraint.gini <- function
(
	ia,			# input assumptions
	value,		# b value
	type = c('=', '>=', '<='),	# type of constraints
	constraints	# constraints structure
)
{
	n0 = ncol(ia$hist.returns)
	n = nrow(constraints$A)	
	nt = nrow(ia$hist.returns)

	# adjust constraints, add a.long, a.short 2 * nt*(nt-1)/2
	constraints = add.variables(nt*(nt-1), constraints, lb=0)
	
	# [ SUM <over i> x.i * (r.ij - r.ik) ] - a.long.jk + a.short.jk = 0
	# for each j = 1,...,T , k>j	
	a = matrix(0, n0 + nt*(nt-1), nt*(nt-1)/2)
		diag(a[(n0+1) : (n0 + nt*(nt-1)/2), ]) = -1
		diag(a[(n0+1+nt*(nt-1)/2) : (n0 + nt*(nt-1)), ]) = 1
	#a = rbind( matrix(0, n, nt*(nt-1)/2), -diag(nt*(nt-1)/2), diag(nt*(nt-1)/2))	
	#	a[1 : n0, ] = t(ia$hist.returns)	
	hist.returns = as.matrix(ia$hist.returns)
	
	i.start = 0
	for(t in 1:(nt-1)) {
		index = (i.start+1) : (i.start + nt -t)
		for(i in 1:n0) {
			a[i, index] = ( hist.returns[t,i] - hist.returns[,i] ) [ (t+1) : nt ] 
		}
		i.start = i.start + nt -t
		
	}
	
	constraints = add.constraints(a, 0, '=', constraints)

	# objective : maximum loss, w
	constraints = add.constraints(c(rep(0, n), rep(1, nt*(nt-1))), value, type[1], constraints)	
		
	return( constraints )	
}

#' @export 
min.gini.portfolio <- function
(
	ia,				# input assumptions
	constraints		# constraints
)
{
	min.portfolio(ia, constraints, add.constraint.gini, portfolio.gini.coefficient)
	
}


#' @export 
portfolio.gini.coefficient <- function
(
	weight,		# weight
	ia			# input assumptions
)	
{
	if(is.null(dim(weight))) dim(weight) = c(1, len(weight))
	
	weight = weight[, 1:ia$n, drop=F]
	
	portfolio.returns = weight %*% t(ia$hist.returns)
	
	n = ncol(portfolio.returns)
	

#portfolio.returns = rnorm(100)
#n = len(portfolio.returns)
#
# http://en.wikipedia.org/wiki/Mean_difference
#sum(outer(portfolio.returns, portfolio.returns, function(x,y) abs(x-y))) / (n*(n-1))
#
# unmht:///file.5/C:/Desktop/1save/blog_entries/ERC/Gini%20Coefficient%20--%20from%20Wolfram%20MathWorld.mht/
#temp = sort(portfolio.returns, decreasing = F)
#2* sum( (2*(1:n) - n - 1) * temp ) / (n*(n-1) )	



	one.to.n = 1:n
	out = weight[,1] * NA
	out[] = apply( portfolio.returns, 1, function(x) {
		temp = sort(x, decreasing = F)
		sum( (2*one.to.n - n - 1) * temp )
		} )
	out = 2 * out /(n*(n-1))
	return(out)
}	






###############################################################################
# Solve LP Portfolio Problem 
#' @export 
###############################################################################
lp.obj.portfolio <- function
(
	ia,				# input assumptions
	constraints,	# constraints
	f.obj = c(ia$expected.return, rep(0, nrow(constraints$A) - ia$n)),
	direction = 'min'
)
{
	x = NA

	binary.vec = 0
	if(!is.null(constraints$binary.index)) binary.vec = constraints$binary.index
	
	sol = try(solve.LP.bounds(direction, f.obj,
		t(constraints$A), 
		c(rep('=', constraints$meq), rep('>=', len(constraints$b) - constraints$meq)), 
		constraints$b, lb = constraints$lb, ub = constraints$ub, binary.vec = binary.vec), TRUE)	
	
	if(!inherits(sol, 'try-error')) {
		x = sol$solution
	}			
	
	return( x )
}

###############################################################################
# Find Maximum Return Portfolio
###############################################################################
# maximize     C x
# subject to   A x <= B
#' @export 
###############################################################################
max.return.portfolio <- function
(
	ia,				# input assumptions
	constraints		# constraints
)
{
	lp.obj.portfolio(ia, constraints, direction = 'max')
}

###############################################################################
# portfolio.return - weight * expected.return
#' @export 
###############################################################################
portfolio.return <- function
(
	weight,		# weight
	ia			# input assumptions
)	
{
	if(is.null(dim(weight))) dim(weight) = c(1, len(weight))
	
	weight = weight[, 1:ia$n, drop=F]
	portfolio.return = weight %*% ia$expected.return
	return( portfolio.return )
}	

###############################################################################
# portfolio.geometric.return
#' @export 
###############################################################################
portfolio.geometric.return <- function
(
	weight,		# weight
	ia			# input assumptions
)	
{
	weight = weight[, 1:ia$n, drop=F]
	
	portfolio.returns = weight %*% t(ia$hist.returns)
	return( apply(portfolio.returns, 1, function(x) (prod(1+x)^(1/len(x)))^ia$annual.factor - 1 ) )
}	


###############################################################################
# Find Maximum Geometric Return Portfolio
#' @export 
###############################################################################
max.geometric.return.portfolio <- function
(
	ia,				# input assumptions
	constraints,	# constraints
	min.risk,
	max.risk
)
{
	# Geometric return
	fn <- function(x){
		portfolio.returns = x %*% t(ia$hist.returns)	
		prod(1 + portfolio.returns)
	}

	# Nonlinear constraints
	nlcon1 <- function(x){
		sqrt(t(x) %*% ia$cov %*% x)
	}
	
	nl.constraints = list()
		nl.constraints$constraints = list(nlcon1)
		nl.constraints$upper = c(max.risk)
		nl.constraints$lower = c(min.risk)
	
	x = optimize.portfolio.nlp(ia, constraints, fn, nl.constraints, direction = 'max')
	
	return( x )
}

###############################################################################
# portfolio.unrebalanced.return
# http://www.effisols.com/mvoplus/sample1.htm
#' @export 
###############################################################################
portfolio.unrebalanced.return <- function
(
	weight,		# weight
	ia			# input assumptions
)	
{
	weight = weight[, 1:ia$n, drop=F]
	
	total.return = apply(1+ia$hist.returns,2,prod)
	total.portfolio.return = weight %*% total.return / rowSums(weight)
	total.portfolio.return = (total.portfolio.return^(1/nrow(ia$hist.returns)))^ia$annual.factor - 1 
	return( total.portfolio.return )
}	










###############################################################################
# Functions to convert between Arithmetic and Geometric means
###############################################################################
# page 8, DIVERSIFICATION, REBALANCING, AND THE GEOMETRIC MEAN FRONTIER by W. Bernstein and D. Wilkinson (1997)
#' @export 
###############################################################################
geom2aritm <- function(G, V, a, b) 
{ 
	(2*G + a*V^2) / (1 - b*G + sqrt((1+b*G)^2 + 2*a*b*V^2)) 
}

#' @export 
aritm2geom <- function(R, V, a, b) 
{ 
	R - a*V^2 / (2*(1 + b*R)) 
}

###############################################################################
# page 14, A4, On the Relationship between Arithmetic and Geometric Returns by D. Mindlin
#' @export 
###############################################################################
geom2aritm4 <- function(G, V) 
{ 
	(1+G)*sqrt(1/2 + 1/2*sqrt(1 + 4*V^2/(1+G)^2)) - 1 
}

#' @export 
aritm2geom4 <- function(R, V) 
{ 
	(1+R)/(sqrt(1 + V^2/(1+R)^2)) - 1 
}


###############################################################################
# Find Portfolio with Minimum Risk and given Target Return
#' @export 
###############################################################################
target.return.portfolio.helper <- function
(
	ia,				# input assumptions
	constraints,	# constraints
	target.return
)
{
	constraints.target = add.constraints(ia$expected.return, type='>=', b=target.return, constraints)							
	sol = try(min.var.portfolio(ia, constraints.target), silent = TRUE)
	
	if(inherits(sol, 'try-error'))
		sol = max.return.portfolio(ia, constraints)

	sol
}

#' @export 	
target.return.portfolio <- function
(
	target.return,
	annual.factor = 252
)
{
	target.return = as.double(target.return[1])
	if(target.return > 1) target.return = target.return / 100
	target.return = target.return / annual.factor

	function
	(
		ia,			# input assumptions
		constraints	# constraints
	)
	{
		target.return.portfolio.helper(ia, constraints, target.return)
	}	
}

###############################################################################
# Find Portfolio with Minimum Risk and given Target Risk
#' @export 
###############################################################################
target.risk.portfolio.helper <- function
(
	ia,				# input assumptions
	constraints,	# constraints
	target.risk,
	silent = T,
	min.w = NA,
	max.w = NA
)
{
	if( is.na(max.w) ) max.w = max.return.portfolio(ia, constraints)
	if( is.na(min.w) ) min.w = min.var.portfolio(ia, constraints)	
	
	max.r = portfolio.return(max.w, ia)
	min.r = portfolio.return(min.w, ia)
	
	max.s = portfolio.risk(max.w, ia)
	min.s = portfolio.risk(min.w, ia)
	
	if( target.risk >= min.s & target.risk <= max.s ) {
		# function to compute risk given return x
		f <- function (x, ia, constraints, target.risk) {
			portfolio.risk(target.return.portfolio.helper(ia, constraints, x), ia) - target.risk
		}
		
		f.lower = min.s - target.risk
		f.upper = max.s - target.risk
				
		sol = uniroot(f, c(min.r, max.r), f.lower=f.lower, f.upper=f.upper, tol = 0.0001, 
			ia=ia, constraints=constraints, target.risk=target.risk)
		if(!silent) cat('Found solution in', sol$iter, 'itterations', '\n')
		return( target.return.portfolio.helper(ia, constraints, sol$root) )
	} else if( target.risk < min.s ) {
		return( min.w )
	} else {
		return( max.w )
	}
	
	stop(paste('target.risk =', target.risk, 'is not possible, max risk =', max.s, ', min risk =', min.s))
}

#' @export 	
target.risk.portfolio <- function
(
	target.risk,
	annual.factor = 252
)
{
	target.risk = as.double(target.risk[1])
	if(target.risk > 1) target.risk = target.risk / 100
	target.risk = target.risk / sqrt(annual.factor)

	function
	(
		ia,			# input assumptions
		constraints	# constraints
	)
	{
		target.risk.portfolio.helper(ia, constraints, target.risk)
	}	
}



###############################################################################
# Find Minimum Risk Portfolio
###############################################################################
# solve.QP function from quadprog library
# min(-d^T w.i + 1/2 w.i^T D w.i) constraints A^T w.i >= b_0
#' @export 
###############################################################################
min.risk.portfolio <- function
(
	ia,				# input assumptions
	constraints		# constraints
)
{
	x = NA			

	binary.vec = 0
	if(!is.null(constraints$binary.index)) binary.vec = constraints$binary.index
	
	if(is.null(ia$cov.temp)) ia$cov.temp = ia$cov
		
	
	sol = try(solve.QP.bounds(Dmat = ia$cov.temp, dvec = rep(0, nrow(ia$cov.temp)) , 
		Amat=constraints$A, bvec=constraints$b, constraints$meq,
		lb = constraints$lb, ub = constraints$ub, binary.vec = binary.vec),TRUE) 
	
	if(!inherits(sol, 'try-error')) {
		if(binary.vec[1] != 0) cat(sol$counter,'QP calls made to solve problem with', len(constraints$binary.index), 'binary variables using Branch&Bound', '\n')
	
		x = sol$solution;
	}		
		
	return( x )
}

###############################################################################
# portfolio.risk - square root of portfolio volatility
#' @export 
###############################################################################
portfolio.risk <- function
(
	weight,		# weight
	ia			# input assumptions
)	
{	
	if(is.null(dim(weight))) dim(weight) = c(1, len(weight))

	weight = weight[, 1:ia$n, drop=F]
	cov = ia$cov[1:ia$n, 1:ia$n]
	
	return( apply(weight, 1, function(x) sqrt(t(x) %*% cov %*% x)) )	
}	



	
###############################################################################
# Find Equal-Risk-Contribution (ERC) Portfolio
#
# Unproxying weight constraints by Pat Burns
# http://www.portfolioprobe.com/2011/04/13/unproxying-weight-constraints/
#
# Analytical Solution for the Equal-Risk-Contribution Portfolio
# http://www.wilmott.com/messageview.cfm?catid=34&amp;threadid=38497
#
# Equally-weighted risk contributions: a new method to build risk balanced diversified portfolios by S. Maillard, T. Roncalli and J. Teiletche (2008)
# http://www.thierry-roncalli.com/download/erc-slides.pdf
#
# On the property of equally-weighted risk contributions portfolios by S. Maillard, T. Roncalli and J. Teiletche (2008)
# http://www.thierry-roncalli.com/download/erc.pdf
#
# Matlab code for Equal Risk Contribution Portfolio by Farid Moussaoui
# http://mfquant.net/erc_portfolio.html
#' @export 
###############################################################################
find.erc.portfolio <- function
(
	ia,				# input assumptions
	constraints		# constraints
)
{
	cov = ia$cov[1:ia$n, 1:ia$n]
	
	# obj
	fn <- function(x){
		risk.contribution = (x * (cov %*% x))
		sum( abs(risk.contribution - mean(risk.contribution)) )
	}
		
	x = optimize.portfolio.nlp(ia, constraints, fn)
	
	return( x )
}	
	
#' @export 	
find.erc.portfolio.simple <- function
(
	ia,				# input assumptions
	constraints		# constraints
)
{
	cov = ia$cov[1:ia$n, 1:ia$n]
	
	# obj
	fn <- function(x){
		# sum(x) = 1
		if (sum(x) == 0) x = x + 1e-2
		x  = x / sum(x)
	
		risk.contribution = (x * (cov %*% x))
		var(as.double(risk.contribution))
	}
		

	x0 = 1/sqrt(diag(cov))
		x0 = x0 / sum(x0)
					
	x = nlminb(start = x0, objective = fn, lower = constraints$lb, upper = constraints$ub)
	
	x$par = x$par / sum(x$par)
	return(x$par)
}	

			

find.erc.portfolio.test <- function() {
	#--------------------------------------------------------------------------
	# Create Efficient Frontier
	#--------------------------------------------------------------------------
	ia = aa.test.create.ia.rebal()				
	n = ia$n		

	
	#--------------------------------------------------------------------------
	# Construct ERC Equal-Risk Portfolio	
	#--------------------------------------------------------------------------
	x0 = 1/sqrt(diag(ia$cov))
		temp = x0 / sum(x0)
	rc.temp = portfolio.risk.contribution(temp, ia)		
		rc.temp = abs(as.vector(rc.temp))
	plot(rc.temp,ylim=c(0,0.4))
	 
	diff(range(rc.temp))
	sd(rc.temp)
 
	
	
	
	#--------------------------------------------------------------------------
	# Create constraints
	#--------------------------------------------------------------------------
	# 0 <= x.i <= 1
	constraints = new.constraints(n, lb = 0, ub = 1)
		
	# SUM x.i = 1
	constraints = add.constraints(rep(1, n), 1, type = '=', constraints)		
	
	#--------------------------------------------------------------------------
	# Construct ERC Equal-Risk-Contribution Portfolio	
	#--------------------------------------------------------------------------
	temp = find.erc.portfolio(ia, constraints)
	rc.temp = portfolio.risk.contribution(temp, ia)		
		rc.temp = abs(as.vector(rc.temp))
	plot(rc.temp,ylim=c(0,0.4))
	 
	diff(range(rc.temp))
	sd(rc.temp)
 
	
	#--------------------------------------------------------------------------
	# Construct ERC Equal-Risk-Contribution Portfolio	
	#--------------------------------------------------------------------------
	temp = find.erc.portfolio.simple(ia, constraints)
		temp = temp / sum(temp)
	rc.temp = portfolio.risk.contribution(temp, ia)		
		rc.temp = abs(as.vector(rc.temp))
	plot(rc.temp,ylim=c(0,0.4))
	 
	diff(range(rc.temp))
	sd(rc.temp)
	
	#--------------------------------------------------------------------------
	# Construct ERC Equal-Risk-Contribution Portfolio	
	#--------------------------------------------------------------------------
	temp = equal.risk.contribution.portfolio(ia, constraints)
		temp = temp / sum(temp)
	rc.temp = portfolio.risk.contribution(temp, ia)		
		rc.temp = abs(as.vector(rc.temp))
	plot(rc.temp,ylim=c(0,0.4))
	 
	diff(range(rc.temp))
	sd(rc.temp)
	
}		


###############################################################################
# portfolio.risk.contribution - (w * V %*% w) / (w %*% V %*% w)
# Unproxying weight constraints by Pat Burns
# http://www.portfolioprobe.com/2011/04/13/unproxying-weight-constraints/
#' @export 
###############################################################################
portfolio.risk.contribution <- function
(
	weight,		# weight
	ia			# input assumptions
)	
{	
	if(is.null(dim(weight))) dim(weight) = c(1, len(weight))

	weight = weight[, 1:ia$n, drop=F]
	cov = ia$cov[1:ia$n, 1:ia$n]
	
	out = weight
	out[] = t(apply( weight, 1, function(x) (x * (cov %*% x)) / (t(x) %*% cov %*% x)[1] ))
	return(out)	
}	



	








###############################################################################
# Create efficient frontier
#' @export 
###############################################################################
portopt <- function
(
	ia,						# Input Assumptions
	constraints = NULL,		# Constraints
	nportfolios = 50,		# Number of portfolios
	name = 'Risk',			# Name
	min.risk.fn = min.risk.portfolio,	# Risk Measure
	equally.spaced.risk = F	# Add extra portfolios so that portfolios on efficient frontier 
							# are equally spaced on risk axis
)
{
	# load / check required packages
	load.packages('quadprog,corpcor,lpSolve,kernlab')
	
	# set up constraints
	if( is.null(constraints) ) {
		constraints = new.constraints(rep(0, ia$n), 0, type = '>=')
	} 	
		
	# set up solve.QP
	ia$risk = iif(ia$risk == 0, 0.000001, ia$risk)
	if( is.null(ia$cov) ) ia$cov = ia$correlation * (ia$risk %*% t(ia$risk))		
	
	# setup covariance matrix used in solve.QP
	ia$cov.temp = ia$cov

	# check if there are dummy variables
	n0 = ia$n
	n = nrow(constraints$A)		
	
	if( n != nrow(ia$cov.temp) ) {
		temp =  matrix(0, n, n)
		temp[1:n0, 1:n0] = ia$cov.temp[1:n0, 1:n0]
		ia$cov.temp = temp
	}
				
	if(!is.positive.definite(ia$cov.temp, method = 'chol')) {
		ia$cov.temp <- make.positive.definite(ia$cov.temp, 0.000000001)
	}	
	

	
	# set up output 
	if(nportfolios<2) nportfolios = 2
	out = list(weight = matrix(NA, nportfolios, nrow(constraints$A)))
		colnames(out$weight) = rep('', ncol(out$weight))
		colnames(out$weight)[1:ia$n] = ia$symbols
		
			
	# find maximum return portfolio	
	out$weight[nportfolios, ] = max.return.portfolio(ia, constraints)

	# find minimum risk portfolio
	out$weight[1, ] = match.fun(min.risk.fn)(ia, constraints)	
		constraints$x0 = out$weight[1, ]
	
	if(nportfolios > 2) {
		# find points on efficient frontier
		out$return = portfolio.return(out$weight, ia)
		target = seq(out$return[1], out$return[nportfolios], length.out = nportfolios)
	
		constraints = add.constraints(c(ia$expected.return, rep(0, nrow(constraints$A) - ia$n)), 
							target[1], type = '>=', constraints)
										
		for(i in 2:(nportfolios - 1) ) {
			constraints$b[ len(constraints$b) ] = target[i]
			out$weight[i, ] = match.fun(min.risk.fn)(ia, constraints)
				constraints$x0 = out$weight[i, ]
				
#cat(i, '\n')
		
		}
		
		if( equally.spaced.risk ) {
			out$risk = portfolio.risk(out$weight, ia)
		
			temp = diff(out$risk)
			index = which(temp >= median(temp) + mad(temp))
			
			if( len(index) > 0 ) {
				index = min(index)

				proper.spacing = ceiling((out$risk[nportfolios] - out$risk[index])/temp[(index-1)])-1
				nportfolios1 = proper.spacing + 2
								
				if(nportfolios1 > 2) {
					out$return = portfolio.return(out$weight, ia)
					out$risk = portfolio.risk(out$weight, ia)
					temp = spline(out$risk, out$return, n = nportfolios, method = 'natural')
										
					target = temp$y[ which(temp$y > out$return[index] & temp$y < out$return[nportfolios] & 
						temp$x > out$risk[index] & temp$x < out$risk[nportfolios])]
					target = c(out$return[index], target, out$return[nportfolios])
					nportfolios1 = len(target)
									
					out1 = list(weight = matrix(NA, nportfolios1, nrow(constraints$A)))
						out1$weight[1, ] = out$weight[index, ]
						out1$weight[nportfolios1, ] = out$weight[nportfolios, ]
					
					constraints$x0 = out1$weight[1, ]					
					for(i in 2:(nportfolios1 - 1) ) {						
						constraints$b[ len(constraints$b) ] = target[i]
						out1$weight[i, ] = match.fun(min.risk.fn)(ia, constraints)				
							constraints$x0 = out1$weight[i, ]
					}
					
					out$weight = rbind(out$weight[-c(index:nportfolios),], out1$weight)
				}
				
				
			}
			
		}
	}

	# remove empty solutions	
	rm.index = is.na(rowSums(out$weight))
	if(any(rm.index)) out$weight = out$weight[!rm.index,]
	
	# compute risk / return
	out$return = portfolio.return(out$weight, ia)
	out$risk = portfolio.risk(out$weight, ia)
	out$name = name
	
	
	return(out)			
}






###############################################################################
# Visualize input assumptions
#' @export 
###############################################################################
plot.ia <- function
(
	ia,				# input assumptions
	layout = NULL	# flag to idicate if layout is already set
)
{
	# create a table with summary statistics
	if( is.null(layout) ) layout(matrix(1:2, nr=1))	
	temp = cbind(ia$expected.return, ia$risk)
		temp[] = plota.format(100 * temp[], 1, '', '%')
		colnames(temp) = spl('Return,Risk')
	plot.table(temp, 'Symbol')
	
	# visualize correlation  matrix
	temp = ia$correlation
		temp[lower.tri(temp, TRUE)] = NA
		temp = temp[-ia$n, -1]
		temp[] = plota.format(100 * temp[], 1, '', '%')			
	plot.table(temp, highlight = TRUE, colorbar = TRUE)	
}

###############################################################################
# Plot efficient fontier(s) and transitopn map
#' @export 
###############################################################################
plot.ef <- function
(
	ia,						# input assumption
	efs,					# efficient fontier(s)
	portfolio.risk.fn = portfolio.risk,	# risk measure
	transition.map = TRUE,	# flag to plot transitopn map
	layout = NULL			# flag to idicate if layout is already set
)
{
	# extract name of risk measure
	risk.label = as.character(substitute(portfolio.risk.fn))

	# prepare plot data
	n = ia$n
	x = match.fun(portfolio.risk.fn)(diag(n), ia)
	y = ia$expected.return
	
	# prepare plot ranges
	xlim = range(c(0, x, 
			max( sapply(efs, function(x) max(match.fun(portfolio.risk.fn)(x$weight,ia))) )
			), na.rm = T)

	ylim = range(c(0, y, 
			min( sapply(efs, function(x) min(portfolio.return(x$weight,ia))) ),
			max( sapply(efs, function(x) max(portfolio.return(x$weight,ia))) )
			), na.rm = T)

	# convert x and y to percentages
	x = 100 * x
	y = 100 * y
	xlim = 100 * xlim
	ylim = 100 * ylim			
				
	# plot
	if( !transition.map ) layout = T
	if( is.null(layout) ) layout(1:2)
	
	par(mar = c(4,3,2,1), cex = 0.8)
	plot(x, y, xlim = xlim, ylim = ylim,
		xlab='', ylab='', main=paste(risk.label, 'vs Return'), col='black')
		mtext('Return', side = 2,line = 2, cex = par('cex'))
		mtext(risk.label, side = 1,line = 2, cex = par('cex'))		
	grid();
	text(x, y, ia$symbols,	col = 'blue', adj = c(1,1), cex = 0.8)

	# plot fontiers
	for(i in len(efs):1) {
		ef = efs[[ i ]]
		
		x = 100 * match.fun(portfolio.risk.fn)(ef$weight, ia)		
		y = 100 * ef$return
		
		lines(x, y, col=i)
	}	
	plota.legend(sapply(efs, function(x) x$name), 1:len(efs))
	
	
	# Transition Map plot
	if(transition.map) {
		plot.transition.map(efs[[i]]$weight, x, risk.label, efs[[i]]$name)
	}
}

# Add portfolios to plot
#' @export 
plot.add.portfolios = function(ia, portfolio.risk.fn = portfolio.risk, ...) {
	portfolios = lst(...)
	
	col = plota.colors(portfolios)
	
	for(i in 1:len(portfolios))
		points(100 * portfolio.risk.fn(portfolios[[i]],ia), 100 * portfolio.return(portfolios[[i]],ia), pch=15, col=col[i])
	
	plota.legend(names(portfolios), col, x='bottomright')
}


###############################################################################
# Plot Transition Map
#' @export 
###############################################################################
plot.transitopn.map <- function(x,y,xlab = 'Risk',name = '',type=c('s','l')) {
	plot.transition.map(x,y,xlab,name,type)
}

#' @export 
plot.transition.map <- function
(
	y,				# weights
	x,				# x data
	xlab = 'Risk',	# x label
	name = '',		# name
	type=c('s','l'),# type
	col = NA		# colors

)
{
	if( is.list(y) ) {
		name = y$name
		x = 100 * y$risk
		y = y$weight
	}
		
	y[is.na(y)] = 0	
		
	par(mar = c(4,3,2,1), cex = 0.8)
	plota.stacked(x, y, xlab = xlab, main = paste('Transition Map for', name),
		type=type[1], col=ifna(col, plota.colors(ncol(y))) )
}





###############################################################################
# Helper functions from the following paper:
# On the property of equally-weighted risk contributions portfolios by S. Maillard, 
# T. Roncalli and J. Teiletche (2008), page 22
# http://www.thierry-roncalli.com/download/erc.pdf
#' @export 
###############################################################################
portfolio.turnover <- function
(
	weight		# weight
)	
{	
	if(is.null(dim(weight))) dim(weight) = c(1, len(weight))

	out = weight[,1] * NA
	out[] = rowSums( abs(weight - mlag(weight)) ) / 2
	return(out)
}	

# Herfindahl Index of portfolio weights
# http://en.wikipedia.org/wiki/Herfindahl_index
#' @export 
portfolio.concentration.herfindahl.index <- function
(
	weight		# weight
)	
{	
	if(is.null(dim(weight))) dim(weight) = c(1, len(weight))

	one.over.n = 1/ rowSums(!is.na(weight))
	out = weight[,1] * NA
	out[] = (rowSums(weight^2, na.rm=T) - one.over.n) / (1 - one.over.n)
	return(out)
	
	
	one.over.n = 1/ncol(weight)
	out = weight[,1] * NA
	out[] = (rowSums(weight^2) - one.over.n) / (1 - one.over.n)
	return(out)
}	

# Gini Coefficient of portfolio weights
# Gini is designed to work with positive numbers!!!
# http://en.wikipedia.org/wiki/Gini_coefficient
# http://en.wikipedia.org/wiki/Mean_difference
#' @export 
portfolio.concentration.gini.coefficient <- function
(
	weight		# weight
)	
{	
	if(is.null(dim(weight))) dim(weight) = c(1, len(weight))

	n = ncol(weight)

	
	# The mean difference formula
	# weight = rep(1/n,n)	
	# 1/2 * (sum(outer(weight, weight, function(x,y) abs(x-y))) / (n*(n-1))) / sum(abs(weight)/n)
		
	# Gini formula by Angus Deaton, more efficient 
	#(n+1)/(n-1) - 2 * sum(weight * rank(-weight)) /(n*(n-1)* mean(weight))
	#(n+1)/(n-1) - 2 * sum(abs(weight) * rank(-abs(weight))) /(n*(n-1)* sum(abs(weight)/n))

	# same formula, but faster
	#temp = sort(weight, decreasing = T)
	#(n+1)/(n-1) - 2 * sum(temp * (1:n)) /(n*(n-1)* mean(temp))

	one.to.n = 1:n	
	out = weight[,1] * NA
	
	
	for(i in 1:nrow(weight)) {
		x = coredata(weight[i,])
		index = !is.na(x)
		n1 = sum(index)
		if( n1 > 0 ) {
			temp = sort(x[index], decreasing = T)			
			
			# make sure all weights are positive
			if(temp[n1] < 0) temp = temp - temp[n1]
			
			out[i] = (n1+1)/(n1-1) - 2 * sum(temp * one.to.n[1:n1]) /(n1*(n1-1)* sum(temp) / n1)
		}
	}
	return(out)

		
	out[] = apply( weight, 1, function(x) {
		temp = sort(x, decreasing = T)
		sum(temp * one.to.n)
		} )
	out = (n+1)/(n-1) - 2 * out /(n*(n-1)* apply(weight, 1, mean))
	return(out)
}	

