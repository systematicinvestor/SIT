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
###############################################################################


###############################################################################
# Building constraints for quadprog, solve.QP
# min(-d^T w.i + 1/2 w.i^T D w.i) constraints A^T w.i >= b_0
#  the first meq constraints are treated as equality constraints, 
#  all further as inequality constraints
###############################################################################
# new.constraints - create new constraints structure
###############################################################################
new.constraints <- function
(
	A,			# matrix with constraints 
	b,			# vector b
	type = c('=', '>=', '<=')	# type of constraints
)
{
	meq = 0
	if ( is.null(dim(A)) ) dim(A) = c(len(A), 1)
	
	if ( type[1] == '=' ) meq = len(b)
	if ( type[1] == '<=' ) {
		A = -A
		b = -b
	}
	
	return( list(A = A, b = b, meq = meq) )
}

###############################################################################
# add.constraints - add to existing constraints structure
###############################################################################
add.constraints <- function
(
	A,			# matrix with constraints 
	b,			# vector b
	type = c('=', '>=', '<='),	# type of constraints
	constraints	# constraints structure
)
{
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
# Functions to determine portfolio returns and risks
###############################################################################
# portfolio.return - weight * expected.return
###############################################################################
portfolio.return <- function
(
	weight,		# weight
	ia			# input assumptions
)	
{
	weight = weight[, 1:ia$n]
	portfolio.return = weight %*% ia$expected.return
	return( portfolio.return )
}	

###############################################################################
# portfolio.risk - square root of portfolio volatility
###############################################################################
portfolio.risk <- function
(
	weight,		# weight
	ia			# input assumptions
)	
{	
	weight = weight[, 1:ia$n]
	cov = ia$cov[1:ia$n, 1:ia$n]
	
	portfolio.risk = diag(weight %*% cov %*% t(weight))
	portfolio.risk[ !is.na(portfolio.risk) ] = sqrt( portfolio.risk[ !is.na(portfolio.risk) ] )
	return( portfolio.risk )
}	

###############################################################################
# portfolio.maxloss - maximum historical loss at any period
###############################################################################
portfolio.maxloss <- function
(
	weight,		# weight
	ia			# input assumptions
)	
{
	weight = weight[, 1:ia$n]
	
	portfolio.returns = weight %*% t(ia$hist.returns)
	return( -apply(portfolio.returns, 1, min) )
}	

###############################################################################
# portfolio.mad - Mean-Absolute Deviation (MAD)
###############################################################################
portfolio.mad <- function
(
	weight,		# weight
	ia			# input assumptions
)	
{
	weight = weight[, 1:ia$n]
	
	portfolio.returns = weight %*% t(ia$hist.returns)
	return( apply(portfolio.returns, 1, function(x) mean(abs(x - mean(x))) ) )
}	


	
###############################################################################
# Find Maximum Return Portfolio
###############################################################################
# maximize     C x
# subject to   A x <= B
###############################################################################
max.return.portfolio <- function
(
	ia,				# input assumptions
	constraints		# constraints
)
{
	x = NA

	binary.vec = 0
	if(!is.null(constraints$binary.index)) binary.vec = constraints$binary.index
	
	sol = try(lp.anyx('max', c(ia$expected.return, rep(0, nrow(constraints$A) - ia$n)),
		t(constraints$A), 
		c(rep('=', constraints$meq), rep('>=', len(constraints$b) - constraints$meq)), 
		constraints$b, min.x.bounds = -100, binary.vec = binary.vec), TRUE)	
	
		
	if(!inherits(sol, 'try-error')) {
		x = sol$solution
	}			
	
	return( x )
}

###############################################################################
# Find Minimum Risk Portfolio
###############################################################################
# solve.QP function from quadprog library
# min(-d^T w.i + 1/2 w.i^T D w.i) constraints A^T w.i >= b_0
###############################################################################
min.risk.portfolio <- function
(
	ia,				# input assumptions
	constraints		# constraints
)
{
	x = NA			

	# check for binary variables
	if( is.null(constraints$binary.index) ) {	
		sol = try(solve.QP(Dmat = ia$cov.temp, dvec = rep(0, nrow(ia$cov.temp)) , 
			Amat=constraints$A, bvec=constraints$b, constraints$meq),TRUE) 
		
		if(!inherits(sol, 'try-error')) {
			x = sol$solution;
		}		

	} else {
		# use Binary Branch and Bound
		sol = solve.QP.binary.branch.bound(Dmat = ia$cov.temp, dvec = rep(0, nrow(ia$cov.temp)), 
							Amat=constraints$A, bvec=constraints$b, meq=constraints$meq,
							binary.vec = constraints$binary.index)
				
		cat(sol$counter,'QP calls made to solve problem with', len(constraints$binary.index), 'binary variables using Branch&Bound', '\n')
		
		x = sol$xmin
	}
		
	return( x )
}

###############################################################################
# Find portfolio that Minimizes Maximum Loss
# page 34, Comparative Analysis of Linear Portfolio Rebalancing Strategies by Krokhmal, Uryasev, Zrazhevsky  
#
# Let x.i , i= 1,...,n  be weights of instruments in the portfolio.
# Let us suppose that j = 1,...,T scenarios of returns are available 
# ( r.ij denotes return of i -th asset in the scenario j ). 
#
# The Maximum Loss (MaxLoss) function has the form 
#  MAX <over j> [ -SUM <over i> r.ij * x.i ] < w
###############################################################################
min.maxloss.portfolio <- function
(
	ia,				# input assumptions
	constraints		# constraints
)
{
	n0 = ia$n
	n = nrow(constraints$A)	
	nt = nrow(ia$hist.returns)
	
	# objective : maximum loss, w
	f.obj = c( rep(0, n), 1)
	
	# adjust prior constraints, add w
	f.con = rbind(constraints$A, 0)
	f.dir = c(rep('=', constraints$meq), rep('>=', len(constraints$b) - constraints$meq))
	f.rhs = constraints$b
	
		
	# -SUM <over i> r.ij * x.i <= w, for each j from 1 ... T
	a1 = rbind( matrix(0, n, nt), 0)
	b1 = rep(0, nt)
		a1[1:n0,] = t(ia$hist.returns)
		a1[(n + 1),] = +1		# w
		
	f.con = cbind( f.con, a1 )
	f.dir = c(f.dir, rep('>=', nt))
	f.rhs = c(f.rhs, b1)
	

	# find optimal solution	
	x = NA
	
	binary.vec = 0
	if(!is.null(constraints$binary.index)) binary.vec = constraints$binary.index
		
	sol = try(lp.anyx('min', f.obj, t(f.con), f.dir, f.rhs, -100, binary.vec), TRUE)	
	
	if(!inherits(sol, 'try-error')) {
		x = sol$solution[1:n]
		
		# to check
		if( F ) {
			v = sol$solution[(n + 1)]
			v - portfolio.maxloss(x, ia)
		}
	}		
	
	return( x )
}	



###############################################################################
# Find portfolio that Minimizes Mean-Absolute Deviation (MAD)
# page 33, Comparative Analysis of Linear Portfolio Rebalancing Strategies by Krokhmal, Uryasev, Zrazhevsky  
#
# Let x.i , i= 1,...,n  be weights of instruments in the portfolio.
# Let us suppose that j = 1,...,T scenarios of returns are available 
# ( r.ij denotes return of i -th asset in the scenario j ). 
#
# The Mean-Absolute Deviation (MAD) function has the form 
#  1/T * [ SUM <over j> (u+.j + u-.j) ] < w
#  [ SUM <over i> r.ij * x.i ] - 1/T * [ SUM <over j> [ SUM <over i> r.ij * x.i ] ] = u+.j - u-.j , for each j = 1,...,T 
#  u+.j, u-.j >= 0, for each j = 1,...,T 
###############################################################################
min.mad.portfolio <- function
(
	ia,				# input assumptions
	constraints		# constraints
)
{
	n0 = ia$n
	n = nrow(constraints$A)		
	nt = nrow(ia$hist.returns)

	# objective : Mean-Absolute Deviation (MAD)
	# 1/T * [ SUM <over j> (u+.j + u-.j) ]
	f.obj = c( rep(0, n), (1/nt) * rep(1, 2 * nt) )
	
	# adjust prior constraints, add u+.j, u-.j
	f.con = rbind( constraints$A, matrix(0, 2 * nt, ncol(constraints$A) ) )
	f.dir = c(rep('=', constraints$meq), rep('>=', len(constraints$b) - constraints$meq))
	f.rhs = constraints$b
	
	# [ SUM <over i> r.ij * x.i ] - 1/T * [ SUM <over j> [ SUM <over i> r.ij * x.i ] ] = u+.j - u-.j , for each j = 1,...,T 
	a1 = rbind( matrix(0, n, nt), -diag(nt), diag(nt))
	b1 = rep(0, nt)
		a1[1:n0,] = t(ia$hist.returns) - repmat(colMeans(ia$hist.returns), 1, nt)
		
	f.con = cbind( f.con, a1 )
	f.dir = c(f.dir, rep('=', nt))
	f.rhs = c(f.rhs, b1)
		
	# find optimal solution	
	x = NA
	
	binary.vec = 0
	if(!is.null(constraints$binary.index)) binary.vec = constraints$binary.index	
	
	min.x.bounds = c( rep(-100, n), rep(0, 2 * nt) ) 	
	sol = try(lp.anyx('min', f.obj, t(f.con), f.dir, f.rhs, min.x.bounds, binary.vec), TRUE)	
	
	if(!inherits(sol, 'try-error')) {
		x = sol$solution[1:n]
		
		# to check
		if( F ) {
			u1 = sol$solution[(n+1):(n+nt)]
			u2 = sol$solution[(n+nt+1):(n+2*nt)]
			mean(u1 + u2) - portfolio.mad(x, ia)
		}
	}		
	
	return( x )
}	

	
	
###############################################################################
# Create efficient frontier
###############################################################################
portopt <- function
(
	ia,						# Input Assumptions
	constraints = NULL,		# Constraints
	nportfolios = 50,		# Number of portfolios
	name = 'Risk',			# Name
	min.risk.fn = min.risk.portfolio	# Risk Measure
)
{
	# load / check required packages
	load.packages('quadprog,corpcor,lpSolve')
	
	# set up constraints
	if( is.null(constraints) ) {
		constraints = new.constraints(rep(0, ia$n), 0, type = '>=')
	} 	
		
	# set up solve.QP
	ia$risk = iif(ia$risk == 0, 0.000001, ia$risk)
	if( is.null(ia$cov) ) ia$cov = ia$cor * (ia$risk %*% t(ia$risk))		
	
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
				
	if(!is.positive.definite(ia$cov.temp)) {
		ia$cov.temp <- make.positive.definite(ia$cov.temp)
	}	
	

	
	# set up output 
	out = list(weight = matrix(NA, nportfolios, nrow(constraints$A)))
		colnames(out$weight) = rep('', ncol(out$weight))
		colnames(out$weight)[1:ia$n] = ia$symbols
		
			
	# find maximum return portfolio	
	out$weight[nportfolios, ] = max.return.portfolio(ia, constraints)

	# find minimum risk portfolio
	out$weight[1, ] = match.fun(min.risk.fn)(ia, constraints)	

	# find points on efficient frontier
	out$return = portfolio.return(out$weight, ia)
	target = seq(out$return[1], out$return[nportfolios], length.out = nportfolios)

if(T) {	
	constraints = add.constraints(c(ia$expected.return, rep(0, nrow(constraints$A) - ia$n)), 
						target[1], type = '>=', constraints)
									
	for(i in 2:(nportfolios - 1) ) {
	
	
		constraints$b[ len(constraints$b) ] = target[i]
		out$weight[i, ] = match.fun(min.risk.fn)(ia, constraints)
		
		
		
	}
} else {

	constraints = add.constraints(c(ia$expected.return, rep(0, nrow(constraints$A) - ia$n)), 
					target[1], type = '=', constraints)
			
	for(i in 2:(nportfolios - 1) ) {
		constraints$b[1] = target[i]
		out$weight[i, ] = match.fun(min.risk.fn)(ia, constraints)
	}
	
}	
	
	
	# compute risk / return
	out$return = portfolio.return(out$weight, ia)
	out$risk = portfolio.risk(out$weight, ia)
	out$name = name
	
	return(out)			
}



###############################################################################
# Visualize input assumptions
###############################################################################
plot.ia <- function
(
	ia		# input assumptions
)
{
	# create a table with summary statistics
	layout(1:2)
	temp = cbind(ia$expected.return, ia$risk)
		temp[] = plota.format(100 * temp[], 1, '', '%')
		temp = cbind(ia$symbol.names, temp)
		colnames(temp) = spl('Name,Return,Risk')
	plot.table(temp, 'Symbol')
	
	# visualize correlation  matrix
	temp = ia$cor
		temp[lower.tri(temp, TRUE)] = NA
		temp = temp[-ia$n, -1]
		temp[] = plota.format(100 * temp[], 1, '', '%')			
	plot.table(temp, highlight = TRUE, colorbar = TRUE)	
}

###############################################################################
# Plot efficient fontier(s) and transitopn map
###############################################################################
plot.ef <- function
(
	ia,						# input assumption
	efs,					# efficient fontier(s)
	portfolio.risk.fn = portfolio.risk,	# risk measure
	transition.map = TRUE	# flag to plot transitopn map
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
	if(transition.map) layout(1:2)
	
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
		plot.transitopn.map(efs[[i]]$weight, x, risk.label, efs[[i]]$name)
	}
}

###############################################################################
# Plot transitopn map
###############################################################################
plot.transitopn.map <- function
(
	y,				# weights
	x,				# x data
	xlab = 'Risk',	# x label
	name = ''		# name

)
{
	if( is.list(y) ) {
		name = y$name
		x = 100 * y$risk
		y = y$weight
	}
		
		
	par(mar = c(4,3,2,1), cex = 0.8)
	plota.stacked(x, y, xlab = xlab, main = paste('Transition Map for', name))				
}

