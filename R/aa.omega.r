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
# Optimizing Omega Ration Functions
# Copyright (C) 2011  Michael Kapler
#
# For more information please visit my blog at www.SystematicInvestor.wordpress.com
# or drop me a line at TheSystematicInvestor at gmail
###############################################################################



###############################################################################
# Omega
# page 6,9, Optimizing Omega by H. Mausser, D. Saunders, L. Seco
#
# Let x.i , i= 1,...,n  be weights of instruments in the portfolio.
# Let us suppose that j = 1,...,T scenarios of returns are available 
# ( r.ij denotes return of i -th asset in the scenario j ). 
#
# The Omega function has the form 
# MAX [ SUM <over j> 1/T * u.j ]
# [ SUM <over i> r.ij * x.i ] - u.j + d.j - L * t = 0, for each j = 1,...,T 
# [ SUM <over j> 1/T * d.j ] = 1
#  u.j, d.j >= 0, for each j = 1,...,T 
#
# Binary b.j enforces that only one of u.j or d.j is greter than 0
# u.j <= b.j
# d.j <= 1 - b.j
#' @export 
###############################################################################
add.constraint.omega <- function
(
	ia,			# input assumptions
	value,		# b value
	type = c('=', '>=', '<='),	# type of constraints
	constraints	# constraints structure
)
{
	if(is.null(ia$parameters.omega)) omega = 0 else omega = ia$parameters.omega

	n0 = ncol(ia$hist.returns)
	n = nrow(constraints$A)	
	nt = nrow(ia$hist.returns)

	# adjust constraints, add u.j, d.j, t
	constraints = add.variables(2*nt + 1, constraints, lb = c(rep(0,2*nt),-Inf))
		# Aw < b => Aw1 - bt < 0
		constraints$A[n + 2*nt + 1, ] = -constraints$b
		constraints$b[] = 0	
		
		# lb/ub same transformation
		index = which( constraints$ub[1:n] < +Inf )	
		if( len(index) > 0 ) {			
			a = rbind( diag(n), matrix(0, 2*nt, n), -constraints$ub[1:n])		
			constraints = add.constraints(a[, index], rep(0, len(index)), '<=', constraints)			
		}
		
		index = which( constraints$lb[1:n] > -Inf )	
		if( len(index) > 0 ) {	
			a = rbind( diag(n), matrix(0, 2*nt, n), -constraints$lb[1:n])		
			constraints = add.constraints(a[, index], rep(0, len(index)), '>=', constraints)					
		}
		
		constraints$lb[1:n] = -Inf
		constraints$ub[1:n] = Inf
		
			
	# [ SUM <over i> r.ij * x.i ] - u.j + d.j - L * t = 0, for each j = 1,...,T 	
	a = rbind( matrix(0, n, nt), -diag(nt), diag(nt), -omega)
		a[1 : n0, ] = t(ia$hist.returns)
	constraints = add.constraints(a, rep(0, nt), '=', constraints)			
		
	# [ SUM <over j> 1/T * d.j ] = 1	
	constraints = add.constraints(c( rep(0,n), rep(0,nt), (1/nt) * rep(1,nt), 0), 1, '=', constraints)				
			
	# objective : Omega
	# [ SUM <over j> 1/T * u.j ]
	constraints = add.constraints(c(rep(0, n), (1/nt) * rep(1, nt), rep(0, nt), 0), value, type[1], constraints)	
			
	return( constraints )	
}

#' @export 
portfolio.omega <- function
(
	weight,		# weight
	ia			# input assumptions
)	
{	
	weight = weight[, 1:ia$n]
	if(is.null(ia$parameters.omega)) omega = 0 else omega = ia$parameters.omega
		
	portfolio.returns = weight %*% t(ia$hist.returns)	
	return( apply(portfolio.returns, 1, function(x) mean(pmax(x - omega,0)) / mean(pmax(omega - x,0)) ) )
}	


###############################################################################
# Find portfolio that Maximizes Omega
#' @export 
###############################################################################
max.omega.portfolio <- function
(
	ia,				# input assumptions
	constraints,	# constraints
	type = c('mixed', 'lp', 'nlp')
)
{
	n = nrow(constraints$A)	
	nt = nrow(ia$hist.returns)
	type = type[1]
	
	if(type == 'mixed'	|| type == 'lp') {
		sol = optimize.portfolio(ia, constraints, add.constraint.omega, portfolio.omega, 'max', T)
			
		x = rep(NA, n)	
		if(!inherits(sol, 'try-error') && sol$status ==0) {
			x0 = sol$solution[1:n]
			u = sol$solution[(1+n):(n+nt)]
			d = sol$solution[(n+nt+1):(n+2*nt)] 
			t = sol$solution[(n+2*nt+1):(n+2*nt+1)] 		
			x = x0/t
		}
	}

	#portfolio.omega(t(x),ia)
	#sol$value

	if((type == 'mixed' && (sol$status !=0 || any( u*d != 0 ) )) || type == 'nlp') {
		# Try solving problem using Rdonlp2
		if(is.null(ia$parameters.omega)) omega = 0 else omega = ia$parameters.omega
	
		# omega
		fn <- function(x){
			portfolio.returns = x %*% t(ia$hist.returns)	
			mean(pmax(portfolio.returns - omega,0)) / mean(pmax(omega - portfolio.returns,0))
		}
		
		x = optimize.portfolio.nlp(ia, constraints, fn, direction = 'max')
		#portfolio.omega(t(x),ia)
	}

	return( x )
}


###############################################################################
# Create efficient frontier
#' @export 
###############################################################################
portopt.omega <- function
(
	ia,						# Input Assumptions
	constraints = NULL,		# Constraints
	nportfolios = 50,		# Number of portfolios
	name = 'Omega'			# Name
)
{
	# set up output 
	out = list(weight = matrix(NA, nportfolios, nrow(constraints$A)))
		colnames(out$weight) = rep('', ncol(out$weight))
		colnames(out$weight)[1:ia$n] = ia$symbols

				
	ef.risk = portopt(ia, constraints, 2)	
					
	# maximum return portfolio	
	out$weight[nportfolios, ] = ef.risk$weight[2,]

	# minimum risk portfolio
	out$weight[1, ] = ef.risk$weight[1,]
		constraints$x0 = out$weight[1, ]
	
	# find points on efficient frontier
	out$return = portfolio.return(out$weight, ia)
	target = seq(out$return[1], out$return[nportfolios], length.out = nportfolios)

	constraints = add.constraints(c(ia$expected.return, rep(0, nrow(constraints$A) - ia$n)), 
						target[1], type = '<=', constraints)
									
	for(i in 1:nportfolios ) {
		cat('i =', i, '\n')
	
		constraints$b[ len(constraints$b) ] = -target[i]
		out$weight[i, ] = max.omega.portfolio(ia, constraints)
		
			constraints$x0 = out$weight[i, ]		
	}
	
	
	# compute risk / return
	out$return = portfolio.return(out$weight, ia)
	out$risk = portfolio.risk(out$weight, ia)
	out$name = name
	
	return(out)			
}


###############################################################################
# Plot Omega Ratio for given portfolios (weights)
#' @export 
###############################################################################
plot.omega <- function
(
	weight,		# weight
	ia			# input assumptions
)	
{	
	omegafn = function(x,L) { mean(pmax(x-L,0)) / mean(pmax(L-x,0)) }

	if(is.null(ia$parameters.omega)) omega = 0 else omega = ia$parameters.omega	
	
	weight = weight[, 1:ia$n, drop=F]
		
	portfolio.returns = weight %*% t(ia$hist.returns)	
	
	threshhold = quantile(portfolio.returns, probs = c(0.05, 0.95))
	threshhold = seq(threshhold[1], threshhold[2], length.out = 100)

	par(mar = c(4,4,2,1), cex = 0.8)
	for(i in 1:nrow(weight)) {	
		data = sapply(threshhold, function(L) omegafn(portfolio.returns[i, ], L))
		
		if(i==1) plot(threshhold,log(data), type='l', col=i, 
			xlab='Threshhold', ylab='Log(Omega)', main='Portfolio Omega')
		lines(threshhold, log(data), col=i)
	}
	abline(v = omega, col='orange')
	grid()
	plota.legend(rownames(weight) ,1:nrow(weight), x = 'bottomleft')
}

