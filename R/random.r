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
# Collection of Random Generators
# Copyright (C) 2005-2006  Roger Stafford - the author of the original Matlab version
# Copyright (C) 2012  Michael Kapler - ported Roger Stafford's code to R
#
# For more information please visit my blog at www.SystematicInvestor.wordpress.com
# or drop me a line at TheSystematicInvestor at gmail
###############################################################################


###############################################################################
# Random Points in an n-Dimensional Hypersphere
# by Roger Stafford
# 24 Dec 2005 (Updated 28 Dec 2005)
#
# Randomly and uniformly distributes points throughout a hypersphere. 
#
# http://www.mathworks.com/matlabcentral/fileexchange/9443-random-points-in-an-n-dimensional-hypersphere
###############################################################################
# This function returns an m by n array, X, in which 
# each of the m rows has the n Cartesian coordinates 
# of a random point uniformly-distributed over the 
# interior of an n-dimensional hypersphere with 
# radius r and center at the origin.  The function 
# 'randn' is initially used to generate m sets of n 
# random variables with independent multivariate 
# normal distribution, with mean 0 and variance 1.
# Then the incomplete gamma function, 'gammainc', 
# is used to map these points radially to fit in the 
# hypersphere of finite radius r with a uniform % spatial distribution.
# Roger Stafford - 12/23/05
#' @export 
###############################################################################
randsphere <- function
(
	m,	# number of samples to draw
	n,	# dimension of distribution
	r	# radius of hypersphere with center at the origin
)
{
	x = matrix(rnorm( m * n ), nrow = m, ncol = n);
	s2 = apply(x^2, 1, sum)

	#return( x * repmat(r*(pgamma(s2/2,n/2)^(1/n))/sqrt(s2),1,n) )
	return( x * repmat(r*(runif(m)^(1/n))/sqrt(s2),1,n) )
}

###############################################################################
# Test for randsphere function
###############################################################################
randsphere.test <- function()
{
	load.packages('car,scatterplot3d')

png(filename = 'plot1.png', width = 500, height = 500, units = 'px', pointsize = 12, bg = 'white')	
		
	# 2 dimensions
	x = randsphere(1000, 2, 1)
		y = x[, 2]
		x = x[, 1]
	
	par(mar = c(5,4,1,1))
	plot(x,y, pch = 20)
	ellipse(c(0, 0), matrix(c(1, 0, 0, 1), nrow = 2), radius = 1)

dev.off()
png(filename = 'plot2.png', width = 500, height = 500, units = 'px', pointsize = 12, bg = 'white')	
	
	# 3 dimensions
	# plot 4 plots : 3d + 3 projections xy,xz,yz
	layout(matrix(1:4,nrow=2))
	
	x = randsphere(10000, 3, 1)
		z = x[, 3]
		y = x[, 2]
		x = x[, 1]
		
	scatterplot3d(x, y, z, highlight.3d = TRUE, pch = 20)
	
	par(mar = c(5,4,1,1))		
	plot(x, y, pch = 20)			
		ellipse(c(0, 0), matrix(c(1, 0, 0, 1), nrow = 2), radius = 1)
	plot(x, z, pch = 20)			
		ellipse(c(0, 0), matrix(c(1, 0, 0, 1), nrow = 2), radius = 1)
	plot(y, z, pch = 20)
		ellipse(c(0, 0), matrix(c(1, 0, 0, 1), nrow = 2), radius = 1)
				
dev.off()
}


###############################################################################
# Random Vectors with Fixed Sum
# by Roger Stafford
# 19 Jan 2006 (Updated 24 Jan 2006)
#
# Randomly and uniformly generates vectors with a specified sum and values in a specified interval.
#
# http://www.mathworks.com/matlabcentral/fileexchange/9700-random-vectors-with-fixed-sum
###############################################################################
# [x,v] = randfixedsum(n,m,s,a,b)
#
#   This generates an n by m array x, each of whose m columns
# contains n random values lying in the interval [a,b], but
# subject to the condition that their sum be equal to s.  The
# scalar value s must accordingly satisfy n*a <= s <= n*b.  The
# distribution of values is uniform in the sense that it has the
# conditional probability distribution of a uniform distribution
# over the whole n-cube, given that the sum of the x's is s.
#
#   The scalar v, if requested, returns with the total
# n-1 dimensional volume (content) of the subset satisfying
# this condition.  Consequently if v, considered as a function
# of s and divided by sqrt(n), is integrated with respect to s
# from s = a to s = b, the result would necessarily be the
# n-dimensional volume of the whole cube, namely (b-a)^n.
#
#   This algorithm does no "rejecting" on the sets of x's it
# obtains.  It is designed to generate only those that satisfy all
# the above conditions and to do so with a uniform distribution.
# It accomplishes this by decomposing the space of all possible x
# sets (columns) into n-1 dimensional simplexes.  (Line segments,
# triangles, and tetrahedra, are one-, two-, and three-dimensional
# examples of simplexes, respectively.)  It makes use of three
# different sets of 'rand' variables, one to locate values
# uniformly within each type of simplex, another to randomly
# select representatives of each different type of simplex in
# proportion to their volume, and a third to perform random
# permutations to provide an even distribution of simplex choices
# among like types.  For example, with n equal to 3 and s set at,
# say, 40% of the way from a towards b, there will be 2 different
# types of simplex, in this case triangles, each with its own
# area, and 6 different versions of each from permutations, for
# a total of 12 triangles, and these all fit together to form a
# particular planar non-regular hexagon in 3 dimensions, with v
# returned set equal to the hexagon's area.
#
# Roger Stafford - Jan. 19, 2006
#' @export 
###############################################################################
randfixedsum <- function
(
	m,	# number of samples to draw
	n,	# dimension of distribution
	s,	# sum of each sample equal to s
	a,	# lower bound for each variable
	b	# upper bound for each variable
)
# The scalar value s must accordingly satisfy n*a <= s <= n*b.
{
	# Check the arguments.
	if( (s<n*a) | (s>n*b) | (a>=b) )
 		stop('Inequalities n*a <= s <= n*b and a < b must hold.\n')

	# Rescale to a unit cube: 0 <= x(i) <= 1
	s = (s - n * a) / (b - a)
	
	
	# Construct the transition probability table, t.
	# t(i,j) will be utilized only in the region where j <= i + 1.
	k = max( min( floor(s), n - 1), 0)	# Must have 0 <= k <= n-1
	s = max( min( s, k + 1), k)			# Must have k <= s <= k+1
	
	s1 = s - (k : (k - n + 1))			# s1 & s2 will never be negative
	s2 = ((k + n) : (k+1)) - s
		
	w = matrix(0, n, (n + 1))
	realmax = 10^300
	w[1, 2] = realmax					# Scale for full 'double' range
	
	t = matrix(0, (n-1), n)
	tiny = 2^(-1074)					# The smallest positive matlab 'double' no.
	
	for( i in 2:n ) {
		tmp1 = w[(i - 1), 2 : (i + 1)] * s1[1 : i] / i
		tmp2 = w[(i - 1), 1 : i] * s2[(n - i + 1) : n] / i
		
		w[i, 2 : (i + 1)] = tmp1 + tmp2
		tmp3 = w[i, 2 : (i+1)] + tiny			# In case tmp1 & tmp2 are both 0,
		tmp4 = (s2[(n - i + 1) : n] > s1[1:i])	# then t is 0 on left & 1 on right
 		t[(i - 1), 1 : i] = (tmp2 / tmp3) * tmp4 + (1 - tmp1 / tmp3) * (!tmp4)
	}

	
	# Derive the polytope volume v from the appropriate
	# element in the bottom row of w.
	v = n^(3/2) * (w[n, (k + 2)] / realmax) * (b - a)^(n - 1)

	
	# Now compute the matrix x.
	x = matrix(0, n, m)
	rt = matrix( runif((n-1) * m), (n-1), m)	# For random selection of simplex type
	rs = matrix( runif((n-1) * m), (n-1), m)	# For random location within a simplex
	
	s = repmat(s, 1, m)
	j = repmat((k + 1), 1, m)	# For indexing in the t table
	
	sm = matrix(0, 1, m)
	pr = matrix(1, 1, m)	# Start with sum zero & product 1

	for( i in (n - 1):1) {  # Work backwards in the t table
 		e = (rt[(n - i), ] <= t[i, j])		# Use rt to choose a transition
 		sx = rs[(n - i), ]^(1/i)			# Use rs to compute next simplex coord.
 		sm = sm + (1 - sx) * pr * s / (i+1)	# Update sum
 		pr = sx * pr						# Update product
 		x[(n - i), ] = sm + pr * e			# Calculate x using simplex coords.
 		s = s - e
 		j = j - e							# Transition adjustment
	}
	
	x[n, ] = sm + pr * s					# Compute the last x


	# Randomly permute the order in the columns of x and rescale.
	rp = matrix( runif(n * m), n, m)		# Use rp to carry out a matrix 'randperm'
	p = apply(rp, 2, order)
	x = (b - a) * x[p + repmat(t(seq(0, n * (m - 1), by = n)), n, 1)] + a	# Permute & rescale x
	
	x = matrix(x, n, m)
	#apply(x,2,sum)	
	
	return(t(x))
}

# Alternative Idea
# http://programming-r-pro-bro.blogspot.com/2011/11/modern-portfolio-optimization-theory.html
# diff(c(0,sort(runif(n-1)), 1))
# diff(c(0,sort(runif(n-1)), 1)) * 0.5
# diff(c(0,sort(runif(n-1,0,0.8)), 0.8))
#
#PORTFOLIO OPTIMIZATION FOR VAR, CVAR, OMEGA AND UTILITY WITH GENERAL RETURN DISTRIBUTIONS:
#A MONTE CARLO APPROACH FOR LONG-ONLY AND BOUNDED SHORT PORTFOLIOS WITH OPTIONAL ROBUSTNESS
#AND A SIMPLIFIED APPROACH TO COVARIANCE MATCHING
#by WILLIAM T. SHAW
#
#http://luc.devroye.org/rnbookindex.html
# Non-Uniform Random Variate Generation by Luc Devroye (1986), chapter 5


###############################################################################
# Test for randfixedsum function
###############################################################################
randfixedsum.test <- function()
{
	load.packages('car,scatterplot3d')

png(filename = 'plot1.png', width = 500, height = 500, units = 'px', pointsize = 12, bg = 'white')	
		
	# 2 dimensions
	x = randfixedsum(100, 2, 1, 0.2, 0.8)
		y = x[, 2]
		x = x[, 1]
	
	par(mar = c(5,4,1,1))
	plot(x,y, pch = 20)

dev.off()
png(filename = 'plot2.png', width = 500, height = 500, units = 'px', pointsize = 12, bg = 'white')	

	# 3 dimensions
	# http://www.statmethods.net/graphs/scatterplot.html
	# plot 4 plots : 3d + 3 projections xy,xz,yz
	layout(matrix(1:4,nrow=2))
	
	x = randfixedsum(1000, 3, 1, 0.2, 0.8)
		z = x[, 3]
		y = x[, 2]
		x = x[, 1]
		
	scatterplot3d(x, y, z, highlight.3d = TRUE, pch = 20, angle=190)
	
	par(mar = c(5,4,1,1))		
	plot(x, y, pch = 20)			
	plot(x, z, pch = 20)			
	plot(y, z, pch = 20)
	
	
dev.off()
}


###############################################################################
# Note rowSums and colSums are alot faster than apply. For example:
# prices = asset.paths(S, c(r,r), sigma = cov.matrix, N, periods = seq(0, 1/12, 1/360))
#
# temp = prices
# dim(temp) = c(2,31*N)
#
# tic(12)
# a = apply(temp,2,sum)
# toc(12)
#
# tic(12)
# b=colSums(temp)
# toc(12)
#
# range(a-b)
###############################################################################
# s0 * matrix(exp(nu * dt + sigma * sqrt(dt) * rnorm(nsteps * nsims)), nc=nsims)
#
# s0 * apply(matrix(exp(nu * dt + sigma * sqrt(dt) * rnorm(nsteps * nsims)), nc=nsims), 2, cumprod)
# s0 * exp(apply(matrix(nu * dt + sigma * sqrt(dt) * rnorm(nsteps * nsims), nc=nsims), 2, cumsum))
###############################################################################
# Simulating Multiple Asset Paths
# http://www.goddardconsulting.ca/matlab-monte-carlo-assetpaths.html
# http://en.wikipedia.org/wiki/Geometric_Brownian_motion
#
# This function returns the simulation matrix (time, simulation, asset)
# Instead of using mvrnorm from MASS library, we can use rmvnorm function from mvtnorm package
#' @export 
###############################################################################
asset.paths <- function(s0, mu, sigma, 
	nsims = 10000, 
	periods = c(0, 1)	# time periods at which to simulate prices
) 
{
	s0 = as.vector(s0)
	nsteps = len(periods)
	dt = c(periods[1], diff(periods))
	
	if( len(s0) == 1 ) {
		drift = mu - 0.5 * sigma^2
		if( nsteps == 1 ) {
			s0 * exp(drift * dt + sigma * sqrt(dt) * rnorm(nsims))
		} else {
			temp = matrix(exp(drift * dt + sigma * sqrt(dt) * rnorm(nsteps * nsims)), nc=nsims)
			for(i in 2:nsteps) temp[i,] = temp[i,] * temp[(i-1),]
			s0 * temp
		}
	} else {
		require(MASS)
		drift = mu - 0.5 * diag(sigma)
		n = len(mu)
		
		if( nsteps == 1 ) {
			s0 * exp(drift * dt + sqrt(dt) * t(mvrnorm(nsims, rep(0, n), sigma)))
		} else {
			temp = array(exp(as.vector(drift %*% t(dt)) + t(sqrt(dt) * mvrnorm(nsteps * nsims, rep(0, n), sigma))), c(n, nsteps, nsims))
			for(i in 2:nsteps) temp[,i,] = temp[,i,] * temp[,(i-1),]
			s0 * temp
		}
	}
}	


###############################################################################
# Test asset.paths function
###############################################################################
asset.paths.test <- function()
{
	#*****************************************************************
	# Plot some price paths
	#******************************************************************  
	S = c(100,105)
	X = 98
	Time = 0.5
	r = 0.05
	sigma = c(0.11,0.16)
	rho = 0.63
	N = 10000

	# Single Asset for 10 years
	periods = 0:10
	prices = asset.paths(S[1], r, sigma[1], N, periods = periods)
	
	# plot
png(filename = 'plot1.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')		    
	matplot(prices[,1:100], type='l', xlab='Years', ylab='Prices',
		main='Selected Price Paths')
dev.off()					
	
		
	# Multiple Assets for 10 years
	periods = 0:10
	cov.matrix = sigma%*%t(sigma) * matrix(c(1,rho,rho,1),2,2)
	prices = asset.paths(S, c(r,r), cov.matrix, N, periods = periods)

	# plot
png(filename = 'plot2.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')		    	
	layout(1:2)
	matplot(prices[1,,1:100], type='l', xlab='Years', ylab='Prices',
		main='Selected Price Paths for Asset 1')
	matplot(prices[2,,1:100], type='l', xlab='Years', ylab='Prices',
		main='Selected Price Paths for Asset 2')
dev.off()					
	
	# check correlation
	cor(as.vector(prices[1,,] / mlag(prices[1,,])),
		as.vector(prices[2,,] / mlag(prices[2,,])),
	use='complete.obs', method='pearson')
	
		
	#*****************************************************************
	# Price European Call Option
	#******************************************************************  
	load.packages('fOptions')

	# Black–Scholes
	GBSOption(TypeFlag = "c", S = S[1], X = X, Time = Time, r = r, b = r, sigma = sigma[1])
	
	# Monte Carlo simulation	
	N = 1000000	
	prices = asset.paths(S[1], r, sigma[1], N, periods = Time)
		future.payoff = pmax(0, prices - X)
		discounted.payoff = future.payoff * exp(-r * Time)
	# option price
	mean(discounted.payoff) 

	#*****************************************************************
	# Price Asian Call Option
	#******************************************************************  
	load.packages('fExoticOptions')

	Time = 1/12

	# Approximation
	TurnbullWakemanAsianApproxOption(TypeFlag = "c", S = S[1], SA = S[1], 
  		X = X, Time = Time, time = Time, tau = 0 , r = r, b = r, sigma = sigma[1])
		
	# Monte Carlo simulation		
	N = 100000
	periods = seq(0,Time,1/360)
		n = len(periods)
	prices = asset.paths(S[1], r, sigma[1], N, periods = periods)
		future.payoff = pmax(0, colSums(prices)/n - X)
		discounted.payoff = future.payoff * exp(-r * Time)
	# option price
	mean(discounted.payoff) 

	#*****************************************************************
	# Price Basket Option
	#******************************************************************  
	
	Time = 0.5
	
	# Approximation
	TwoRiskyAssetsOption(TypeFlag = "cmax", S1 = S[1], S2 = S[2],
		X = X, Time = Time, r = r, b1 = r, b2 = r,
		sigma1 = sigma[1], sigma2 = sigma[2], rho = rho)
	
	# Monte Carlo simulation		
	N = 100000
	cov.matrix = sigma%*%t(sigma) * matrix(c(1,rho,rho,1),2,2)
	prices = asset.paths(S, c(r,r), sigma = cov.matrix, N, periods = Time)
		future.payoff = pmax(0, apply(prices,2,max) - X)
		discounted.payoff = future.payoff * exp(-r * Time)
	# option price
	mean(discounted.payoff) 

	#*****************************************************************
	# Price Asian Basket Option
	#******************************************************************  
	
	Time = 1/12
	
	# Monte Carlo simulation		
	N = 10000
	periods = seq(0,Time,1/360)
		n = len(periods)
	
	prices = asset.paths(S, c(r,r), sigma = cov.matrix, N, periods = periods)
		future.payoff = pmax(0, colSums(apply(prices,c(2,3),max))/n - X)
		discounted.payoff = future.payoff * exp(-r * Time)
	# option price
	mean(discounted.payoff) 

}


#******************************************************************************
# Simulate Historical Prices
#' @export 
#******************************************************************************
mat.3d = function(mat, n3dim) { array(mat, c(dim(mat), n3dim)) }

# aperm(array(t(matrix(1:12, nr=6)),c(2,3,2)),c(2,1,3))
#' @export 
mat.slice.3d = function(mat, slice.at) { aperm(array(t(mat), c(ncol(mat), nrow(mat) / slice.at, slice.at)),c(2,1,3)) }


# assuming strategy makes all descions at period.ends
# asset.paths.at.period.ends function simulates price paths
# that resemable history at each period.end
#
# please note that to generate prices at period.end
# asset.paths.at.period.ends function examines historical
# prices prior to and including period.end
#' @export 
asset.paths.at.period.ends <- function(
	prices,
	period.ends,
	nsims = 100,
	lookback.len = NA	# if lookback.len is missing genearte price scenarios for time T
						# based on information from period.ends[T-1]:period.ends[T]
) {
	# if(any(is.na(prices))) stop('asset.paths.at.period.ends cannot handle missing values')
	
	load.packages('MASS')
	
	ret = prices / mlag(prices) - 1	
		n = ncol(prices)
		nperiods = nrow(prices)	
	
	# find value and location of first non-NA value
	index = 1:nperiods
	first.index = sapply(1:n, function(i) index[!is.na(prices[,i])][1])
	first.value = sapply(1:n, function(i) prices[first.index[i],i])
			
	s0 = rep(1,n)
	scenarios = array(NA, c(nperiods, n, nsims))
	
	period.ends.all = sort(unique(c(0, period.ends, len(period.ends))))

	for(i in 2:len(period.ends.all)) {
		# default lockback is to a prior period end
		index.hist = index = (period.ends.all[i-1] + 1) : period.ends.all[i]
			n.index = len(index)		

		# handle case if user wants to base population data on different lookback			
		if(!is.na(lookback.len)) 
			index.hist = max(1, period.ends.all[i] - lookback.len + 1 ) : period.ends.all[i]
			
		hist = ret[index.hist,, drop=F]
		
		# only consider assets with historical data
  		include.index = colSums(!is.na(hist)) != 0    
  			if(!any(include.index)) continue
  						
  		hist = hist[,include.index, drop=F]
  			
  		# compute population parameters
        mu = apply(hist, 2, mean, na.rm=T)
        risk = apply(hist, 2, sd, na.rm=T)
        correlation = cor(hist, use = 'complete.obs', method = 'pearson')
        cov = correlation * (risk %*% t(risk))
        
        # sample from normal distribution with given population parameters
        scenarios[index,include.index,] = mat.slice.3d(mvrnorm(n.index * nsims, mu, cov), nsims)

		#temp = asset.paths(rep(1,n), mu, cov, nsims, periods = 1:n.index) 		
	}
	
	# first return is always zero
	scenarios[1,,] = 0
		
	for(i in 1:n)
		for(j in 1:nsims) {
			if(first.index[i] > 1) scenarios[first.index[i],i,j] = 0			
			scenarios[first.index[i]:nperiods,i,j] = first.value[i] * cumprod(1 + scenarios[first.index[i]:nperiods,i,j])			
		}
	
	# keep NA's as in the original data
	scenarios = scenarios * mat.3d(1+0*prices, nsims)
	
	scenarios
}


###############################################################################
# Generate All Possible Combinations
# there are 2^n - 1 distinct permutations
# Please note: first row contains all zeros
#' @export 
###############################################################################
all.permutations <- function(n = 1) {
	m = matrix(F,2^n,n)
		m[2,1] = T
	if (n == 1) return(m)
	
	istart = 2
	for(i in 2:n) {
		index = (istart+1):(2*istart)
		m[index, ] = m[1:istart,]
			m[index, i] = T
		istart = istart * 2	
	}
	return(m)
}