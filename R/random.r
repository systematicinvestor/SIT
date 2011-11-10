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
# Copyright (C) 2011  Michael Kapler - ported Roger Stafford's code to R
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

