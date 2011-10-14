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
###############################################################################


###############################################################################
# Solve LP problem, allow negative x
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
###############################################################################
lp.anyx <- function(direction, objective.in, const.mat, const.dir,const.rhs, min.x.bounds = -100)
{
	dvec = min.x.bounds
	if( len(min.x.bounds) == 1 ) dvec = rep(min.x.bounds, len(objective.in))
	
	sol = lp( direction, objective.in, const.mat, const.dir, 
			const.rhs - const.mat %*% dvec )
	
	sol$solution = sol$solution + dvec
		
	return( sol )
}

