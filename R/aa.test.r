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
# Test cases for Asset Allocation Functions
# Copyright (C) 2011  Michael Kapler
#
# For more information please visit my blog at www.SystematicInvestor.wordpress.com
# or drop me a line at TheSystematicInvestor at gmail
###############################################################################


###############################################################################
# Test AA functions, introduction
###############################################################################
aa.test <- function()
{
	#--------------------------------------------------------------------------
	# Create historical input assumptions
	#--------------------------------------------------------------------------

	ia = aa.test.create.ia()
	
png(filename = 'plot1.png', width = 500, height = 500, units = 'px', pointsize = 12, bg = 'white')	
	
	# visualize input assumptions
	plot.ia(ia)
	
dev.off()	
png(filename = 'plot2.png', width = 500, height = 500, units = 'px', pointsize = 12, bg = 'white')	
		

	# display each asset in the Risk - Return plot 
	layout(1)
	par(mar = c(4,4,2,1), cex = 0.8)
	x = 100 * ia$risk
	y = 100 * ia$expected.return
	
	plot(x, y, xlim = range(c(0, x)), ylim = range(c(0, y)),
		xlab='Risk', ylab='Return', main='Risk vs Return', col='black')
	grid();
	text(x, y, ia$symbols,	col = 'blue', adj = c(1,1), cex = 0.8)
	
dev.off()

	#--------------------------------------------------------------------------
	# Create Efficient Frontier
	#--------------------------------------------------------------------------
	n = ia$n		

	# 0 <= x.i <= 0.8 
	constraints = new.constraints(n, lb = 0, ub = 0.8)

	# SUM x.i = 1
	constraints = add.constraints(rep(1, n), 1, type = '=', constraints)		
	
	# create efficient frontier
	ef = portopt(ia, constraints, 50, 'Efficient Frontier')

	
	
		
png(filename = 'plot3.png', width = 500, height = 500, units = 'px', pointsize = 12, bg = 'white')	
		
	plot.ef(ia, list(ef))	

dev.off()
	
	#--------------------------------------------------------------------------
	# Plot multiple Efficient Frontiers
	#--------------------------------------------------------------------------
	
	ef.risk = portopt(ia, constraints, 50, 'Risk')
	ef.maxloss = portopt(ia, constraints, 50, 'Max Loss', min.maxloss.portfolio)	
	ef.mad = portopt(ia, constraints, 50, 'MAD', min.mad.portfolio)
	
png(filename = 'plot4.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')	
	
	layout( matrix(1:4, nrow = 2) )
	plot.ef(ia, list(ef.risk, ef.maxloss, ef.mad), portfolio.risk, F)	
	plot.ef(ia, list(ef.risk, ef.maxloss, ef.mad), portfolio.maxloss, F)	
	plot.ef(ia, list(ef.risk, ef.maxloss, ef.mad), portfolio.mad, F)	

dev.off()	
png(filename = 'plot5.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')	

	layout( matrix(1:4, nrow = 2) )
	plot.transition.map(ef.risk)
	plot.transition.map(ef.maxloss)
	plot.transition.map(ef.mad)

dev.off()	

	
}

###############################################################################
# Test AA functions, long/short 130:30
###############################################################################
# Workingimplementation of 130:30
# Asset Allocation and Risk Assessment with Gross Exposure Constraints for Vast Portfolios by J. Fan, Zhang J., Yu K. (2008)
# http://papers.ssrn.com/sol3/papers.cfm?abstract_id=1307423
#
# Note 3 on Page 8
# To get 130 long, 30 short
#--------------------------------------------
# One alternative
# -v.i <= x.i <= v.i, v.i>0, SUM(v.i) = 1.6
#
# Transfrom the covariance Q into
# | Q    0*Q |
# | 0*Q  0*Q |
#--------------------------------------------
# Another alternative
# Split x into x.long and x.short, x_long and x_short >= 0
# SUM(x.long) - SUM(x.short) = 1.6
#
# Transfrom the covariance Q into
# | Q -Q |
# |-Q  Q |
#--------------------------------------------
# The problem is that 1.6 is not always inforced because
# minimum variance can be achived at a lower leverage
###############################################################################
aa.long.short.test <- function()
{
	#--------------------------------------------------------------------------
	# Create Efficient Frontier
	#--------------------------------------------------------------------------
	ia = aa.test.create.ia()
	n = ia$n		

	# -0.5 <= x.i <= 0.8 
	constraints = new.constraints(n, lb = -0.5, ub = 0.8)
		
	# SUM x.i = 1
	constraints = add.constraints(rep(1, n), 1, type = '=', constraints)		
	
	# create efficient frontier(s)
	ef.risk = portopt(ia, constraints, 50, 'Risk')
	ef.mad = portopt(ia, constraints, 50, 'MAD', min.mad.portfolio)
		
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')	
	
	layout( matrix(1:4, nrow = 2) )
	plot.ef(ia, list(ef.risk, ef.mad), portfolio.risk, F)	
	plot.ef(ia, list(ef.risk, ef.mad), portfolio.mad, F)	

	plot.transition.map(ef.risk)
	plot.transition.map(ef.mad)
	
dev.off()

	#--------------------------------------------------------------------------
	# Create 130:30
	# -v.i <= x.i <= v.i, v.i>0, SUM(v.i) = 1.6
	#--------------------------------------------------------------------------

	# -0.5 <= x.i <= 0.8 
	constraints = new.constraints(n, lb = -0.5, ub = 0.8)
	
	# SUM x.i = 1
	constraints = add.constraints(rep(1, n), 1, type = '=', constraints)		

	# adjust prior constraints, add v.i
	constraints = add.variables(n, constraints)

	# -v.i <= x.i <= v.i
	#   x.i + v.i >= 0
	constraints = add.constraints(rbind(diag(n), diag(n)), rep(0, n), type = '>=', constraints)
	#   x.i - v.i <= 0
	constraints = add.constraints(rbind(diag(n), -diag(n)), rep(0, n), type = '<=', constraints)
	
	# SUM(v.i) = 1.6
	constraints = add.constraints(c(rep(0, n), rep(1, n)), 1.6, type = '=', constraints)

	# create efficient frontier(s)
	ef.risk = portopt(ia, constraints, 50, 'Risk')
		# keep only portfolio weights
		ef.risk$weight = ef.risk$weight[,(1:n)]			
		
	ef.mad = portopt(ia, constraints, 50, 'MAD', min.mad.portfolio)
		ef.mad$weight = ef.mad$weight[,(1:n)]
		
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')	
		
	layout( matrix(1:4, nrow = 2) )
	plot.ef(ia, list(ef.risk, ef.mad), portfolio.risk, F)	
	plot.ef(ia, list(ef.risk, ef.mad), portfolio.mad, F)	

	plot.transition.map(ef.risk)
	plot.transition.map(ef.mad)
		
dev.off()

	#--------------------------------------------------------------------------
	# Create 130:30
	# Split x into x.long and x.short, x_long and x_short >= 0
	# SUM(x.long) - SUM(x.short) = 1.6
	#--------------------------------------------------------------------------
	ia.ls = aa.test.ia.add.short(ia)
	
	# x.long and x.short >= 0
	# x.long <= 0.8 
	# x.short <= 0.5 
	constraints = new.constraints(2*n, lb = 0, ub = c(rep(0.8,n),rep(0.5,n)))
		
	# SUM (x.long - x.short) = 1
	constraints = add.constraints(c(rep(1,n), -rep(1,n)), 1, type = '=', constraints)		

	# SUM (x.long + x.short) = 1.6
	constraints = add.constraints(c(rep(1,n), rep(1,n)), 1.6, type = '=', constraints)		

	# create efficient frontier(s)
	ef.risk = portopt(ia.ls, constraints, 50, 'Risk')
		# compute x
		ef.risk$weight = ef.risk$weight[, 1:n] - ef.risk$weight[, (n+1):(2*n)]
		
	ef.mad = portopt(ia.ls, constraints, 50, 'MAD', min.mad.portfolio)
		ef.mad$weight = ef.mad$weight[, 1:n] - ef.mad$weight[, (n+1):(2*n)]
		
png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')	
		
	layout( matrix(1:4, nrow = 2) )
	plot.ef(ia, list(ef.risk, ef.mad), portfolio.risk, F)	
	plot.ef(ia, list(ef.risk, ef.mad), portfolio.mad, F)	

	plot.transition.map(ef.risk)
	plot.transition.map(ef.mad)
	
dev.off()
	

	#--------------------------------------------------------------------------
	# Create 200:100
	# Split x into x.long and x.short, x_long and x_short >= 0
	# SUM(x.long) - SUM(x.short) = 3
	#
	# The problem is that 3 is not always inforced because
	# minimum variance can be achived at a lower leverage	
	#--------------------------------------------------------------------------
	
	# x.long and x.short >= 0
	# x.long <= 0.8 
	# x.short <= 0.5 
	constraints = new.constraints(2*n, lb = 0, ub = c(rep(0.8,n),rep(0.5,n)))
		
	# SUM (x.long - x.short) = 1
	constraints = add.constraints(c(rep(1,n), -rep(1,n)), 1, type = '=', constraints)		

	# SUM (x.long + x.short) = 3
	constraints = add.constraints(c(rep(1,n), rep(1,n)), 3, type = '=', constraints)		

	# create efficient frontier(s)
	ef.risk = portopt(ia.ls, constraints, 50, 'Risk')
		# compute x
		ef.risk$weight = ef.risk$weight[, 1:n] - ef.risk$weight[, (n+1):(2*n)]
		
	ef.mad = portopt(ia.ls, constraints, 50, 'MAD', min.mad.portfolio)
		ef.mad$weight = ef.mad$weight[, 1:n] - ef.mad$weight[, (n+1):(2*n)]
		
png(filename = 'plot4.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')	
		
	layout( matrix(1:4, nrow = 2) )
	plot.ef(ia, list(ef.risk, ef.mad), portfolio.risk, F)	
	plot.ef(ia, list(ef.risk, ef.mad), portfolio.mad, F)	

	plot.transition.map(ef.risk)
	plot.transition.map(ef.mad)
	
dev.off()

	#--------------------------------------------------------------------------
	# Create 200:100 using binary[0/1] variables and Branch and Bound algorithm
	# Split x into x.long and x.short, x_long and x_short >= 0
	# SUM(x.long) - SUM(x.short) = 3
	#
	# Solve using branch and bound: add a binary var b1:bn, xL < b, xS < (1-b)
	#--------------------------------------------------------------------------
		
	# x.long and x.short >= 0
	# x.long <= 0.8 
	# x.short <= 0.5 
	constraints = new.constraints(2*n, lb = 0, ub = c(rep(0.8,n),rep(0.5,n)))
	
	# SUM (x.long - x.short) = 1
	constraints = add.constraints(c(rep(1,n), -rep(1,n)), 1, type = '=', constraints)		

	# SUM (x.long + x.short) = 3
	constraints = add.constraints(c(rep(1,n), rep(1,n)), 3, type = '=', constraints)		
				
	# new add binary constraint	
	# adjust prior constraints: add b.i
	constraints = add.variables(n, constraints)
	
	# index of binary variables b.i
	constraints$binary.index = (2*n+1):(3*n)
	
	# binary variable b.i : x.long < b, x.short < (1 - b)
	# x.long < b
	constraints = add.constraints(rbind(diag(n), 0*diag(n), -diag(n)), rep(0, n), type = '<=', constraints)

	# x.short < (1 - b)
	constraints = add.constraints(rbind(0*diag(n), diag(n), diag(n)), rep(1, n), type = '<=', constraints)
	
	# create efficient frontier(s)
	ef.risk = portopt(ia.ls, constraints, 50, 'Risk')
		# compute x
		ef.risk$weight = ef.risk$weight[, 1:n] - ef.risk$weight[, (n+1):(2*n)]
		
	ef.mad = portopt(ia.ls, constraints, 50, 'MAD', min.mad.portfolio)
		ef.mad$weight = ef.mad$weight[, 1:n] - ef.mad$weight[, (n+1):(2*n)]
		

png(filename = 'plot5.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')	
		
	layout( matrix(1:4, nrow = 2) )
	plot.ef(ia, list(ef.risk, ef.mad), portfolio.risk, F)	
	plot.ef(ia, list(ef.risk, ef.mad), portfolio.mad, F)	

	plot.transition.map(ef.risk)
	plot.transition.map(ef.mad)
	
dev.off()
	
}

###############################################################################
# Test AA functions, Cardinality Constraints
###############################################################################
# Minimum Invesment Constraint
# Pre-determined Number of Asstes Constraint
###############################################################################
aa.cardinality.test <- function()
{

	#--------------------------------------------------------------------------
	# Create Efficient Frontier
	#--------------------------------------------------------------------------
	ia = aa.test.create.ia()
	n = ia$n		

	# 0 <= x.i <= 0.8 
	constraints = new.constraints(n, lb = 0, ub = 0.8)
		
	# SUM x.i = 1
	constraints = add.constraints(rep(1, n), 1, type = '=', constraints)		
	
	# create efficient frontier(s)
	ef.risk = portopt(ia, constraints, 50, 'Risk')
	ef.mad = portopt(ia, constraints, 50, 'MAD', min.mad.portfolio)

png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')			

	aa.plot.ef.summary.test <- function(ef)
	{
		layout(1:2)
		par(mar = c(4,4,2,1), cex = 0.8)
		y = iif(ef$weight > 0.000001, ef$weight, NA) 
		plot(as.vector(sort(100 * y)), pch=20, xaxt='n', ylim = c(0, 80),
			xlab='', ylab='Weight', main='Portfolio Weights')
			abline(h=0, col = 'red')
			abline(h=10, col = 'red')
			
		plot(100* ef$risk, rowSums(!is.na(y), na.rm = T), pch=20, type='b', 
			xlab='Risk', ylab='# Assets', main='Number of Assets')
	
	}
	
	aa.plot.ef.summary.test(ef.risk)

dev.off()	
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')			

	layout( matrix(1:4, nrow = 2) )
	plot.ef(ia, list(ef.risk, ef.mad), portfolio.risk, F)	
	plot.ef(ia, list(ef.risk, ef.mad), portfolio.mad, F)	

	plot.transition.map(ef.risk)
	plot.transition.map(ef.mad)
		
dev.off()	


	#--------------------------------------------------------------------------
	# Minimum Investment Constraint is 10%
	# Add binary[0/1] variables
	# 0.1 * b <= x.i <= 0.8 * b
	#--------------------------------------------------------------------------
	
	# SUM x.i = 1
	constraints = new.constraints(n,rep(1, n), 1, type = '=')		
	
	# new add binary constraint	
	# adjust prior constraints: add b.i
	constraints = add.variables(n, constraints)
		
	# index of binary variables b.i
	constraints$binary.index = (n+1):(2*n)
		
	# 0.1 * b <= x.i <= 0.8 * b
	# x.i >= 0.1 * b 
	constraints = add.constraints(rbind(diag(n), -0.1 * diag(n)), rep(0, n), type = '>=', constraints)

	# x.i <= 0.8 * b
	constraints = add.constraints(rbind(diag(n), -0.8 * diag(n)), rep(0, n), type = '<=', constraints)
	
	
	# create efficient frontier(s)
	ef.risk = portopt(ia, constraints, 50, 'Risk')
		ef.risk$weight = ef.risk$weight[, 1:n]
	ef.mad = portopt(ia, constraints, 50, 'MAD', min.mad.portfolio)
		ef.mad$weight = ef.mad$weight[, 1:n]
		
png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')			
		
	aa.plot.ef.summary.test(ef.risk)

dev.off()	
png(filename = 'plot4.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')			
	
	
	layout( matrix(1:4, nrow = 2) )
	plot.ef(ia, list(ef.risk, ef.mad), portfolio.risk, F)	
	plot.ef(ia, list(ef.risk, ef.mad), portfolio.mad, F)	

	plot.transition.map(ef.risk)
	plot.transition.map(ef.mad)
	
dev.off()	

	
	#--------------------------------------------------------------------------
	# Limit number of assets to 3
	# Add binary[0/1] variables
	# 0.00001 * b <= x.i <= 0.8 * b
	# SUM b.i = 3
	#--------------------------------------------------------------------------
	
	# SUM x.i = 1
	constraints = new.constraints(n, rep(1, n), 1, type = '=')		
	
	# new add binary constraint	
	# adjust prior constraints: add b.i
	constraints = add.variables(n, constraints)
	
	# index of binary variables b.i
	constraints$binary.index = (n+1):(2*n)
		
	# 0.00001 * b <= x.i <= 0.8 * b
	# x.i >= 0.00001 * b 
	constraints = add.constraints(rbind(diag(n), -0.00001 * diag(n)), rep(0, n), type = '>=', constraints)

	# x.i <= 0.8 * b
	constraints = add.constraints(rbind(diag(n), -0.8 * diag(n)), rep(0, n), type = '<=', constraints)
	
	# SUM b = 3
	constraints = add.constraints(c(rep(0,n), rep(1,n)), 3, type = '=', constraints)
	
	# create efficient frontier(s)
	ef.risk = portopt(ia, constraints, 50, 'Risk')
		ef.risk$weight = ef.risk$weight[, 1:n]
	ef.mad = portopt(ia, constraints, 50, 'MAD', min.mad.portfolio)
		ef.mad$weight = ef.mad$weight[, 1:n]

		
png(filename = 'plot5.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')			
				
	aa.plot.ef.summary.test(ef.risk)		

dev.off()	
png(filename = 'plot6.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')			
		
	layout( matrix(1:4, nrow = 2) )
	plot.ef(ia, list(ef.risk, ef.mad), portfolio.risk, F)	
	plot.ef(ia, list(ef.risk, ef.mad), portfolio.mad, F)	

	plot.transition.map(ef.risk)
	plot.transition.map(ef.mad)
	
dev.off()	
	
}


###############################################################################
# Test AA functions, Average Correlation
# Forecast-Free Algorithms: A New Benchmark For Tactical Strategies
# http://cssanalytics.wordpress.com/2011/08/09/forecast-free-algorithms-a-new-benchmark-for-tactical-strategies/
#
# Follow up FAQ: “Forecast-Free” Algorithms and Minimum Correlation Algorithm
# http://cssanalytics.wordpress.com/2011/08/15/follow-up-faq-forecast-free-algorithms-and-minimum-correlation-algorithm/
###############################################################################
aa.avg.cor.test <- function()
{
	#--------------------------------------------------------------------------
	# Create Efficient Frontier
	#--------------------------------------------------------------------------
	ia = aa.test.create.ia()
	n = ia$n		

	# 0 <= x.i <= 0.8 
	constraints = new.constraints(n, lb = 0, ub = 0.8)
	
	# SUM x.i = 1
	constraints = add.constraints(rep(1, n), 1, type = '=', constraints)		
	

	# create efficient frontier(s)
	ef.risk = portopt(ia, constraints, 50, 'Risk')
	ef.cor.insteadof.cov = portopt(ia, constraints, 50, 'Cor instead of Cov', min.cor.insteadof.cov.portfolio)
	ef.avgcor = portopt(ia, constraints, 50, 'AvgCor', min.avgcor.portfolio)

	
	
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')			
	
	layout(1:2)
	plot.ef(ia, list(ef.risk, ef.avgcor, ef.cor.insteadof.cov), portfolio.risk, F)	
	plot.ef(ia, list(ef.risk, ef.avgcor, ef.cor.insteadof.cov), portfolio.avgcor, F)	
	
dev.off()	
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')			
	
	layout( matrix(1:4, nrow = 2) )
	plot.transition.map(ef.risk)
	plot.transition.map(ef.avgcor)
	plot.transition.map(ef.cor.insteadof.cov)

dev.off()	
png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')			
	
	# visualize input assumptions
	plot.ia(ia)
	
dev.off()		
		

}


###############################################################################
# Test AA functions, CVaR Efficient Frontier
###############################################################################
aa.cvar.test <- function()
{

	#--------------------------------------------------------------------------
	# Create Efficient Frontier
	#--------------------------------------------------------------------------
	ia = aa.test.create.ia()
	n = ia$n		

	# 0 <= x.i <= 0.8 
	constraints = new.constraints(n, lb = 0, ub = 0.8)
	
	# SUM x.i = 1
	constraints = add.constraints(rep(1, n), 1, type = '=', constraints)		
	

# Expected shortfall (CVaR)
# http://www.investopedia.com/articles/04/092904.asp
ia$parameters.alpha = 0.95
	
		
	
	# create efficient frontier(s)
	ef.risk = 		portopt(ia, constraints, 50, 'Risk')
	ef.maxloss = 	portopt(ia, constraints, 50, 'MaxLoss',	min.maxloss.portfolio)
	ef.mad = 		portopt(ia, constraints, 50, 'MAD', 	min.mad.portfolio)
	ef.cvar = 		portopt(ia, constraints, 50, 'CVaR', 	min.cvar.portfolio)
	ef.cdar = 		portopt(ia, constraints, 50, 'CDaR', 	min.cdar.portfolio)


png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')	
	
	layout( matrix(1:4, nrow = 2) )
	plot.ef(ia, list(ef.risk, ef.cvar, ef.cdar), portfolio.risk, F)	
	plot.ef(ia, list(ef.risk, ef.cvar, ef.cdar), portfolio.cvar, F)	
	plot.ef(ia, list(ef.risk, ef.cvar, ef.cdar), portfolio.cdar, F)	

dev.off()	
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')	

	layout( matrix(1:4, nrow = 2) )
	plot.transition.map(ef.risk)
	plot.transition.map(ef.cvar)
	plot.transition.map(ef.cdar)

dev.off()

	return()
	

	
	layout( matrix(1:4, nrow = 2) )
	plot.ef(ia, list(ef.risk, ef.maxloss, ef.mad, ef.cvar, ef.cdar), portfolio.risk, F)
	plot.ef(ia, list(ef.risk, ef.maxloss, ef.mad, ef.cvar, ef.cdar), portfolio.maxloss, F)
	plot.ef(ia, list(ef.risk, ef.maxloss, ef.mad, ef.cvar, ef.cdar), portfolio.cvar, F)
	plot.ef(ia, list(ef.risk, ef.maxloss, ef.mad, ef.cvar, ef.cdar), portfolio.cdar, F)
	
		
	layout( matrix(1:4, nrow = 2) )
	plot.transition.map(ef.maxloss)
	plot.transition.map(ef.mad)	
	plot.transition.map(ef.cvar)
	plot.transition.map(ef.cdar)
		

}

###############################################################################
# Test AA functions, Omega Efficient Frontier
###############################################################################
aa.omega.test <- function()
{
	#--------------------------------------------------------------------------
	# Create Efficient Frontier
	#--------------------------------------------------------------------------
	ia = aa.test.create.ia()
	n = ia$n		

	# 0 <= x.i <= 0.8 
	constraints = new.constraints(n, lb = 0, ub = 0.8)
	
	# SUM x.i = 1
	constraints = add.constraints(rep(1, n), 1, type = '=', constraints)		
	
	# Omega - http://en.wikipedia.org/wiki/Omega_ratio
	ia$parameters.omega = 13/100 
		ia$parameters.omega = 12/100 
		# convert annual to monthly
		ia$parameters.omega = ia$parameters.omega / 12


	# create efficient frontier(s)
	ef.risk = portopt(ia, constraints, 50, 'Risk')

png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')			
	
	# Plot Omega Efficient Frontiers and Transition Maps
	layout( matrix(1:4, nrow = 2, byrow=T) )
	
	# weights
	rownames(ef.risk$weight) = paste('Risk','weight',1:50,sep='_')
	plot.omega(ef.risk$weight[c(1,10,40,50), ], ia)
	
	# assets
	temp = diag(n)
	rownames(temp) = ia$symbols
	plot.omega(temp, ia)
		
	# portfolio
	plot.ef(ia, list(ef.risk), portfolio.omega, T, T)			

dev.off()	
		
	#--------------------------------------------------------------------------
	# Create Efficient Frontier in Omega Ratio framework
	#--------------------------------------------------------------------------
		
	# Create maximum Omega Efficient Frontier
	ef.omega = portopt.omega(ia, constraints, 50, 'Omega')
	

png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')		
	
	# Plot Omega Efficient Frontiers and Transition Maps
	layout( matrix(1:4, nrow = 2, byrow=T) )

	# weights
	plot.omega(ef.risk$weight[c(1,10,40,50), ], ia)

	# weights
	rownames(ef.omega$weight) = paste('Omega','weight',1:50,sep='_')	
	plot.omega(ef.omega$weight[c(1,10,40,50), ], ia)
		
	# portfolio
	plot.ef(ia, list(ef.omega, ef.risk), portfolio.omega, T, T)			
		
dev.off()	
png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')			
	
	# Plot multiple Efficient Frontiers and Transition Maps
	layout( matrix(1:4, nrow = 2) )
	plot.ef(ia, list(ef.risk,ef.omega), portfolio.risk, F)			
	plot.ef(ia, list(ef.risk,ef.omega), portfolio.omega, F)			

	plot.transition.map(ef.risk)
	plot.transition.map(ef.omega)
	
dev.off()	
				
}


###############################################################################
# Test AA functions, Downside Risk
###############################################################################
aa.downside.test <- function()
{
	#--------------------------------------------------------------------------
	# Create Efficient Frontier
	#--------------------------------------------------------------------------
	ia = aa.test.create.ia()
	n = ia$n		

	# 0 <= x.i <= 0.8
	constraints = new.constraints(n, lb = 0, ub = 0.8)

	# SUM x.i = 1
	constraints = add.constraints(rep(1, n), 1, type = '=', constraints)		

	# Set target return (or Minimum Acceptable Returns (MAR))
	# and consider only returns that are less than the target 
	ia$parameters.mar = 0/100 
		# convert annual to monthly
		ia$parameters.mar = ia$parameters.mar / 12

		
	# create efficient frontier(s)
	ef.mad = portopt(ia, constraints, 50, 'MAD', min.mad.portfolio)
	ef.mad.downside = portopt(ia, constraints, 50, 'S-MAD', min.mad.downside.portfolio)
	
	ef.risk = portopt(ia, constraints, 50, 'Risk')
	ef.risk.downside = portopt(ia, constraints, 50, 'S-Risk', min.risk.downside.portfolio)
	
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')		

	# Plot multiple Efficient Frontiers and Transition Maps
	layout( matrix(1:4, nrow = 2) )
	plot.ef(ia, list(ef.mad.downside, ef.mad), portfolio.mad, F)			
	plot.ef(ia, list(ef.mad.downside, ef.mad), portfolio.mad.downside, F)			
		
	plot.transition.map(ef.mad)
	plot.transition.map(ef.mad.downside)

dev.off()
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')		
	
	# Plot multiple Efficient Frontiers and Transition Maps
	layout( matrix(1:4, nrow = 2) )
	plot.ef(ia, list(ef.risk.downside, ef.risk), portfolio.risk, F)			
	plot.ef(ia, list(ef.risk.downside, ef.risk), portfolio.risk.downside, F)			

	plot.transition.map(ef.risk)
	plot.transition.map(ef.risk.downside)

dev.off()		
}

	
###############################################################################
# Test AA functions, Multiple Risk Measures Efficient Frontier
###############################################################################
aa.multiple.risk.measures.test <- function()
{
	# Following linear risk constraints are implemented
	# add.constraint.maxloss
	# add.constraint.mad
	# add.constraint.cvar
	# add.constraint.cdar
	

	#--------------------------------------------------------------------------
	# Create Efficient Frontier
	#--------------------------------------------------------------------------
	ia = aa.test.create.ia()
	n = ia$n		

	# 0 <= x.i <= 0.8 
	constraints = new.constraints(n, lb = 0, ub = 0.8)
	
	# SUM x.i = 1
	constraints = add.constraints(rep(1, n), 1, type = '=', constraints)		
	
		
	# create efficient frontier(s)
	ef.risk = 		portopt(ia, constraints, 50, 'Risk')
	ef.maxloss = 	portopt(ia, constraints, 50, 'MaxLoss',	min.maxloss.portfolio)

png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')		

	layout( matrix(1:4, nrow = 2) )
	plot.ef(ia, list(ef.risk, ef.maxloss), portfolio.risk, F)	
	plot.ef(ia, list(ef.risk, ef.maxloss), portfolio.maxloss, F)	

	plot.transition.map(ef.risk)
	plot.transition.map(ef.maxloss)

dev.off()
	
	#--------------------------------------------------------------------------
	# Add MaxLoss <= 12 constraint
	#--------------------------------------------------------------------------
	
	constraints = add.constraint.maxloss(ia, 12/100, '<=', constraints)	
		
	ef.risk.maxloss = 		portopt(ia, constraints, 50, 'Risk+MaxLoss')
		ef.risk.maxloss$weight = ef.risk.maxloss$weight[, 1:n]
	
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')		

	layout( matrix(1:4, nrow = 2) )
	plot.ef(ia, list(ef.risk.maxloss, ef.risk, ef.maxloss), portfolio.risk, F)	
	plot.ef(ia, list(ef.risk.maxloss, ef.risk, ef.maxloss), portfolio.maxloss, F)	

	plot.transition.map(ef.risk)
	plot.transition.map(ef.risk.maxloss)

dev.off()
	
	return()		
	
	
	
	#--------------------------------------------------------------------------
	# Other Examples
	#--------------------------------------------------------------------------
	
	# constraints
	constraints = new.constraints(n, lb = 0, ub = 0.8)
	constraints = add.constraints(rep(1, n), 1, type = '=', constraints)			
	
	# Alpha for CVaR and DVar
	ia$parameters.alpha = 0.95
				
	# create efficient frontier(s)
	ef.risk = 		portopt(ia, constraints, 50, 'Risk')
	ef.maxloss = 	portopt(ia, constraints, 50, 'MaxLoss',	min.maxloss.portfolio)
	ef.mad = 		portopt(ia, constraints, 50, 'MAD', 	min.mad.portfolio)
	ef.cvar = 		portopt(ia, constraints, 50, 'CVaR', 	min.cvar.portfolio)
	ef.cdar = 		portopt(ia, constraints, 50, 'CDaR', 	min.cdar.portfolio)
	
	
	#--------------------------------------------------------------------------
	# Limit Max Loss
	#--------------------------------------------------------------------------
	layout(1)
	plot.ef(ia, list(ef.risk, ef.maxloss), portfolio.maxloss, F)
	
	# constraints
	constraints = new.constraints(n, lb = 0, ub = 0.8)
	constraints = add.constraints(rep(1, n), 1, type = '=', constraints)			
	constraints = add.constraint.maxloss(ia, 15/100, '<=', constraints)	
	
	ef.risk.new = portopt(ia, constraints, 50, 'Risk+')
		ef.risk.new$weight = ef.risk.new$weight[, 1:n]
	
	# 3. compare new ef	
	layout(1:2)
	plot.ef(ia, list(ef.risk), portfolio.maxloss, F)
	plot.ef(ia, list(ef.risk.new), portfolio.maxloss, F)
	
	layout( matrix(1:4, nrow = 2) )
	plot.ef(ia, list(ef.risk.new, ef.risk,ef.maxloss), portfolio.maxloss, F)	
	plot.ef(ia, list(ef.risk.new, ef.risk, ef.maxloss), portfolio.risk, F)
	plot.transition.map(ef.risk)
	plot.transition.map(ef.risk.new)
	
	#--------------------------------------------------------------------------
	# Limit MAD
	#--------------------------------------------------------------------------
	layout(1)
	plot.ef(ia, list(ef.risk, ef.mad), portfolio.mad, F)
	
	# constraints
	constraints = new.constraints(n, lb = 0, ub = 0.8)
	constraints = add.constraints(rep(1, n), 1, type = '=', constraints)				
	constraints = add.constraint.mad(ia, 2.9/100, '<=', constraints)	
	
	ef.risk.new = portopt(ia, constraints, 50, 'Risk+')
		ef.risk.new$weight = ef.risk.new$weight[, 1:n]
	
	# 3. compare new ef	
	layout(1:2)
	plot.ef(ia, list(ef.risk), portfolio.mad, F)
	plot.ef(ia, list(ef.risk.new), portfolio.mad, F)
	
	layout( matrix(1:4, nrow = 2) )
	plot.ef(ia, list(ef.risk.new, ef.risk,ef.mad), portfolio.mad, F)	
	plot.ef(ia, list(ef.risk.new, ef.risk, ef.mad), portfolio.risk, F)
	plot.transition.map(ef.risk)
	plot.transition.map(ef.risk.new)
	
	#--------------------------------------------------------------------------
	# Limit CVaR
	#--------------------------------------------------------------------------
	layout(1)
	plot.ef(ia, list(ef.risk, ef.cvar), portfolio.cvar, F)
	
	# constraints
	constraints = new.constraints(n, lb = 0, ub = 0.8)
	constraints = add.constraints(rep(1, n), 1, type = '=', constraints)					
	constraints = add.constraint.cvar(ia, 8/100, '<=', constraints)	
	
	ef.risk.new = portopt(ia, constraints, 50, 'Risk+')
		ef.risk.new$weight = ef.risk.new$weight[, 1:n]
	
	# 3. compare new ef	
	layout(1:2)
	plot.ef(ia, list(ef.risk), portfolio.cvar, F)
	plot.ef(ia, list(ef.risk.new), portfolio.cvar, F)
	
	layout( matrix(1:4, nrow = 2) )
	plot.ef(ia, list(ef.risk.new, ef.risk,ef.cvar), portfolio.cvar, F)	
	plot.ef(ia, list(ef.risk.new, ef.risk, ef.cvar), portfolio.risk, F)
	plot.transition.map(ef.risk)
	plot.transition.map(ef.risk.new)

	#--------------------------------------------------------------------------
	# Limit CVaR
	#--------------------------------------------------------------------------
	layout(1)
	plot.ef(ia, list(ef.risk, ef.cdar), portfolio.cdar, F)
	
	# constraints
	constraints = new.constraints(n, lb = 0, ub = 0.8)
	constraints = add.constraints(rep(1, n), 1, type = '=', constraints)						
	constraints = add.constraint.cdar(ia, 15/100, '<=', constraints)	
	
	ef.risk.new = portopt(ia, constraints, 50, 'Risk+')
		ef.risk.new$weight = ef.risk.new$weight[, 1:n]
	
	# 3. compare new ef	
	layout(1:2)
	plot.ef(ia, list(ef.risk), portfolio.cdar, F)
	plot.ef(ia, list(ef.risk.new), portfolio.cdar, F)
	
	layout( matrix(1:4, nrow = 2) )
	plot.ef(ia, list(ef.risk.new, ef.risk,ef.cdar), portfolio.cdar, F)	
	plot.ef(ia, list(ef.risk.new, ef.risk, ef.cdar), portfolio.risk, F)
	plot.transition.map(ef.risk)
	plot.transition.map(ef.risk.new)


	#--------------------------------------------------------------------------
	# Limit both Max Loss and CDaR
	#--------------------------------------------------------------------------
	layout(1:2)
	plot.ef(ia, list(ef.risk, ef.maxloss), portfolio.maxloss, F)
	plot.ef(ia, list(ef.risk, ef.cdar), portfolio.cdar, F)

	# constraints
	constraints = new.constraints(n, lb = 0, ub = 0.8)
	constraints = add.constraints(rep(1, n), 1, type = '=', constraints)							
	constraints = add.constraint.maxloss(ia, 15/100, '<=', constraints)		
	constraints = add.constraint.cdar(ia, 15/100, '<=', constraints)	
	
	ef.risk.new = portopt(ia, constraints, 50, 'Risk+')
		ef.risk.new$weight = ef.risk.new$weight[, 1:n]
	
	# 3. compare new ef	
	layout( matrix(1:4, nrow = 2) )
	plot.ef(ia, list(ef.risk), portfolio.maxloss, F)
	plot.ef(ia, list(ef.risk.new), portfolio.maxloss, F)
	plot.ef(ia, list(ef.risk), portfolio.cdar, F)
	plot.ef(ia, list(ef.risk.new), portfolio.cdar, F)


	layout( matrix(1:4, nrow = 2) )
	plot.ef(ia, list(ef.risk.new, ef.risk, ef.maxloss, ef.cdar), portfolio.maxloss, F)		
	plot.ef(ia, list(ef.risk.new, ef.risk, ef.maxloss, ef.cdar), portfolio.cdar, F)	
	plot.ef(ia, list(ef.risk.new, ef.risk, ef.maxloss, ef.cdar), portfolio.risk, F)
	
	layout( matrix(1:4, nrow = 2) )
	plot.transition.map(ef.risk)
	plot.transition.map(ef.risk.new)
	plot.transition.map(ef.maxloss)
	plot.transition.map(ef.cdar)


}


###############################################################################
# Test AA functions, Arithmetic vs Geometric Efficient Frontier
###############################################################################
aa.arithmetic.geometric.test <- function()
{
	#--------------------------------------------------------------------------
	# Create Efficient Frontier
	#--------------------------------------------------------------------------
	ia = aa.test.create.ia.rebal()
	n = ia$n		

	# -1 <= x.i <= 1
	constraints = new.constraints(n, lb = 0, ub = 1)

	# SUM x.i = 1
	constraints = add.constraints(rep(1, n), 1, type = '=', constraints)		
	
	# create efficient frontier(s)
	ef.risk = portopt(ia, constraints, 50, 'Arithmetic', equally.spaced.risk = T)	
	
	# compute historical geometrical returns
	ef.risk.geometric = ef.risk
		ef.risk.geometric$name = 'Geometric'
		ef.risk.geometric$return = portfolio.geometric.return(ef.risk$weight, ia)		
		
		
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')	
				
	# Plot multiple Efficient Frontiers and Transition Maps
	plot.ef(ia, list(ef.risk, ef.risk.geometric), portfolio.risk, T)			

dev.off()	
	

	#--------------------------------------------------------------------------
	# Following DIVERSIFICATION, REBALANCING, AND THE GEOMETRIC MEAN FRONTIER by W. Bernstein and D. Wilkinson (1997)
	# paper's notation : A(1,0) and A(1,1) page 8, 14
	#--------------------------------------------------------------------------
	# A(1,0)
	ef.risk.A10 = ef.risk
		ef.risk.A10$name = 'A(1;0)'
		ef.risk.A10$return = apply( cbind(ef.risk$return, ef.risk$risk), 1, 
								function(x) aritm2geom(x[1], x[2], 1, 0) )
	# A(1,1)
	ef.risk.A11 = ef.risk
		ef.risk.A11$name = 'A(1;1)'
		ef.risk.A11$return = apply( cbind(ef.risk$return, ef.risk$risk), 1, 
								function(x) aritm2geom(x[1], x[2], 1, 1) )
							
	# G(1,0)
	ia.G = ia
	ia.G$expected.return = apply( cbind(ia$geometric.return, ia$risk), 1, 
								function(x) geom2aritm(x[1], x[2], 1, 0) )
	ef.risk.G10 = portopt(ia.G, constraints, 50, 'G(1;0)',equally.spaced.risk = T)	
		ef.risk.G10$return = apply( cbind(ef.risk.G10$return, ef.risk.G10$risk), 1, 
								function(x) aritm2geom(x[1], x[2], 1, 0) )
	# G(1,1)
	ia.G$expected.return = apply( cbind(ia$geometric.return, ia$risk), 1, 
								function(x) geom2aritm(x[1], x[2], 1, 1) )
	ef.risk.G11 = portopt(ia.G, constraints, 50, 'G(1;1)',equally.spaced.risk = T)	
		ef.risk.G11$return = apply( cbind(ef.risk.G11$return, ef.risk.G11$risk), 1, 
								function(x) aritm2geom(x[1], x[2], 1, 1) )
									
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')	

	# Plot multiple Efficient Frontiers
	layout( matrix(1:4, nrow = 2) )
	plot.ef(ia, list(ef.risk, ef.risk.geometric, ef.risk.A10), portfolio.risk, F)
	plot.ef(ia, list(ef.risk, ef.risk.geometric, ef.risk.A11), portfolio.risk, F)
	plot.ef(ia, list(ef.risk, ef.risk.geometric, ef.risk.G10), portfolio.risk, F)
	plot.ef(ia, list(ef.risk, ef.risk.geometric, ef.risk.G11), portfolio.risk, F)
	
dev.off()		

	#--------------------------------------------------------------------------
	# Use A4 method to convert between Arithmetic and Geometric means
	#--------------------------------------------------------------------------
	# A	
	ef.risk.A4 = ef.risk
		ef.risk.A4$name = 'Risk A4'
		ef.risk.A4$return = apply( cbind(ef.risk$return, ef.risk$risk), 1, 
								function(x) aritm2geom4(x[1], x[2]) )

	# G
	ia.G = ia
	ia.G$expected.return = apply( cbind(ia$geometric.return, ia$risk), 1, 
								function(x) geom2aritm4(x[1], x[2]) )
	ef.risk.G4 = portopt(ia.G, constraints, 50, 'Risk G4',equally.spaced.risk = T)	
		ef.risk.G4$return = apply( cbind(ef.risk.G4$return, ef.risk.G4$risk), 1, 
								function(x) aritm2geom4(x[1], x[2]) )

png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')	
								
	# Plot multiple Efficient Frontiers						
	layout( matrix(1:2, nrow = 2) )
	plot.ef(ia, list(ef.risk, ef.risk.geometric, ef.risk.A4), portfolio.risk, F)
	plot.ef(ia, list(ef.risk, ef.risk.geometric, ef.risk.G4), portfolio.risk, F)

dev.off()		
	
	#--------------------------------------------------------------------------
	# Create True Geometric Efficient Frontier
	#--------------------------------------------------------------------------
	ef.true.geometric = ef.risk
		ef.true.geometric$name = 'True Geometric'
		constraints$x0 = ef.risk$weight[1,]

	for(i in 1:len(ef.risk$risk)) {
		cat('i =', i, '\n')
		ef.true.geometric$weight[i,] = max.geometric.return.portfolio(ia, constraints, ef.risk$risk[i], ef.risk$risk[i])
			constraints$x0 = ef.true.geometric$weight[i,]
	}
	
	ef.true.geometric$return = portfolio.geometric.return(ef.true.geometric$weight, ia)		

png(filename = 'plot4.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')	
		
	# Plot multiple Efficient Frontiers						
	layout( matrix(1:4, nrow = 2) )
	plot.ef(ia, list(ef.risk.geometric, ef.risk, ef.true.geometric), portfolio.risk, T, T)			
	plot.ef(ia, list(ef.true.geometric, ef.risk, ef.risk.geometric), portfolio.risk, T, T)			
	
dev.off()			
	
	#--------------------------------------------------------------------------
	# Double check that NonLinear Optimization finds global maximums by
	# creating random portfolios that satisfy constraints. 
	# Plot True Geometric Efficient Frontier and random portfolios, check
	# that all portfolios lie below the efficient frontier.
	#--------------------------------------------------------------------------	
	# Generate random portfolios
	ef.random = list()
		ef.random$name = 'Random'
		ef.random$weight = randfixedsum(1000000, n, 1, 0, 1)
		
		ef.random$risk = portfolio.risk(ef.random$weight, ia)		
		ef.random$return = portfolio.geometric.return(ef.random$weight, ia)		
		
		
png(filename = 'plot5.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')	
		
	# Plot True Geometric Efficient Frontier and random portfolios
	layout(1)
	plot(100*ef.random$risk, 100*ef.random$return, type='p', pch=20,
			xlim = 100*range(ef.random$risk, ef.true.geometric$risk),
			ylim = 100*range(ef.random$return, ef.true.geometric$return),
			main = 'True Geometric Efficient Frontier vs Random Portfolios',
			xlab = 'portfolio.risk',
			ylab = 'Return'			
		)
	lines(100*ef.true.geometric$risk, 100*ef.true.geometric$return, type='l', lwd=2,col = 'red')
		
dev.off()		

		
	# find max Geom Mean portfolio	
	#x=max.geometric.return.portfolio(ia, constraints, 0, 1)
	#constraints$x0 = x
	#x=max.geometric.return.portfolio(ia, constraints, 0.4, 1)	
	#lines( portfolio.risk(t(x), ia), portfolio.geometric.return(t(x), ia), type='p', pch=20, col = 'blue')
}





###############################################################################
# Create Input Assumptions used in 
# DIVERSIFICATION, REBALANCING, AND THE GEOMETRIC MEAN FRONTIER by W. Bernstein and D. Wilkinson (1997)
# www.effisols.com/basics/rebal.pdf
###############################################################################
aa.test.create.ia.rebal <- function()
{
	symbols = spl('SP500	SmallUS   	Europe	Pacific	Japan  	Gold   	20Y_Treas	5Y_Treas	TBills', '\t')
	
	data = 
'1970	0.0403	-0.1743	-0.0935	-0.13	-0.156	0.0871	0.121	0.1685	0.0652
1971	0.1432	0.165	0.2803	0.1082	0.6107	-0.0373	0.1324	0.0874	0.0439
1972	0.1898	0.0443	0.1582	0.6678	1.1447	0.602	0.0567	0.0517	0.0384
1973	-0.1466	-0.309	-0.0773	-0.2392	-0.1595	0.9184	-0.011	0.0461	0.0693
1974	-0.2647	-0.1995	-0.2277	-0.4059	-0.1392	0.1094	0.0435	0.0568	0.0801
1975	0.372	0.5282	0.439	0.6342	0.1723	-0.2407	0.0919	0.0782	0.058
1976	0.2384	0.5738	-0.0637	0.0572	0.2637	-0.3258	0.1676	0.1288	0.0508
1977	-0.0718	0.2538	0.2392	0.0334	0.1722	0.3549	-0.0065	0.014	0.0513
1978	0.0656	0.2346	0.243	0.2397	0.5182	0.0934	-0.0118	0.0349	0.072
1979	0.1844	0.4346	0.1467	0.5216	-0.1461	1.6133	-0.0121	0.041	0.1038
1980	0.3242	0.3988	0.1452	0.6149	0.2939	0.6427	-0.0396	0.039	0.1126
1981	-0.0491	0.1388	-0.1045	-0.1547	0.1041	-0.2514	0.0186	0.0944	0.1472
1982	0.2141	0.2801	0.0569	-0.2818	-0.0023	0.4786	0.4037	0.291	0.1053
1983	0.2251	0.3967	0.2238	0.3421	0.2779	0.0259	0.0069	0.0741	0.088
1984	0.0623	-0.0667	0.0126	-0.0724	0.1701	0.2922	0.1554	0.1403	0.0978
1985	0.3216	0.2466	0.7979	0.1729	0.4413	-0.0887	0.3096	0.2034	0.0773
1986	0.1847	0.0685	0.4446	0.4839	0.9185	0.3593	0.2445	0.1513	0.0615
1987	0.0523	-0.093	0.041	0.042	0.4187	0.3753	-0.027	0.029	0.0546
1988	0.1681	0.2287	0.1635	0.3056	0.3534	-0.1846	0.0968	0.0609	0.0636
1989	0.3149	0.1018	0.2906	0.1585	0.0217	0.2538	0.181	0.1327	0.0838
1990	-0.0317	-0.2156	-0.0337	-0.1015	-0.3618	-0.2373	0.062	0.0974	0.0782
1991	0.3055	0.4463	0.1366	0.3661	0.0882	-0.042	0.1926	0.1531	0.056
1992	0.0766	0.2335	-0.0425	0.0701	-0.2111	-0.1598	0.0941	0.072	0.0351
1993	0.099	0.21	0.2979	0.8035	0.2505	0.8287	0.1824	0.1124	0.029
1994	0.012	0.031	0.0266	-0.141	0.2217	-0.1193	-0.0778	-0.0513	0.0391
1995	0.3753	0.3448	0.2213	0.1295	0.0069	0.0191	0.3069	0.1905	0.0551
1996	0.2295	0.1765	0.2895	0.2054	-0.155	0.0706	-0.0127	0.0661	0.0502'

	hist.returns = matrix( as.double(spl( gsub('\n', '\t', data), '\t')), 
				nrow = len(spl(data, '\n')), byrow=TRUE)
				
				
	load.packages('quantmod')
	symbol.names = symbols
	
	hist.returns = as.xts( hist.returns[,-1] , 
							as.Date(paste('1/1/', hist.returns[,1], sep=''), '%d/%m/%Y')
						) 
	colnames(hist.returns) = symbols
	
	#--------------------------------------------------------------------------
	# Create historical input assumptions
	#--------------------------------------------------------------------------
		
	# setup input assumptions
	ia = list()
	ia$symbols = symbols
	ia$symbol.names = symbol.names
	ia$n = len(symbols)
	ia$hist.returns = hist.returns
	ia$annual.factor = 1
	
	# compute historical returns, risk, and correlation
	ia$arithmetic.return = apply(hist.returns, 2, mean, na.rm = T)
	ia$geometric.return = apply(hist.returns, 2, function(x) prod(1+x)^(1/len(x))-1 )
		
	ia$risk = apply(hist.returns, 2, sd, na.rm = T)
	# use N instead of N-1 in computation of variance
	# ia$risk = apply(hist.returns, 2, function(x) sqrt(sum((x-mean(x))^2)/len(x)) )
	
	ia$correlation = cor(hist.returns, use = 'complete.obs', method = 'pearson')			
	
	ia$cov = ia$cor * (ia$risk %*% t(ia$risk))		
	
	ia$expected.return = ia$arithmetic.return
	
	return(ia)
}



###############################################################################
# Create Input Assumptions used in aa.test functions
###############################################################################
aa.test.create.ia <- function()
{
	#--------------------------------------------------------------------------
	# Load historical prices and compute simple returns
	#--------------------------------------------------------------------------
	load.packages('quantmod,quadprog')

	# load historical prices from Yahoo Finance
	symbols = spl('SPY,QQQ,EEM,IWM,EFA,TLT,IYR,GLD')	
	symbol.names = spl('S&P 500,Nasdaq 100,Emerging Markets,Russell 2000,EAFE,20 Year Treasury,U.S. Real Estate,Gold')
	
	getSymbols(symbols, from = '1980-01-01', auto.assign = TRUE)
			
	# align dates for all symbols & convert to monthly 
	hist.prices = merge(SPY,QQQ,EEM,IWM,EFA,TLT,IYR,GLD)		
		month.ends = endpoints(hist.prices, 'months')
		hist.prices = Ad(hist.prices)[month.ends, ]
		colnames(hist.prices) = symbols
		
	# remove any missing data	
	hist.prices = na.omit(hist.prices['1995::2010'])
	
	# compute simple returns	
	hist.returns = na.omit( ROC(hist.prices, type = 'discrete') )
	
	#--------------------------------------------------------------------------
	# Create historical input assumptions
	#--------------------------------------------------------------------------
		
	# setup input assumptions
	ia = list()
	ia$symbols = symbols
	ia$symbol.names = symbol.names
	ia$n = len(symbols)
	ia$hist.returns = hist.returns
	
	# compute historical returns, risk, and correlation
	ia$arithmetic.return = apply(hist.returns, 2, mean, na.rm = T)
	ia$geometric.return = apply(hist.returns, 2, function(x) prod(1+x)^(1/len(x))-1 )
	
	ia$risk = apply(hist.returns, 2, sd, na.rm = T)
	ia$correlation = cor(hist.returns, use = 'complete.obs', method = 'pearson')			
	
		# convert to annual, year = 12 months
		ia$annual.factor = 12
		
		#ia$arithmetic.return = ia$annual.factor * ia$arithmetic.return
		ia$arithmetic.return = (1 + ia$arithmetic.return)^ia$annual.factor - 1
		
		ia$geometric.return = (1 + ia$geometric.return)^ia$annual.factor - 1
		ia$risk = sqrt(ia$annual.factor) * ia$risk

		# compute covariance matrix
		ia$risk = iif(ia$risk == 0, 0.000001, ia$risk)
	ia$cov = ia$cor * (ia$risk %*% t(ia$risk))		
	
	ia$expected.return = ia$arithmetic.return
	
	return(ia)
}

###############################################################################
# Add short (negative copy) input assumptions to given ia
###############################################################################
aa.test.ia.add.short <- function(ia)
{
	ia$symbols = c(ia$symbols,ia$symbols)
	ia$n = 2*ia$n
	ia$hist.returns = cbind(ia$hist.returns, -ia$hist.returns)
	
	ia$expected.return = c(ia$expected.return, -ia$expected.return)
	ia$risk = c(ia$risk, ia$risk)

	# Transfrom correlation & covariance
	# | cov -cov |
	# |-cov  cov |		
	ia$correlation = cbind( rbind(ia$correlation, -ia$correlation), rbind(-ia$correlation, ia$correlation) )	
	ia$cov = cbind( rbind(ia$cov, -ia$cov), rbind(-ia$cov, ia$cov) )
	
	return(ia)
}


	