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

	# x.i >= 0 
	constraints = new.constraints(diag(n), rep(0, n), type = '>=')

	# x.i <= 0.8 
	constraints = add.constraints(diag(n), rep(0.8, n), type = '<=', constraints)
	
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
	plot.transitopn.map(ef.risk)
	plot.transitopn.map(ef.maxloss)
	plot.transitopn.map(ef.mad)

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

	# x.i >= -0.5 
	constraints = new.constraints(diag(n), rep(-0.5, n), type = '>=')

	# x.i <= 0.8 
	constraints = add.constraints(diag(n), rep(0.8, n), type = '<=', constraints)
	
	# SUM x.i = 1
	constraints = add.constraints(rep(1, n), 1, type = '=', constraints)		
	
	# create efficient frontier(s)
	ef.risk = portopt(ia, constraints, 50, 'Risk')
	ef.mad = portopt(ia, constraints, 50, 'MAD', min.mad.portfolio)
		
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')	
	
	layout( matrix(1:4, nrow = 2) )
	plot.ef(ia, list(ef.risk, ef.mad), portfolio.risk, F)	
	plot.ef(ia, list(ef.risk, ef.mad), portfolio.mad, F)	

	plot.transitopn.map(ef.risk)
	plot.transitopn.map(ef.mad)
	
dev.off()

	#--------------------------------------------------------------------------
	# Create 130:30
	# -v.i <= x.i <= v.i, v.i>0, SUM(v.i) = 1.6
	#--------------------------------------------------------------------------

	# x.i >= -0.5 
	constraints = new.constraints(diag(n), rep(-0.5, n), type = '>=')

	# x.i <= 0.8 
	constraints = add.constraints(diag(n), rep(0.8, n), type = '<=', constraints)
	
	# SUM x.i = 1
	constraints = add.constraints(rep(1, n), 1, type = '=', constraints)		

	# adjust prior constraints, add v.i
	constraints$A = rbind( constraints$A, matrix(0, n, ncol(constraints$A)) )

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

	plot.transitopn.map(ef.risk)
	plot.transitopn.map(ef.mad)
		
dev.off()

	#--------------------------------------------------------------------------
	# Create 130:30
	# Split x into x.long and x.short, x_long and x_short >= 0
	# SUM(x.long) - SUM(x.short) = 1.6
	#--------------------------------------------------------------------------
	ia.ls = aa.test.ia.add.short(ia)
	
	# x.long and x.short >= 0
	constraints = new.constraints(diag(2*n), rep(0, 2*n), type = '>=')

	# x.long <= 0.8 
	constraints = add.constraints(rbind(diag(n),0*diag(n)), rep(0.8, n), type = '<=', constraints)
			
	# x.short <= 0.5 
	constraints = add.constraints(rbind(0*diag(n),diag(n)), rep(0.5, n), type = '<=', constraints)
	
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

	plot.transitopn.map(ef.risk)
	plot.transitopn.map(ef.mad)
	
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
	constraints = new.constraints(diag(2*n), rep(0, 2*n), type = '>=')

	# x.long <= 0.8 
	constraints = add.constraints(rbind(diag(n),0*diag(n)), rep(0.8, n), type = '<=', constraints)
			
	# x.short <= 0.5 
	constraints = add.constraints(rbind(0*diag(n),diag(n)), rep(0.5, n), type = '<=', constraints)
	
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

	plot.transitopn.map(ef.risk)
	plot.transitopn.map(ef.mad)
	
dev.off()

	#--------------------------------------------------------------------------
	# Create 200:100 using binary[0/1] variables and Branch and Bound algorithm
	# Split x into x.long and x.short, x_long and x_short >= 0
	# SUM(x.long) - SUM(x.short) = 3
	#
	# Solve using branch and bound: add a binary var b1:bn, xL < b, xS < (1-b)
	#--------------------------------------------------------------------------
		
	# x.long and x.short >= 0
	constraints = new.constraints(diag(2*n), rep(0, 2*n), type = '>=')

	# x.long <= 0.8 
	constraints = add.constraints(rbind(diag(n),0*diag(n)), rep(0.8, n), type = '<=', constraints)
			
	# x.short <= 0.5 
	constraints = add.constraints(rbind(0*diag(n),diag(n)), rep(0.5, n), type = '<=', constraints)
	
	# SUM (x.long - x.short) = 1
	constraints = add.constraints(c(rep(1,n), -rep(1,n)), 1, type = '=', constraints)		

	# SUM (x.long + x.short) = 3
	constraints = add.constraints(c(rep(1,n), rep(1,n)), 3, type = '=', constraints)		
				
	# new add binary constraint	
	# adjust prior constraints: add b.i
	constraints$A = rbind( constraints$A, matrix(0, n, ncol(constraints$A)) )
	
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

	plot.transitopn.map(ef.risk)
	plot.transitopn.map(ef.mad)
	
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

	# x.i >= 0 
	constraints = new.constraints(diag(n), rep(0, n), type = '>=')

	# x.i <= 0.8 
	constraints = add.constraints(diag(n), rep(0.8, n), type = '<=', constraints)
	
	# SUM x.i = 1
	constraints = add.constraints(rep(1, n), 1, type = '=', constraints)		
	
	# create efficient frontier(s)
	ef.risk = portopt(ia, constraints, 50, 'Risk')
	ef.mad = portopt(ia, constraints, 50, 'MAD', min.mad.portfolio)

png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')			

	layout(1:2)
	par(mar = c(4,4,2,1), cex = 0.8)
	plot(as.vector(sort(100*ef.risk$weight)), pch=20, 
		xlab='', ylab='Weight', main='Portfolio Weights')
	plot(ef.risk$risk, rowSums(ef.risk$weight > 0.000001), pch=20, type='b', 
		xlab='Risk', ylab='# Assets', main='Number of Assets')

dev.off()	
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')			

	layout( matrix(1:4, nrow = 2) )
	plot.ef(ia, list(ef.risk, ef.mad), portfolio.risk, F)	
	plot.ef(ia, list(ef.risk, ef.mad), portfolio.mad, F)	

	plot.transitopn.map(ef.risk)
	plot.transitopn.map(ef.mad)
		
dev.off()	


	#--------------------------------------------------------------------------
	# Minimum Investment Constraint is 5%
	# Add binary[0/1] variables
	# 0.05 * b <= x.i <= 0.8 * b
	#--------------------------------------------------------------------------
	
	# SUM x.i = 1
	constraints = new.constraints(rep(1, n), 1, type = '=')		
	
	# new add binary constraint	
	# adjust prior constraints: add b.i
	constraints$A = rbind( constraints$A, matrix(0, n, ncol(constraints$A)) )
	
	# index of binary variables b.i
	constraints$binary.index = (n+1):(2*n)
		
	# 0.05 * b <= x.i <= 0.8 * b
	# x.i >= 0.05 * b 
	constraints = add.constraints(rbind(diag(n), -0.05 * diag(n)), rep(0, n), type = '>=', constraints)

	# x.i <= 0.8 * b
	constraints = add.constraints(rbind(diag(n), -0.8 * diag(n)), rep(0, n), type = '<=', constraints)
	
	
	# create efficient frontier(s)
	ef.risk = portopt(ia, constraints, 50, 'Risk')
		ef.risk$weight = ef.risk$weight[, 1:n]
	ef.mad = portopt(ia, constraints, 50, 'MAD', min.mad.portfolio)
		ef.mad$weight = ef.mad$weight[, 1:n]
		
png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')			
		
	layout(1:2)
	par(mar = c(4,4,2,1), cex = 0.8)
	plot(as.vector(sort(100*ef.risk$weight)), pch=20, 
		xlab='', ylab='Weight', main='Portfolio Weights')
	plot(ef.risk$risk, rowSums(ef.risk$weight > 0.000001), pch=20, type='b', 
		xlab='Risk', ylab='# Assets', main='Number of Assets')


dev.off()	
png(filename = 'plot4.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')			
	
	
	layout( matrix(1:4, nrow = 2) )
	plot.ef(ia, list(ef.risk, ef.mad), portfolio.risk, F)	
	plot.ef(ia, list(ef.risk, ef.mad), portfolio.mad, F)	

	plot.transitopn.map(ef.risk)
	plot.transitopn.map(ef.mad)
	
dev.off()	

	
	#--------------------------------------------------------------------------
	# Pre-determined number of asstes, say 3 
	# Add binary[0/1] variables
	# 0.00001 * b <= x.i <= 0.8 * b
	# SUM b = 3
	#--------------------------------------------------------------------------
	
	# SUM x.i = 1
	constraints = new.constraints(rep(1, n), 1, type = '=')		
	
	# new add binary constraint	
	# adjust prior constraints: add b.i
	constraints$A = rbind( constraints$A, matrix(0, n, ncol(constraints$A)) )
	
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
				
	layout(1:2)
	par(mar = c(4,4,2,1), cex = 0.8)
	plot(as.vector(sort(100*ef.risk$weight)), pch=20, 
		xlab='', ylab='Weight', main='Portfolio Weights')
	plot(ef.risk$risk, rowSums(ef.risk$weight > 0.000001), pch=20, type='b', 
		xlab='Risk', ylab='# Assets', main='Number of Assets')



dev.off()	
png(filename = 'plot6.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')			
		
	layout( matrix(1:4, nrow = 2) )
	plot.ef(ia, list(ef.risk, ef.mad), portfolio.risk, F)	
	plot.ef(ia, list(ef.risk, ef.mad), portfolio.mad, F)	

	plot.transitopn.map(ef.risk)
	plot.transitopn.map(ef.mad)
	

dev.off()	
	
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
		hist.prices = Cl(hist.prices)[month.ends, ]
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
	ia$expected.return = apply(hist.returns, 2, mean, na.rm = T)
	ia$risk = apply(hist.returns, 2, sd, na.rm = T)
	ia$correlation = cor(hist.returns, use = 'complete.obs', method = 'pearson')			
	
		# convert to annual, year = 12 months
		annual.factor = 12
		ia$expected.return = annual.factor * ia$expected.return
		ia$risk = sqrt(annual.factor) * ia$risk

		# compute covariance matrix
		ia$risk = iif(ia$risk == 0, 0.000001, ia$risk)
	ia$cov = ia$cor * (ia$risk %*% t(ia$risk))		
	
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

	