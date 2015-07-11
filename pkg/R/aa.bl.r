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
# Black-Litterman model Functions
# Copyright (C) 2011  Michael Kapler
#
# For more information please visit my blog at www.SystematicInvestor.wordpress.com
# or drop me a line at TheSystematicInvestor at gmail
###############################################################################


###############################################################################
# Black-Litterman model Functions
###############################################################################
# He & Litterman: The intuition Behind Black- Litterman Model Portfolios
# T. Idzorek: A STEP-BY-STEP GUIDE TO THE BLACK-LITTERMAN MODEL
# note (5)
#
#' @export 
bl.compute.risk.aversion <- function(bench, risk.free = 0)
{
	# The implied risk aversion coefficient can be estimated by dividing
	# the expected excess return by the variance of the portfolio
	lambda = mean(coredata(bench) - coredata(risk.free)) / var(coredata(bench))
	return( as.double(lambda) )
}

# He & Litterman: The intuition Behind Black- Litterman Model Portfolios
# formulas (2)
#
# T. Idzorek: A STEP-BY-STEP GUIDE TO THE BLACK-LITTERMAN MODEL
# formulas (1)
#
# use reverse optimization to compute the vector of equilibrium returns
#' @export 
bl.compute.eqret <- function
(
	risk.aversion, 	# Risk Aversion
	cov, 			# Covariance matrix
	cap.weight, 	# Market Capitalization Weights
	risk.free = 0	# Rsik Free Interest Rate
)
{
	return( risk.aversion * cov %*% as.vector(cap.weight) +  as.double(risk.free))	
}

# He & Litterman: The intuition Behind Black- Litterman Model Portfolios
# formulas (8), (9), (10)
# compute the posterior estimate of the returns and covariance
#' @export 
bl.compute.posterior <- function
(
	mu, 		# Equilibrium returns
	cov, 		# Covariance matrix
	pmat=NULL, 	# Views pick matrix
	qmat=NULL, 	# View mean vector
	tau=0.025, 	# Measure of uncertainty of the prior estimate of the mean returns
	confidences=NULL  # Confidence of each view
)
{
	out = list()	

	if( !is.null(pmat) ) {
		if( is.null(confidences) ) {
		# The Black-Litterman Model In Detail by Jay Walters
		# Assume that the variance of the views will be proportional to the variance of the asset
		# returns, just as the variance of the prior distribution is. He and Litterman (1999)
		# This specification of the variance, or uncertainty, of the views essentially equally weights the investor's
		# views and the market equilibrium weights. By including tau in the expression, the final solution becomes
		# independent of tau as well.
				
		# contactenate 1 and remove first row, col ([-1,-1]) to work properly with single view
		omega = diag(c(1,diag(tau * pmat %*% cov %*% t(pmat))))[-1,-1]
		} else {
		omega = diag(c(1,confidences))[-1,-1]
		}
		
		temp = solve(solve(tau * cov) + t(pmat) %*% solve(omega) %*% pmat)
	
		out$cov = cov + temp
	
		out$expected.return = temp %*% (solve(tau * cov) %*% mu + t(pmat) %*% solve(omega) %*% qmat)
	} else {	# no views	
		temp = tau * cov
	
		out$cov = cov + temp
	
		out$expected.return = temp %*% (solve(tau * cov) %*% mu )
	
	}
	return(out)
}


# He & Litterman: The intuition Behind Black- Litterman Model Portfolios
# formulas (13)
#
# T. Idzorek: A STEP-BY-STEP GUIDE TO THE BLACK-LITTERMAN MODEL
# formulas (2)
#
# compute the portfolio weights for the optimal portfolio on the unconstrained efficient frontier
#' @export 
bl.compute.optimal <- function(risk.aversion, mu, cov)
{
	return( (1/risk.aversion) * solve(cov) %*% mu )
}

	

aa.black.litterman.examples <- function()
{
	# He & Litterman: The intuition Behind Black- Litterman Model Portfolios. 

	data =
 '1,0.4880,0.4780,0.5150,0.4390,0.5120,0.4910
 0.4880,1,0.6640,0.6550,0.3100,0.6080,0.7790
 0.4780,0.6640,1,0.8610,0.3550,0.7830,0.6680
 0.5150,0.6550,0.8610,1,0.3540,0.7770,0.6530
 0.4390,0.3100,0.3550,0.3540,1,0.4050,0.3060
 0.5120,0.6080,0.7830,0.7770,0.4050,1,0.6520
 0.4910,0.7790,0.6680,0.6530,0.3060,0.6520,1'
	
	Corrmat = matrix( as.double(spl( gsub('\n', ',', data), ',')), 
				nrow = len(spl(data, '\n')), byrow=TRUE)
	
	RiskAversion = 2.5

	stdevs = c(16.0, 20.3, 24.8, 27.1, 21.0,  20.0, 18.7)/100

	MktWeight = c(1.6, 2.2, 5.2, 5.5, 11.6, 12.4, 61.5)/100

	tau = 0.05

	Covmat = Corrmat * (stdevs %*% t(stdevs))
	
	EqRiskPrem = RiskAversion * Covmat %*% MktWeight
EqRiskPrem = bl.compute.eqret(RiskAversion, Covmat, MktWeight)

	AssetNames = c('Australia','Canada','France','Germany','Japan','UK','USA')

	Table2 = cbind(AssetNames, round(cbind(stdevs, MktWeight, EqRiskPrem) * 100,1))
		colnames(Table2) = c('Assets','Std Dev','Weq','PI')
		Table2
		
	#View1 is The German Equity Market Will Outperform the rest of European Markets by 5% a year.
	P = matrix(c(0, 0, -29.5, 100, 0, -70.5, 0)/100, nrow=1)
	Q = 5/100
		
	Omega = diag(c(1,diag(tau * P %*% Covmat %*% t(P))))[-1,-1]

	PostCov = solve(solve(tau*Covmat) + (t(P) %*% solve(Omega) %*% P))

	SigmaP = Covmat + PostCov

	ExpRet = PostCov %*% (solve(tau * Covmat) %*% EqRiskPrem + t(P) %*% solve(Omega) %*% Q)
	
post = bl.compute.posterior(EqRiskPrem, Covmat, P, Q, tau)		
	ExpRet = post$expected.return
	SigmaP = post$cov
	
	OptimalWeights = (1/RiskAversion) * solve(SigmaP) %*% ExpRet
OptimalWeights = bl.compute.optimal(RiskAversion, ExpRet, SigmaP)	
	
	Tab4Col4 = OptimalWeights - (MktWeight)/(1+tau)
		
	Table4 = cbind(AssetNames, round(cbind(t(P), ExpRet, OptimalWeights, round(Tab4Col4 * 1000)/1000)*100,1))
		colnames(Table4) = c('Assets', 'P', 'MU', 'W','W - Weq/1+tau')
		Table4 


	# example from Thomas M. Idzorek's paper "A STEP-BY-STEP GUIDE TO THE BLACK-LITTERMAN MODEL"
	x = 
	c(0.001005,0.001328,-0.000579,-0.000675,0.000121,0.000128,-0.000445,-0.000437 ,
     0.001328,0.007277,-0.001307,-0.000610,-0.002237,-0.000989,0.001442,-0.001535 ,
     -0.000579,-0.001307,0.059852,0.027588,0.063497,0.023036,0.032967,0.048039 ,
    -0.000675,-0.000610,0.027588,0.029609,0.026572,0.021465,0.020697,0.029854 ,
     0.000121,-0.002237,0.063497,0.026572,0.102488,0.042744,0.039943,0.065994 ,
     0.000128,-0.000989,0.023036,0.021465,0.042744,0.032056,0.019881,0.032235 ,
    -0.000445,0.001442,0.032967,0.020697,0.039943,0.019881,0.028355,0.035064 ,
    -0.000437,-0.001535,0.048039,0.029854,0.065994,0.032235,0.035064,0.079958 )

    varCov <- matrix(x, ncol = 8, nrow = 8)
    mu <- c(0.08, 0.67,6.41, 4.08, 7.43, 3.70, 4.80, 6.60) / 100
    pick <- matrix(0, ncol = 8, nrow = 3, dimnames = list(NULL, letters[1:8]))
    pick[1,7] <- 1
    pick[2,1] <- -1; pick[2,2] <- 1
    pick[3, 3:6] <- c(0.9, -0.9, .1, -.1)
    
post = bl.compute.posterior(mu, varCov, pick, c(0.0525, 0.0025, 0.02), tau = 0.025)		    
    
	library(BLCOP)
    confidences <- 1 / c(0.000709, 0.000141, 0.000866)
    myViews <- BLViews(pick, c(0.0525, 0.0025, 0.02), confidences, letters[1:8])
    myPosterior <- posteriorEst(myViews, tau = 0.025, mu, varCov )
    myPosterior

 
 myPosterior@posteriorMean - post$expected.return
 myPosterior@posteriorCovar - post$cov
 
 
			
}
