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
# Read GNU MathProg model 
# Copyright (C) 2012  Michael Kapler
#
# For more information please visit my blog at www.SystematicInvestor.wordpress.com
# or drop me a line at TheSystematicInvestor at gmail
###############################################################################


###############################################################################
# Read GNU MathProg model
# based on Rglpk_read_file, modified to eliminate objective fn from constraint matrix
#' @export 
############################################################################### 
Rglpk.read.model <- function(file, type = c("MPS_fixed", "MPS_free", "CPLEX_LP", "MathProg"), ignore_first_row = FALSE, verbose = FALSE){
	if(!file.exists(file)) stop(paste("There is no file called", file, "!"))
	
  	type_db <- c("MPS_fixed" = 1L, "MPS_free"  = 2L, "CPLEX_LP"  = 3L, "MathProg"  = 4L)
  	obj <- list(file = tools::file_path_as_absolute(file), type = type_db[match.arg(type)])
  	
	meta_data <- Rglpk:::glp_get_meta_data_from_file(obj, verbose)
	milp_data <- Rglpk:::glp_retrieve_MP_from_file(meta_data, ignore_first_row, verbose)
	MP_data <- Rglpk:::glp_merge_MP_data(meta_data, milp_data)
	
	dir_db <- c("free" = 1L, ">=" = 2L, "<=" = 3L, "DB" = 4L, "==" = 5L)
  	MP_data$direction_of_constraints <- names(dir_db[MP_data$direction_of_constraints])
  	
	types <- rep("C", length.out = MP_data$n_objective_vars)
  	if(any(MP_data$objective_var_is_integer)) types[MP_data$objective_var_is_integer] <- "I"  	
  	if(any(MP_data$objective_var_is_binary)) types[MP_data$objective_var_is_binary] <- "B"  	
  	MP_data$types = types
  	
	# remove objective fn from constraints
	index = which(MP_data$direction_of_constraints == 'free')
	if( length(index) > 0 ) {
		MP_data$constraint_matrix = as.matrix(MP_data$constraint_matrix)[-index,]
		MP_data$direction_of_constraints = MP_data$direction_of_constraints[-index]
		MP_data$right_hand_side = MP_data$right_hand_side[-index]	
	}  
	MP_data
}


###############################################################################
# Create constraints structure from model data (Rglpk.read.model)
#' @export 
############################################################################### 
Rglpk.create.constraints <- function( prob ) 
{       
	#--------------------------------------------------------------------------
	# Create constraints
	#--------------------------------------------------------------------------
	n = prob$n_objective_vars
	
	lb = rep(NA,n)
		lb[prob$bounds$lower$ind] = prob$bounds$lower$val
	ub = rep(NA,n)
		ub[prob$bounds$upper$ind] = prob$bounds$upper$val
	constraints = new.constraints(n, lb = lb, ub = ub)
	
	# binary variables
	constraints$binary.index = which(prob$objective_var_is_binary == 1)	
	if(len(constraints$binary.index) == 0) constraints$binary.index = 0
					
	#constraints
	if(is.null(dim(prob$constraint_matrix))) {
		prob$constraint_matrix = matrix(prob$constraint_matrix)
	} else {
		prob$constraint_matrix = t(prob$constraint_matrix)
	}
	
	index = which(prob$direction_of_constraints == '==') 
	if(len(index)>0) constraints = add.constraints(prob$constraint_matrix[,index], type = '=', b = prob$right_hand_side[index], constraints)
	
	index = which(prob$direction_of_constraints == '<=') 
	if(len(index)>0) constraints = add.constraints(prob$constraint_matrix[,index], type = '<=', b = prob$right_hand_side[index], constraints)

	index = which(prob$direction_of_constraints == '>=') 
	if(len(index)>0) constraints = add.constraints(prob$constraint_matrix[,index], type = '>=', b = prob$right_hand_side[index], constraints)
	
	# objective function
	f.obj = prob$objective_coefficients
	dir = ifelse(prob$maximize, 'max', 'min')

   	prob$names = prob$objective_vars_names
   	prob$tickers = prob$objective_vars_names
	
    # find tickers wgt[AAPL]
    if(len(grep('\\[',prob$objective_vars_names)) > 0) {
    temp = matrix(spl(gsub(']','', prob$objective_vars_names),'\\['), nr=2)
    	prob$names = temp[1,]
    	prob$tickers = temp[2,]
    }
	
	return(list(constraints=constraints, f.obj=f.obj, dir=dir, prob=prob))
}
 



###############################################################################
# Parse Views / Constraints using GNU Mathprog specifications
#' @export 
############################################################################### 
parse.views = function(symbolnames, views) {
	load.packages('Rglpk')

	views = parse.expr(views)
	if (len(views)==0)
		return(list(
			A = matrix(0, nr=0, nc=len(symbolnames)),
			b = c(),
			meq = 0
		))	
	
model.file = tempfile('temp.model')
on.exit(unlink(model.file))	
	
	# create GNU MathProg model
	cat("	
###############################################################################	
# Define Variables
", join(paste0('var ', symbolnames, '>=0;'),'\n'), "

# Define Objective
minimize objective : ", join(symbolnames, '+'), ";

# Define Constraints
", join(paste0('V', 1:len(views), ':', views,';'),'\n'), "
 
###############################################################################
	", file = model.file, append = FALSE)
	
	#--------------------------------------------------------------------------
	# Read GNU MathProg model/Setup constraints/Solve QP problem
	#--------------------------------------------------------------------------	
	# read model
	model = Rglpk.read.model(model.file,type = 'MathProg') 	

	# convert GNU MathProg model to constraint used in solve.QP
	temp = Rglpk.create.constraints(model)$constraints	
	
	A = t(as.matrix(temp$A))
		colnames(A) = symbolnames
	
	list(
		A = A,
		b = temp$b,
		meq = temp$meq
	)
}	
	


###############################################################################
# Helper function to find Minimum Variance Portfolio
#' @export 
############################################################################### 
min.var.portfolio.gmpl <- function(ia, constraints)
{
	#--------------------------------------------------------------------------
	# Adjust Covariance matrix
	#--------------------------------------------------------------------------
	load.packages('quadprog,corpcor')
	
	cov.temp = ia$cov
	
	# check if there are dummy variables
	n0 = ia$n
	n = nrow(constraints$A)			
	if( n != nrow(cov.temp) ) {
		temp =  matrix(0, n, n)
		temp[1:n0, 1:n0] = cov.temp[1:n0, 1:n0]
		cov.temp = temp
	}	
	
	if(!is.positive.definite(cov.temp)) {
		cov.temp <- make.positive.definite(cov.temp, 0.000000001)
	}	
	
	#--------------------------------------------------------------------------
	# Solve QP problem
	#--------------------------------------------------------------------------
	binary.vec = 0
	if(!is.null(constraints$binary.index)) binary.vec = constraints$binary.index
	
	sol = solve.QP.bounds(Dmat = cov.temp, dvec = rep(0, nrow(cov.temp)) , 
		Amat=constraints$A, bvec=constraints$b, constraints$meq,
		lb = constraints$lb, ub = constraints$ub, binary.vec = binary.vec)

	if(binary.vec[1] != 0) cat(sol$counter,'QP calls made to solve problem with', len(binary.vec), 'binary variables using Branch&Bound', '\n')		
	
	x = sol$solution[1:ia$n]
		names(x) = ia$symbols

	return(x)
}		


###############################################################################
# Portfolio Optimization: Specify constraints with GNU MathProg language
#
# Examples:
# http://en.wikibooks.org/wiki/GLPK/GMPL_%28MathProg%29
# http://en.wikibooks.org/wiki/GLPK/Literature#Official_GLPK_documentation
#
# The GNU Linear Programming Kit (GLPK) : Resources, Tutorials
# http://spokutta.wordpress.com/tag/gnu-mathprog/
###############################################################################
portopt.mathprog.test <- function( ) 
{
	#*****************************************************************
	# Load packages
	#****************************************************************** 
	load.packages('quantmod,quadprog,corpcor')
			
	#--------------------------------------------------------------------------
	# Create historical input assumptions
	#--------------------------------------------------------------------------
	tickers = dow.jones.components()
	ia = aa.test.create.ia.custom(tickers, dates = '2000::2010')
	
	#--------------------------------------------------------------------------
	# Create Constraints & Solve QP problem
	#--------------------------------------------------------------------------
	n = ia$n		

	# 0 <= x.i <= 1 
	constraints = new.constraints(n, lb = 0, ub = 1)

	# SUM x.i = 1
	constraints = add.constraints(rep(1, n), 1, type = '=', constraints)		
	
	# Solve QP problem
	x = min.var.portfolio.gmpl(ia, constraints)	
	
	# plot weights
png(filename = 'plot1.png', width = 600, height = 400, units = 'px', pointsize = 12, bg = 'white')										
	barplot(100*x, las=2, main='Minimum Variance Portfolio')
dev.off()	
	

	
	
	
	
	#*****************************************************************
	# Load packages
	#****************************************************************** 
	# load Rglpk to read GNU MathProg files
	load.packages('Rglpk')
		
	#--------------------------------------------------------------------------
	# Create Constraints: GNU MathProg model
	#--------------------------------------------------------------------------
	model.file = 'model1.mod'
	
	# create GNU MathProg model
	cat('	
###############################################################################	
set SYMBOLS ;	
	
# set min/max weights for individual stocks
var weight{i in SYMBOLS} >= 0, <= 1 ;
 
# objective function, NOT USED
minimize alpha : sum{i in SYMBOLS} weight[i] ;
 
# weights must sum to 1 (fully invested)
subject to fully_invested : sum{i in SYMBOLS} weight[i] = 1 ;

data;

set SYMBOLS := ', ia$symbols, ';
###############################################################################
	', file = model.file, append = FALSE)

	#--------------------------------------------------------------------------
	# Read GNU MathProg model/Setup constraints/Solve QP problem
	#--------------------------------------------------------------------------	
	# read model
	model = Rglpk.read.model(model.file,type = 'MathProg') 	

	# convert GNU MathProg model to constraint used in solve.QP
	constraints = Rglpk.create.constraints(model)$constraints	
		
	# Solve QP problem
	x = min.var.portfolio.gmpl(ia, constraints)	
	
	# plot weights
png(filename = 'plot2.png', width = 600, height = 400, units = 'px', pointsize = 12, bg = 'white')										
	barplot(100*x, las=2, main='Minimum Variance Portfolio using GNU MathProg model')
dev.off()	

	
	#--------------------------------------------------------------------------
	# Create Constraints: GNU MathProg model
	# Control Minimum Investment and Number of Assets: Portfolio Cardinality Constraints
	# http://systematicinvestor.wordpress.com/2011/10/20/minimum-investment-and-number-of-assets-portfolio-cardinality-constraints/
	#--------------------------------------------------------------------------
	model.file = 'model2.mod'
	
	# create GNU MathProg model
	cat('	
###############################################################################	
set SYMBOLS ;	
	
# set min/max weights for individual stocks
var weight{i in SYMBOLS} >= 0, <= 1 ;
 
# add binary, 1 if held, 0 if not held
var held{SYMBOLS} binary;

# objective function, NOT USED
minimize alpha : sum{i in SYMBOLS} weight[i] ;
 
# weights must sum to 1 (fully invested)
subject to fully_invested : sum{i in SYMBOLS} weight[i] = 1 ;

# min weight constraint for individual asset
subject to MinWgt {i in SYMBOLS} : weight[i] >= 0.025 * held[i];
 
# max weight constraint for individual asset
subject to MaxWgt {i in SYMBOLS} : weight[i] <= .20 * held[i] ;

# number of stocks in portfolio
subject to MaxAssetsLB : 0 <= sum {i in SYMBOLS} held[i] ;
subject to MaxAssetsUB : sum {i in SYMBOLS} held[i] <= 6 ;
 
data;

set SYMBOLS := ', ia$symbols, ';
###############################################################################
	', file = model.file, append = FALSE)
	
	#--------------------------------------------------------------------------
	# Read GNU MathProg model/Setup constraints/Solve QP problem
	#--------------------------------------------------------------------------	
	# read model
	model = Rglpk.read.model(model.file,type = 'MathProg') 	

	# convert GNU MathProg model to constraint used in solve.QP
	constraints = Rglpk.create.constraints(model)$constraints	
		
	# Solve QP problem
	x = min.var.portfolio.gmpl(ia, constraints)	
	
	# plot weights
png(filename = 'plot3.png', width = 600, height = 400, units = 'px', pointsize = 12, bg = 'white')										
	barplot(100*x, las=2, main='Minimum Variance Portfolio using GNU MathProg model \n with Minimum Investment and Number of Assets Constraints')
dev.off()	

	
	#--------------------------------------------------------------------------
	# Create Constraints: GNU MathProg model
	# Control Long and Short positions based on 130/30 Portfolio Construction
	# http://systematicinvestor.wordpress.com/2011/10/18/13030-porfolio-construction/
	#--------------------------------------------------------------------------
	model.file = 'model3.mod'

	# create GNU MathProg model
	cat('	
###############################################################################	
set SYMBOLS ;	
	
# set min/max weights for individual stocks
var long {i in SYMBOLS} >= 0, <= 0.8 ;
var short{i in SYMBOLS} >= 0, <= 0.5 ;
 
# add binary, 1 if long, 0 if short
var islong{SYMBOLS} binary;

# objective function, NOT USED
minimize alpha : sum{i in SYMBOLS} long[i] ;
 
# weights must sum to 1 (fully invested)
subject to fully_invested : sum{i in SYMBOLS} (long[i] - short[i]) = 1 ;

# leverage is 1.6 = longs + shorts
subject to leverage : sum{i in SYMBOLS} (long[i] + short[i]) = 1.6 ;

# force long and short to be mutually exclusive (only one of them is greater then 0 for each i)
subject to long_flag  {i in SYMBOLS} : long[i]  <= islong[i] ;
subject to short_flag {i in SYMBOLS} : short[i] <= (1 - islong[i]) ;
 
data;

set SYMBOLS := ', ia$symbols, ';
###############################################################################
	', file = model.file, append = FALSE)
	
	#--------------------------------------------------------------------------
	# Read GNU MathProg model/Setup constraints/Solve QP problem
	#--------------------------------------------------------------------------	
	# read model
	model = Rglpk.read.model(model.file,type = 'MathProg') 	

	# convert GNU MathProg model to constraint used in solve.QP
	constraints = Rglpk.create.constraints(model)$constraints	
		
	# Solve QP problem, modify Input Assumptions to include short positions
	x = min.var.portfolio.gmpl(aa.test.ia.add.short(ia), constraints)	
	
	# Compute total weight = longs - short
	x = x[1:ia$n] - x[-c(1:ia$n)]
	
	# plot weights
png(filename = 'plot4.png', width = 600, height = 400, units = 'px', pointsize = 12, bg = 'white')										
	barplot(100*x, las=2, main='Minimum Variance Portfolio using GNU MathProg model \n with 130:30 Constraints')
dev.off()	
	




	# reduce problem size
	ia = aa.test.create.ia.custom(tickers[1:15], dates = '2000::2010')

	#--------------------------------------------------------------------------
	# Create Constraints: GNU MathProg model
	# Turnover Constraints : Control Maximum Trade Size and Number of Trades
	#--------------------------------------------------------------------------
	model.file = 'model4.mod'

	# create parameters to hold Current Weight
	param = ia$cov[,1,drop=F]
		colnames(param) = 'CurWgt'
		param[,'CurWgt'] = 1/ia$n
		
	
	# create GNU MathProg model
	cat('	
###############################################################################	
set SYMBOLS ;	
	
param CurWgt{SYMBOLS} ;

# set min/max weights for individual stocks
var weight{i in SYMBOLS} >= 0, <= 1 ;

# TradePos[i] - TradeNeg[i] = CurWgt[i] - weight[i]
var TradePos{i in SYMBOLS} >= 0 ;
var TradeNeg{i in SYMBOLS} >= 0 ;

# Only one of TradePos or TradeNeg is > 0
var TradeFlag{SYMBOLS} binary;

# add binary, 1 if traded, 0 if not traded
var trade{SYMBOLS} binary;

# objective function, NOT USED
minimize alpha : sum{i in SYMBOLS} weight[i] ;
 
# weights must sum to 1 (fully invested)
subject to fully_invested : sum{i in SYMBOLS} weight[i] = 1 ;

# setup Trades for individual asset
subject to TradeRange {i in SYMBOLS} : (CurWgt[i] - weight[i]) = (TradePos[i] - TradeNeg[i]) ;

# Only one of TradePos or TradeNeg is > 0
subject to TradeFlagPos {i in SYMBOLS} : TradePos[i] <= 100 * TradeFlag[i];
subject to TradeFlagNeg {i in SYMBOLS} : TradeNeg[i] <= 100 * (1 - TradeFlag[i]);

# min trade size constraint for individual asset
subject to MinTradeSize {i in SYMBOLS} : (TradePos[i] + TradeNeg[i]) >= 0.01 * trade[i];
subject to MaxTradeSize {i in SYMBOLS} : (TradePos[i] + TradeNeg[i]) <= .90 * trade[i] ; 

# number of trades in portfolio
subject to MaxTrade : sum {i in SYMBOLS} trade[i] <= 48 ;
 
data;

set SYMBOLS := ', ia$symbols, ';

param : CurWgt:=
	', file = model.file, append = FALSE)
	
write.table(param, sep='\t', quote = F, col.names = F, file = model.file, append = TRUE)
cat('; 
###############################################################################
	', file = model.file, append = TRUE)

	
	#--------------------------------------------------------------------------
	# Read GNU MathProg model/Setup constraints/Solve QP problem
	#--------------------------------------------------------------------------	
	model = Rglpk.read.model(model.file,type = 'MathProg') 	
	constraints = Rglpk.create.constraints(model)$constraints	
	

	# Solve QP problem
	x = min.var.portfolio.gmpl(ia, constraints)	
		sqrt(x %*% ia$cov %*% x)
	
	
	# plot weights
png(filename = 'plot5.png', width = 600, height = 400, units = 'px', pointsize = 12, bg = 'white')										
	barplot(100*x, las=2, main='Minimum Variance Portfolio using GNU MathProg model \n with Turnover Constraints')
dev.off()		










# reduce problem size
ia = aa.test.create.ia.custom(tickers[1:10], dates = '2000::2010')


	#--------------------------------------------------------------------------
	# Create Constraints: GNU MathProg model
	# Turnover Constraints : Control Maximum Trade Size and Number of Trades
	#--------------------------------------------------------------------------
	model.file = 'model4.mod'

	# create parameters to hold Current Weight
	param = ia$cov[,1,drop=F]
		colnames(param) = 'CurWgt'
		param[,'CurWgt'] = 1/ia$n
		
	
	# create GNU MathProg model
	cat('	
###############################################################################	
set SYMBOLS ;	
	
param CurWgt{SYMBOLS} ;

# set min/max weights for individual stocks
var weight{i in SYMBOLS} >= 0, <= 1 ;

# TradePos[i] - TradeNeg[i] = CurWgt[i] - weight[i]
var TradePos{i in SYMBOLS} >= 0 ;
var TradeNeg{i in SYMBOLS} >= 0 ;

# Only one of TradePos or TradeNeg is > 0
var TradeFlag{SYMBOLS} binary;

# add binary, 1 if traded, 0 if not traded
var trade{SYMBOLS} binary;

# objective function, NOT USED
minimize alpha : sum{i in SYMBOLS} weight[i] ;
 
# weights must sum to 1 (fully invested)
subject to fully_invested : sum{i in SYMBOLS} weight[i] = 1 ;

# setup Trades for individual asset
subject to TradeRange {i in SYMBOLS} : (CurWgt[i] - weight[i]) = (TradePos[i] - TradeNeg[i]) ;

# Only one of TradePos or TradeNeg is > 0
subject to TradeFlagPos {i in SYMBOLS} : TradePos[i] <= 100 * TradeFlag[i];
subject to TradeFlagNeg {i in SYMBOLS} : TradeNeg[i] <= 100 * (1 - TradeFlag[i]);

# min trade size constraint for individual asset
subject to MinTradeSize {i in SYMBOLS} : (TradePos[i] + TradeNeg[i]) >= 0.05 * trade[i];
subject to MaxTradeSize {i in SYMBOLS} : (TradePos[i] + TradeNeg[i]) <= .20 * trade[i] ; 

# number of trades in portfolio
subject to MaxTrade : sum {i in SYMBOLS} trade[i] <= 8 ;
 
data;

set SYMBOLS := ', ia$symbols, ';

param : CurWgt:=
	', file = model.file, append = FALSE)
	
write.table(param, sep='\t', quote = F, col.names = F, file = model.file, append = TRUE)
cat('; 
###############################################################################
	', file = model.file, append = TRUE)

	
	#--------------------------------------------------------------------------
	# Read GNU MathProg model/Setup constraints/Solve QP problem
	#--------------------------------------------------------------------------	
	model = Rglpk.read.model(model.file,type = 'MathProg') 	
	constraints = Rglpk.create.constraints(model)$constraints	
	

	# Solve QP problem
	x = min.var.portfolio.gmpl(ia, constraints)	
		sqrt(x %*% ia$cov %*% x)
	
	
	# plot weights
png(filename = 'plot6.png', width = 600, height = 400, units = 'px', pointsize = 12, bg = 'white')										
	barplot(100*x, las=2, main='Minimum Variance Portfolio using GNU MathProg model \n with Turnover Constraints')
dev.off()		





}



					