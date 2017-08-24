###############################################################################
# This software is provided 'as-is', without any express or implied
# warranty. In no event will the authors be held liable for any damages
# arising from the use of this software.
# 
# Permission is granted to anyone to use this software for any purpose,
# including commercial applications, and to alter it and redistribute it
# freely, subject to the following restrictions:
# 
# 1. The origin of this software must not be misrepresented; you must not
#    claim that you wrote the original software. If you use this software
#    in a product, an acknowledgment in the product documentation would be
#    appreciated but is not required.
# 2. Altered source versions must be plainly marked as such, and must not be
#    misrepresented as being the original software.
# 3. This notice may not be removed or altered from any source distribution.
###############################################################################
# Binary Branch and Bound and it's adaption for QP problem
# Copyright (C) 1998-2000  Alberto Bemporad, Domenico Mignone - author's of the original Matlab version					
#
# For more information please email at TheSystematicInvestor at gmail
###############################################################################


###############################################################################
# Binary Branch and Bound algorithm adpated from
# miqp.m, a Matlab function for solving Mixed Integer Quadratic Programs
# by Alberto Bemporad, Domenico Mignone
# The routine was modified to work with large set of optimization problems.
#
# http://www.aut.ee.ethz.ch/~hybrid/miqp/ 
#' @export 
###############################################################################
binary_branch_bound <- function
(
	index_binvar,		# index of binary[0/1] variables
	bbb_data, 			# data used for solving problems
	bbb_solve, 			# bbb_solve(bbb_data, binvar_lb, binvar_ub) - function to solve problems
	control = bbb_control()	# control the behavior of binary_branch_bound
)
{
# Output:     
# xmin: minimizer of the cost function
# fmin: minimum value of the cost function
# counter: number of executions
# flag: integer flag characterizing the result, where:
#  if flag = 1 there exists a feasible solution
#  if flag = 5 the solution is not integer feasible
#  if flag = 7 no feasible solution exists
	
	fbest = Inf
	xbest = 0 * bbb_data$x0
	counter = 0
	nbinvar = length(index_binvar)
	flag = 7 	# by default it is infeasible
				
	# The Variable STACK will contain the subproblems
	stack = new.env()
		stack$data = list()
		stack$cost = c()
		stack$pointer = c()
	stack$data[[1]] = list(lb = bbb_data$lb, 
							ub = bbb_data$ub, 
							var = 1:nbinvar, 
							path = rep(0,nbinvar), 
							level = 0, 
							fval = Inf)	
	stack$cost = 0		# Array storing the cost of the problems, ordered in decreasing fashion (cost(1)=largest value)
	stack$pointer = 1	# pointer stores the order of the list	
	
	control$proborder.selected = control$proborder
	
	if(F) {
		lb = bbb_data$lb 
		ub = bbb_data$ub
		 
		# presolve two default cases
		for( i in 0:1 ) {
			lb[] = i
			ub[] = i
			sol = match.fun(bbb_solve)(bbb_data, lb, ub)
		
			if( sol$ok ) {                 
				x = sol$x
				fval = sol$fval
	            xi = x[index_binvar]	# binary variables
				            
				# found solution
				if ( max(abs( round(xi,0) - xi )) < control$bineps ) {
					fbest = fval
				    xbest = x              
				    flag = 1
				    if( !control$silent ) cat('FOUND SOLUTION =', fbest, '\n');
				} 
			}
		}
	}
	
	# Main Loop
	while ( length(stack$data) > 0 ) {	
	    # Get the next subproblem from the STACK
		subprob = bbb_pop(stack)
		
		if( !control$silent ) {
			cat('-----------------------------------------------------', '\n')
			if( max(subprob$path) > 0 ) {
				temp.index = order(-subprob$path)[1 : sum(subprob$path > 0)]
				cat('\t', 
					paste('b', temp.index, ' = ', subprob$lb[temp.index],sep='') 
					, '\n')				
			} else {
				cat(counter, '\t', 'FIRST NODE', '\n')
			}
			
			cat(counter, '\t', subprob$lb, '\t', subprob$var, '\t', subprob$fval, '\t', fbest, '\n')
			cat('\t', subprob$ub, '\n')
			cat('stack size =', len(stack$pointer), '\n')
		}
	    
		if( is.finite( subprob$fval ) & is.finite( fbest ) & fbest <= subprob$fval ) {
			# skip this problem because fbest is alredy smaller
			if( !control$silent ) cat('SKIP this problem because a solution with lower FVAL already found\n')
		} else {
			
		    # Solve the qp
		    counter = counter + 1
			sol = match.fun(bbb_solve)(bbb_data, subprob$lb, subprob$ub)

			                   
			if( !sol$ok ) {                 
				if( !control$silent ) cat('NO SOLUTION EXISTS\n\n');												  			
			} else {
				x = sol$x
				fval = sol$fval
				
				if( !control$silent ) {
					cat('SOLUTION OK', '\t', sol$fval, '\n')								  								
					cat('\t', round(x[index_binvar[subprob$var]],3), '\n\n')
				}

				
		        if ( flag !=1 ) flag=5
		
		        # Check if value function is better than the value so far
		        if ( fval <= fbest ) {
			        if ( length(subprob$var ) == 0 ) {
			        	# found solution
						fbest = fval               
			            xbest = x              
			            flag = 1  
						if( !control$silent ) cat('FOUND SOLUTION =', fbest, '\n');
					} else {
			            xi = x[index_binvar[subprob$var]]	# binary variables
			            
			            # found solution
			            if ( max(abs( round(xi,0) - xi )) < control$bineps ) {
			                fbest = fval
			                xbest = x              
			                flag = 1
			                if( !control$silent ) cat('FOUND SOLUTION =', fbest, '\n');
			            } else {
			                # split problem in 0/1 subproblems	                
			                branchvar = bbb_decision(xi,control)
			                probs = bbb_separate(subprob, branchvar, fval)
			                p0 = probs$p0
			                p1 = probs$p1
			  
			                if( !control$silent ) cat('Branch on =', subprob$var[branchvar], '\n');
			                
			                
			                
			                if( control$searchdir == 0 ) {               
								cost=1/(subprob$level+1) 	# Depth first
							} else if( control$searchdir == 1 ) {               
								cost=subprob$level+1		# Breadth first
							} else if( control$searchdir == 2 ) {               
								cost=fval					# Best-first. This tends to go breadth-first
							} else if( control$searchdir == 3 ) {               
								cost=fval/(subprob$level+1)	# This privilegiates deep nodes
							}					
							
							if( control$proborder == 2 ) {
								control$proborder.selected = round(xi[branchvar],0)
							}
							
			                if( control$proborder.selected == 0 ) {
			                	bbb_push(stack, p1, p0, cost)
			                } else {
			                	bbb_push(stack, p0, p1, cost) 
							}
						}
					}
				}
			}
						
		    # verbose
		    if( F ) {
		        cat('counter =', counter, '\n')
		        cat('fbest     =', fbest, '\n')
		        cat('stack$pointer =', stack$pointer, '\n')
		        cat('\n')
		    }
		}
	} #end while
	rm(list=ls(stack,all=TRUE), envir=stack)

	#xbest[index_binvar] = round(xbest[index_binvar],0) # ROUNDOFF binary solution	
	return(list(xmin = xbest, fmin = fbest, counter = counter, flag = flag))	
}

###############################################################################
# Decision: find next branching variable
###############################################################################
bbb_decision <- function
(
	xi,			# x for binary variables
	control		# control the behavior of binary_branch_bound
)
{
	if( control$branchvar == 0 ) {
		# first free variable is chosen as branching variable
        branchvar = 1
	} else if( control$branchvar == 1 ) {
        # variable with max frac part is chosen as branching variable
        branchvar = which.max( abs(xi-round(xi,0)) )	#pick up the first of with max value	
	} else if( control$branchvar == 2 ) {
        # variable with min frac part is chosen as branching variable
        branchvar = which.min( abs(xi-round(xi,0)) )	#pick up the first of with min value	        
	} else {
		branchvar = 1
	}
	return(branchvar)
}

###############################################################################
# Pop: returns top element of the STACK and eliminate the element from the stack
###############################################################################
bbb_pop <- function(stack)
{
	i = stack$pointer[ length(stack$data) ]
	subprob   = stack$data[[i]]
	
	stack$pointer[ stack$pointer > i ] = stack$pointer[ stack$pointer > i ] - 1
	
	# remove last
	stack$data[[i]] = NULL
	length(stack$cost) = length(stack$data)
	length(stack$pointer) = length(stack$data)
	
	return(subprob)
}

###############################################################################
# Push: puts a subproblem onto the STACK
###############################################################################
bbb_push <- function
(
	stack, 		# stack structure
	element1, 	# element to push on stack
	element2,	# element to push on stack
	cost		# cost
)
{
	n = length(stack$data)
	
	# Determine position in STACK where problem is inserted, according to a best first strategy
	i = match(TRUE, stack$cost <= cost)		# EX: STACKCOST=[100 80 33 22 ^ 5 3 2], cost=10
	if( is.na(i) ) i = n else i = i - 1

	stack$data[[ (n+1) ]] = element1
	stack$data[[ (n+2) ]] = element2

	if(i == 0) {
		stack$pointer=c((n+1),(n+2), stack$pointer)
		stack$cost=c(cost,cost, stack$cost)
	} else {	
		stack$pointer=c(stack$pointer[1:i], (n+1),(n+2), stack$pointer[-c(1:i)])
		stack$cost=c(stack$cost[1:i], cost, cost, stack$cost[-c(1:i)])
	}
}

###############################################################################
# Separate: generates 2 new suproblems from a given problem
###############################################################################
bbb_separate <- function
(
	prob,		# QP parent problem
	branchvar,	# branching variable
	fval		# fval for parent problem
)
{
	if(length(prob$var) >= 1) {
		p0 = prob
			p0$fval = fval
		    p0$level = prob$level + 1
		    p0$var = prob$var[-branchvar]
		    p0$path[ prob$var[branchvar] ] = 1 + max(p0$path)
		p1 = p0
        		
		p0$lb[ prob$var[branchvar] ] = 0
		p0$ub[ prob$var[branchvar] ] = 0
		
		p1$lb[ prob$var[branchvar] ] = 1
		p1$ub[ prob$var[branchvar] ] = 1
	} else {
		stop('no more integer variables to branch on')
	}
	return( list(p0 = p0, p1 = p1) )
}

