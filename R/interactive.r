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
# Collection of routines for interactive plots
# Copyright (C) 2015  Michael Kapler
#
# For more information please visit my blog at www.SystematicInvestor.wordpress.com
# or drop me a line at TheSystematicInvestor at gmail
###############################################################################



###############################################################################
#' Add Vertical, Horizontal, Cross line to existing plot at location of mouse click
#' based on getGraphicsEvent {grDevices}
#'
#' Please note there is also an interacive plot version in
#' library(playwith)
#' playwith(plot(1:10))
#'
#' @return nothing
#'
#' @examples
#' \dontrun{ 
#' plot(1:10)
#' iline()
#' }
#' @export 
############################################################################### 
iline = function(type=c('v','h','cross'), col='red', remove.col='white',stop.key = 'q') {
	type = tolower(substr(type[1],1,1))
	#type = switch(type, 'v'=1, 'h'=2, 'c'=3)
	
	keydown <- function(key) {
		if (key == stop.key) return(invisible(1))
		NULL
	}
	
	# alternative to locator(1)
	prev.x = NULL
	prev.y = NULL
	mouseup <- function(buttons, x, y) {
	par(xpd=NA)
		if(type == 'v' || type == 'c')
			if(!is.null(prev.x)) abline(v=prev.x, col=remove.col)
		if(type == 'h' || type == 'c')
			if(!is.null(prev.y)) abline(h=prev.y, col=remove.col)	
			
		prev.x <<- grconvertX(x, "ndc", "user")
		prev.y <<- grconvertY(y, "ndc", "user")
			
		if(type == 'v' || type == 'c')
			abline(v=prev.x, col=col)
		if(type == 'h' || type == 'c')
			abline(h=prev.y, col=col)
	par(xpd=FALSE)
	NULL
	}	
	
	getGraphicsEvent(prompt = "Click to plot v/h/cross line, hit q to quit", 
		onMouseDown = mouseup,onKeybd = keydown)
}

###############################################################################
#' Visualize System Parameter Optimization
#' based on [Visualizing Data](http://sanzprophet.blogspot.tw/2013/01/visualizing-data.html)
#' The article was using [XDat](http://www.xdat.org/index.php?ref=download) app
#'
#' @return nothing
#'
#' @examples
#' \dontrun{ 
#' result = cbind(rnorm(20,0,1),rnorm(20,10,1),rnorm(20,0,100),rnorm(20,20,21))
#'  colnames(result) = spl('CAGR,A,B,C')
#' visualize.system.parameter.optimization(result)
#' }
#' @export 
############################################################################### 
visualize.system.parameter.optimization = function(result) { 
  load.packages('rpanel,tkrplot,MASS')

  draw = function(panel) {
    par(bg='white')
    col = cols
    index = colSums(result.t >= panel$min & result.t <= panel$max) == n
    col[ index ] = 2
    parcoord(result, var.label =T, col=col, lwd=col)
    panel
  }

  redraw = function(panel) {
    rp.tkrreplot(panel, tkrp)
    panel
  }

  max.val = apply(result,2,max) 
  min.val = apply(result,2,min)
  max.val = max.val + 0.1 * abs(max.val) 
  min.val = min.val - 0.1 * abs(min.val)

  n = ncol(result)
  cols = rep(1, nrow(result))
  result.t = t(result)  
  colnames(result) = paste0(colnames(result), '\n(', 1:n,')')

  panel  = rp.control(title = 'Parallel Coordinates', max=max.val, min=min.val)
  rp.slider(panel, max, max.val, min.val, horizontal=F, showvalue = T, action = redraw,initval=max.val)
  rp.slider(panel, min, max.val, min.val, horizontal=F, showvalue = T, action = redraw,initval=min.val)   
  rp.tkrplot(panel, tkrp, draw)
}

