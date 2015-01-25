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

