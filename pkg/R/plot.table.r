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
# Plot table ( vector or matrix )
# Copyright (C) 2011  Michael Kapler
#
# For more information please visit my blog at www.SystematicInvestor.wordpress.com
# or drop me a line at TheSystematicInvestor at gmail
###############################################################################



###############################################################################
# Internal table drawing routines, based on the example at
# http://addictedtor.free.fr/graphiques/RGraphGallery.php?graph=28
###############################################################################
# make.table - create empty plot
###############################################################################
make.table <- function
(
	nr,	# number of rows
	nc	# number of columns
)
{
	savepar = par(mar = rep(1, 4))
	plot(c(0.5, nc*2 + 0.5), c(-0.5, -(nr + 0.5)), xaxs = 'i', yaxs = 'i', 
		type = 'n', xlab = '', ylab = '', axes = FALSE)
    savepar
}

###############################################################################
# draw.cell - draw cell at location (r,c)
###############################################################################
draw.cell <- function
(
	title,				# text to draw in this cell
	r,					# row
	c,					# column
	text.cex = 1,		# size of text
	bg.col = 'white',	# background color
	frame.cell = T		# flag to draw border around this cell
)
{
	if(!frame.cell) bcol = bg.col else bcol = 'black'
    rect((2*(c - 1) + .5), -(r - .5), (2*c + .5), -(r + .5), col = bg.col, border = bcol)        
    
    if( c == 1) { # first column
    	text((2*(c - 1) + .5), -r, title, adj = 0, cex = text.cex)     
    } else if( r == 1 ) { # first row
    	text((2*(c - 1) + .5), -r, title, adj = 0, cex = text.cex)        
    } else {
    	text((2*c + .5), -r, title, adj = 1, cex = text.cex)
    }
}


###############################################################################
# plot.table.helper.auto.adjust.cex - determine how to auto-adjust text size
#  code is based on discussion at
#  http://www.mail-archive.com/r-help@r-project.org/msg04577.html			
###############################################################################
plot.table.helper.auto.adjust.cex <- function
(
	temp.table, 				# matrix to plot
	keep.all.same.cex = FALSE	# flag to auto-adjust text size
)
{
	nr = nrow(temp.table)
	nc = ncol(temp.table)
	
	all.xrange = diff(par()$usr[1:2]) / nc
		xrange = matrix( strwidth(paste('  ', temp.table), units = 'user', cex = 1), nc = nc)
		
	all.yrange = diff(par()$usr[3:4]) / nr
		yrange = matrix( 5/3 * strheight(temp.table, units = 'user', cex = 1), nc = nc)
		
	plot.matrix.cex = pmin( round(all.yrange / yrange, 2) , round(all.xrange / xrange, 2) )
		header.col.cex = min(plot.matrix.cex[1,-1])
		header.row.cex = min(plot.matrix.cex[-1,1])
		title.cex = plot.matrix.cex[1, 1]
		data.cex = min(plot.matrix.cex[-1, -1])	
		
	if ( keep.all.same.cex ) {
		plot.matrix.cex[] = min(plot.matrix.cex)
	} else {		
		plot.matrix.cex[1,-1] = min(c(header.col.cex, header.row.cex))
		plot.matrix.cex[-1,1] = min(c(header.col.cex, header.row.cex))
		plot.matrix.cex[-1,-1]= min(c(header.col.cex, header.row.cex, data.cex))
		plot.matrix.cex[1,1]= min(c(header.col.cex, header.row.cex, data.cex, title.cex))

		plot.matrix.cex[1,-1] = min(c(header.col.cex))
		plot.matrix.cex[-1,1] = min(c(header.row.cex))
		plot.matrix.cex[-1,-1]= min(c(data.cex))
		plot.matrix.cex[1,1]= min(c(title.cex))
		
			

	}
	return(plot.matrix.cex)
}


###############################################################################
# plot.table.param - plot table with user specified parameters
###############################################################################
plot.table.param <- function
(
	plot.matrix, 				# matrix to plot
	smain = '', 				# text to draw in top,left cell
	plot.matrix.cex, 			# text size
	plot.matrix_bg.col, 		# background color
	frame.cell = T, 			# flag to draw border
	keep.all.same.cex = FALSE	# flag to auto-adjust text size
)
{	
	n = nrow(plot.matrix) 
	pages = unique(c(seq(0, n, by = 120), n))
	
	for(p in 1:(len(pages)-1)) {	
		rindex = (pages[p]+1) : pages[p+1]
		
		temp.table = matrix('', nr = len(rindex)+1, nc = ncol(plot.matrix)+1)
			temp.table[-1, -1] = plot.matrix[rindex,]
			temp.table[1, -1] = colnames(plot.matrix)
			temp.table[-1, 1] = rownames(plot.matrix)[rindex]
			temp.table[1, 1] = smain
				
		nr = nrow(temp.table) 
		nc = ncol(temp.table)
		
		par(mar = c(0, 0, 0, 0), cex = 0.5) 
		oldpar = make.table(nr, nc)

		text.cex = plot.matrix.cex[c(1, 1 + rindex), ]		
			text.cex = plot.table.helper.auto.adjust.cex(temp.table, keep.all.same.cex)
		bg.col = plot.matrix_bg.col[c(1, 1 + rindex), ]

		for(r in 1:nr) {
			for(c in 1:nc) {
				draw.cell( paste('', temp.table[r,c], '', sep=' '), r, c, 
					text.cex = text.cex[r,c], bg.col = bg.col[r,c], frame.cell = frame.cell)
			}
		}
	}	
}


###############################################################################
# plot.table.helper.color - default coloring scheme for highlight
###############################################################################
plot.table.helper.color <- function
(
	temp	# matrix to plot 
){
	# convert temp to numerical matrix
	temp = matrix(as.double(gsub('[%,$]', '', temp)), nrow(temp), ncol(temp))

	highlight = as.vector(temp)
	cols = rep(NA, len(highlight))
		ncols = len(highlight[!is.na(highlight)])
		cols[1:ncols] = rainbow(ncols, start = 0, end = 0.3)			
		
	o = sort.list(highlight, na.last = TRUE, decreasing = FALSE)
		o1 = sort.list(o, na.last = TRUE, decreasing = FALSE)
		highlight = matrix(cols[o1], nrow = nrow(temp))
		highlight[is.na(temp)] = NA
	return(highlight)
}



###############################################################################
# plot.table.helper.colorbar - plot colorbar
###############################################################################
plot.table.helper.colorbar <- function
(
	plot.matrix		# matrix to plot 
)
{
	nr = nrow(plot.matrix) + 1
	nc = ncol(plot.matrix) + 1
	
	c = nc
	r1 = 1
	r2 = nr
	
    rect((2*(c - 1) + .5), -(r1 - .5), (2*c + .5), -(r2 + .5), col='white', border='white')
   	rect((2*(c - 1) + .5), -(r1 - .5), (2*(c - 1) + .5), -(r2 + .5), col='black', border='black')
    
	y1= c( -(r2) : -(r1) )
	
	graphics::image(x = c(  (2*(c - 1) + 1.5) : (2*c + 0.5) ),
		y   = y1,
        z   = t(matrix(  y1  , ncol = 1)),
        col = t(matrix( rainbow(len( y1  ), start = 0, end = 0.3) , ncol = 1)),
        add = T)
}






###############################################################################
# Public table drawing routines
###############################################################################
#' Plot Table
#'
#' Create Plot of the given matrix
#'
#' @param plot.matrix matrix to plot
#' @param smain text to draw in top,left cell
#' @param text.cex text size, \strong{defaults to 1}
#' @param frame.cell flag to draw border, \strong{defaults to TRUE}
#' @param highlight flag to highlight data, \strong{defaults to FALSE}
#' @param colorbar flag to draw colorbar, \strong{defaults to FALSE}
#' @param keep_all.same.cex flag to auto-adjust text size, \strong{defaults to FALSE}
#'
#' @return nothing
#'
#' @examples
#' \dontrun{ 
#' # generate 1,000 random numbers from Normal(0,1) distribution 
#' data =  matrix(rnorm(1000), nc=10)
#'   colnames(data) = paste('data', 1:10, sep='')
#' 		
#' # compute Pearson correlation of data and format it nicely
#' temp = cor(data, use='complete.obs', method='pearson')
#'   temp[] = plota.format(100 * temp, 0, '', '%')
#' 		
#' # plot temp with colorbar, display Correlation in (top, left) cell	
#' plot.table(temp, smain='Correlation', highlight = TRUE, colorbar = TRUE)	
#' }
#' @export 
###############################################################################
plot.table <- function
(
	plot.matrix,				# matrix to plot
	smain = NULL, 				# text to draw in top,left cell
	text.cex = 1, 				# text size
	frame.cell = T, 			# flag to draw border
	highlight = F, 				# either flag to highlight or matrix with 
								# background colors
	colorbar = FALSE, 			# flag to draw colorbar
	keep_all.same.cex = FALSE	# flag to auto-adjust text size
)
{
	# deal with missing col/row names
	if( is.null(rownames(plot.matrix)) & is.null(colnames(plot.matrix)) ) {
		temp.matrix = plot.matrix
		if( nrow(temp.matrix) == 1 ) temp.matrix = rbind('', temp.matrix)
		if( ncol(temp.matrix) == 1 ) temp.matrix = cbind('', temp.matrix)
		
		plot.matrix = temp.matrix[-1, -1, drop = FALSE]
		colnames(plot.matrix) = temp.matrix[1, -1]
		rownames(plot.matrix) = temp.matrix[-1, 1]
		smain = iif(is.null(smain), temp.matrix[1, 1], smain)
		
	} else if( is.null(rownames(plot.matrix)) ) {
		temp.matrix = plot.matrix
		if( ncol(plot.matrix) == 1 ) temp.matrix = cbind('', temp.matrix)
		
		plot.matrix = temp.matrix[, -1, drop = FALSE]
		colnames(plot.matrix) = colnames(temp.matrix)[-1]
		rownames(plot.matrix) = temp.matrix[,1]
		smain = iif(is.null(smain), colnames(temp.matrix)[1], smain)
		
	} else if( is.null(colnames(plot.matrix)) ) {
		temp.matrix = plot.matrix
		if( nrow(temp.matrix) == 1 ) temp.matrix = rbind('', temp.matrix)
		
		plot.matrix = temp.matrix[-1, , drop = FALSE]
		rownames(plot.matrix) = rownames(temp.matrix)[-1]
		colnames(plot.matrix) = temp.matrix[1, ]
		smain = iif(is.null(smain), rownames(temp.matrix)[1], smain)
	}
	
	smain = iif(is.null(smain), '', smain)
		
	# remove N/As
	plot.matrix[which(trim(plot.matrix) == 'NA')] = ''
	plot.matrix[which(trim(plot.matrix) == 'NA%')] = ''
	plot.matrix[which(is.na(plot.matrix))] = ''
	
	# add space to the right if colorbar will be drawn
	if(colorbar) {
		plot.matrix = cbind(plot.matrix, '')
		if(!is.null(highlight)) if(!is.logical(highlight)) { highlight = cbind(highlight, NA) }
	}

	nr = nrow(plot.matrix) + 1
	nc = ncol(plot.matrix) + 1
	
	is_highlight = T
	if(is.logical(highlight)) { 
		is_highlight = highlight
		if(highlight) highlight = plot.table.helper.color(plot.matrix)
	}
	
	if(!is_highlight) {
		# default coloring scheme : alternate white/yellow each other row
		plot.matrix.cex = matrix(1, nr = nr, nc = nc )
		plot.matrix_bg.col = matrix('white', nr = nr, nc = nc )
			plot.matrix_bg.col[seq(1, nr, 2), ] = 'yellow'
			plot.matrix_bg.col[1,] = 'gray';			
			
		plot.table.param( plot.matrix, smain, plot.matrix.cex, plot.matrix_bg.col, 
			frame.cell, keep_all.same.cex)
	} else {
		plot.matrix.cex = matrix(1, nr = nr, nc = nc )
		plot.matrix_bg.col = matrix('white', nr = nr, nc = nc )
			plot.matrix_bg.col[1,] = 'gray'
			plot.matrix_bg.col[2:nr,2:nc] = highlight	
			
		plot.table.param(plot.matrix, smain, plot.matrix.cex, plot.matrix_bg.col, 
			frame.cell, keep_all.same.cex)
	}
	
	if(colorbar) plot.table.helper.colorbar(plot.matrix);
}





###############################################################################
# Examples of plot.table function
###############################################################################
plot.table.test <- function()
{
	# basic plot.table
		# define row and column titles
		mrownames = spl('row one,row two,row 3')
		mcolnames = spl('col 1,col 2,col 3,col 4')

		# create temp matrix with data you want to plot
		temp = matrix(NA, len(mrownames), len(mcolnames))
			rownames(temp) = mrownames
			colnames(temp) = mcolnames		
			temp[,] = matrix(1:12,3,4)

		png(filename = 'plot1.png', width = 500, height = 500, units = 'px', pointsize = 12, bg = 'white')		
	
		# plot temp, display current date in (top, left) cell
		plot.table(temp, format(as.Date(Sys.time()), '%d %b %Y'))
	
		dev.off()
	
	# plot.table with colorbar
		# generate 1,000 random numbers from Normal(0,1) distribution 
		data =  matrix(rnorm(1000), nc=10)
			colnames(data) = paste('data', 1:10, sep='')
		
		# compute Pearson correlation of data and format it nicely
		temp = cor(data, use='complete.obs', method='pearson')
			temp[] = plota.format(100 * temp, 0, '', '%')

		png(filename = 'plot2.png', width = 500, height = 500, units = 'px', pointsize = 12, bg = 'white')		
			
		# plot temp with colorbar, display Correlation in (top, left) cell	
		plot.table(temp, smain='Correlation', highlight = TRUE, colorbar = TRUE)	
		
		dev.off()
}




###############################################################################
# Construct Periodic table, like in Single Country Index Returns
# http://us.ishares.com/content/stream.jsp?url=/content/en_us/repository/resource/single_country_periodic_table.pdf&mimeType=application/pdf
###############################################################################
plot.periodic.table1 <- function(hist.returns)
{	
	n = ncol(hist.returns)
	
	# create temp matrix with data you want to plot
	temp = t(coredata(hist.returns))
		colnames(temp) = format(index.xts(hist.returns), '%Y')
		rownames(temp) = 1:n
			rownames(temp)[1] = ' Best '
			rownames(temp)[n] = ' Worst '

	# highlight each column
	col = plota.colors(n)
	highlight = apply(temp,2, function(x) col[order(x, decreasing = T)] )
	
	# sort each column
	temp[] = apply(temp,2, sort, decreasing = T)
	
	# format data as percentages
	temp[] = plota.format(100 * temp, 0, '', '%')	

	# plot temp and legend
	plot.table(temp, highlight = highlight)			
	plota.legend(colnames(hist.returns), col)	# , cex=1.5
}

plot.periodic.table2 <- function(hist.returns)
{	
	# create temp matrix with data you want to plot
	temp = t(coredata(hist.returns))
		colnames(temp) = format(index.xts(hist.returns), '%Y')

	# format data as percentages
	temp[] = plota.format(100 * temp, 0, '', '%')
		
	# highlight each column separately 		
	highlight = apply(temp,2, function(x) plot.table.helper.color(t(x)) )
	
	# plot temp with colorbar
	plot.table(temp, highlight = highlight, colorbar = TRUE)	
}	
