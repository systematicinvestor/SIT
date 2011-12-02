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
# Collection of routines to work with data
# Copyright (C) 2011  Michael Kapler
#
# For more information please visit my blog at www.SystematicInvestor.wordpress.com
# or drop me a line at TheSystematicInvestor at gmail
###############################################################################

###############################################################################
# extract.table.from.webpage
###############################################################################
extract.table.from.webpage <- function
(
	txt, 		# source text of webpage
	marker,		# key-phrase(s) located in the table to extract
	hasHeader=T	# flag if table has a header
)
{
	tryCatch({		
		# find location of data
		marker = spl(marker)
		pos1=1
		
		for(i in 1:len(marker)) {
			pos1 = regexpr(marker[i], substr(txt, pos1, nchar(txt))) + pos1
		}
		
		# find start/end of table
		pos0 = tail(gregexpr('<table', substr(txt, 1, pos1))[[1]], 1)
		pos2 = head(gregexpr('</table', substr(txt, pos1, nchar(txt)))[[1]], 1)
		temp =  substr(txt, pos0, pos1 + pos2 - 2)
	
		# remove all formating	
		temp = gsub(pattern = '<br>', replacement = '', temp, perl = TRUE) 
		
		temp = gsub(pattern = '</tr>', replacement = ';row;', temp, perl = TRUE) 
		temp = gsub(pattern = '</td>', replacement = ';col;', temp, perl = TRUE) 
		temp = gsub(pattern = '</th>', replacement = ';col;', temp, perl = TRUE) 
						
		temp = gsub(pattern = '<.*?>', replacement = '', temp, perl = TRUE) 
		
		temp = gsub(pattern = '\r', replacement = '', temp, perl = TRUE) 
		temp = gsub(pattern = '\n', replacement = '', temp, perl = TRUE) 
		temp = gsub(pattern = '\t', replacement = '', temp, perl = TRUE) 
		temp = gsub(pattern = '&nbsp;', replacement = '', temp, perl = TRUE) 
		temp = gsub(pattern = '&amp;', replacement = '', temp, perl = TRUE) 
		temp = gsub(pattern = '&raquo;', replacement = '', temp, perl = TRUE) 		
				
		# parse into matrix	
		temp = lapply( strsplit(temp, ';row;'), strsplit, ';col;')	
		n = max( sapply(temp[[1]], function(x) len(x)) )
		temp = t( sapply(temp[[1]], function(x) x[1:n]) )
		
		if(hasHeader) {
			colnames(temp) = temp[1, ]
			temp = temp[-1, ]
		}

	}, error = function(ex) {
		temp <<- txt
	}, finally = {
		return(temp)
	})
}
 	
###############################################################################
# Test for extract.table.from.webpage function
###############################################################################
extract.table.from.webpage.test <- function()
{
	load.packages('quantmod')

	Symbol = 'IBM'	
	
	# download Key Statistics from yahoo	
	url = paste('http://finance.yahoo.com/q/ks?s=', Symbol, sep = '')
	txt = join(readLines(url))

	# extract Valuation Measures table from this page
	temp = extract.table.from.webpage(txt, 'Market Cap', hasHeader = F)
		temp = rbind(c('', Symbol), temp)	# add header row

		
	# download IBM price history from Yahoo
	data = getSymbols(Symbol, from = '1980-01-01', auto.assign = FALSE)
		
	# prepare IBM data for 2010:2011 and compute 50 days moving average
	y = data['2010::2011']
	sma50 = SMA(Cl(y), 50)
	
	png(filename = 'plot1.png', width = 500, height = 500, units = 'px', pointsize = 12, bg = 'white')
	
	# plote candles and volume and table	
	layout(c(1,1,2,3,3))		
	
	plota(y, type = 'candle', main = Symbol, plotX = F)
		plota.lines(sma50, col='blue')
		plota.legend(c(Symbol,'SMA 50'), 'green,blue', list(y,sma50))
		
	y = plota.scale.volume(y)
	plota(y, type = 'volume')		
		
	plot.table(temp)	
	
	dev.off()		
}


###############################################################################
# Properly handling quote when reading text from CGIwithR
###############################################################################
scan.text <- function
(
	string, 
	what = character(0), 
	...
)
{
    tc = textConnection(string)
    result = scan(tc, what = what, quiet = TRUE, ...)
    close(tc)
    return(result)
}		

###############################################################################
# Parsing BATS qoutes
# http://www.batstrading.com/book/AA/data/
###############################################################################			
parse.bats <- function(txt) 
{
	tryCatch({			
		# remove commas within qoutes
		temp = scan.text(txt, what = '', sep = ',', quote='\"')
			temp = paste(gsub(',', '', temp), collapse = ',')
	
		# find rows
		poss = gregexpr('\\[[^\\]]+\\]', temp, perl=T)[[1]]					
			rows = sapply(1:length(poss), function(i) substr(temp,poss[i],poss[i] + attr(poss, 'match.length')[i]-1))
			rows = gsub(pattern = '[\\[\\]]', replacement = '', rows, perl=TRUE)	
			temp = t( sapply(strsplit(rows, ','), function(x) x) )
	}, error = function(ex) {
		temp <<- txt
	}, finally = {
		return(temp)
	})
}		
	
parse.bats.test <- function()
{
	Symbol = 'IBM'	
	
	# download BATS qoute
	url = paste('http://www.batstrading.com/book/', Symbol, '/data/', sep = '')
	txt = join(readLines(url))
	
	# trades, bids, asks, 
	pos = regexpr('\"trades\":((?:(?!]]).)*)]]', txt, perl = T)
	parse.bats( substr(txt, pos, pos + attr(pos, 'match.length')-1) ) 	

}

###############################################################################
# getSymbols interface to BATS qoutes
###############################################################################			
getSymbols.bats <- function
(
	Symbols, 
	env = .GlobalEnv, 
	auto.assign = TRUE
)
{
	for (i in 1:len(Symbols)) {
		# download BATS qoute
		url = paste('http://www.batstrading.com/book/', trim(Symbols[i]), '/data/', sep = '')
		txt = join(readLines(url))

		# trades
		pos = regexpr('\"trades\":((?:(?!]]).)*)]]', txt, perl = T)
		temp = substr(txt, pos, pos + attr(pos, 'match.length')-1)
		temp = parse.bats( temp )
		
		# volume
		pos = regexpr('\"volume\":([^,]*),', txt, perl = T)
		volume = substr(txt, attr(pos, 'capture.start'), attr(pos, 'capture.start') + attr(pos, 'capture.length')-1)
		
		# date
   		date = as.Date(Sys.Date())
   			
   		temp = as.double( temp[,3] )
		out = matrix(double(1),len(date), 6)
   			colnames(out) = spl('Open,High,Low,Close,Volume,Adjusted')	
   			out[,1] = tail(temp,1)
   			out[,2] = range(temp)[2]
   			out[,3] = range(temp)[1]
   			out[,4] = temp[1]
   			out[,5] = as.double( volume )
   			out[,6] = out[,4]	# Adjusted = Close   			
		out = make.xts( out,  date)
			
		if (auto.assign) {		
			assign(gsub('\\^', '', Symbols[i]), out, env)	
		}	
	}
	if (!auto.assign) {
		return(out)
	} else {		
		return(env)				
	}	
}

###############################################################################
# getSymbols interface to Yahoo today's delayed qoutes
# based on getQuote.yahoo from quantmod package
###############################################################################			
getQuote.yahoo.today <- function
(
	Symbols, 
	env = .GlobalEnv, 
	auto.assign = TRUE
)
{
	what = yahooQF(names = spl('Symbol,Last Trade Date,Open,Days High,Days Low,Last Trade (Price Only),Volume'))
	names = spl('Symbol,Date,Open,High,Low,Close,Volume')
	
	all.symbols = lapply(seq(1, len(Symbols), 100), function(x) na.omit(Symbols[x:(x + 99)])) 
	
	for(i in 1:len(all.symbols)) {
		# download
		url = paste('http://download.finance.yahoo.com/d/quotes.csv?s=', 
			join( trim(all.symbols[[i]]), ','),
			'&f=', what[[1]], sep = '')
		tempi = readLines(url)
		
		for (j in 1:len(tempi)) {
			tempj = scan.text(tempi[j], what = '', sep = ',', quote='\"')
			names(tempj) = names
	
			out = matrix(double(1),1, 6)
	   			colnames(out) = spl('Open,High,Low,Close,Volume,Adjusted')	
	   			out[1,] = as.double( tempj[spl('Open,High,Low,Close,Volume,Close')] )
			out = make.xts( out,  as.Date(tempj['Date'], '%m/%d/%Y'))
				
			if (auto.assign) {		
				assign(gsub('\\^', '', tempj['Symbol']), out, env)	
			}	
		}
	}
	if (!auto.assign) {
		return(out)
	} else {		
		return(env)				
	}	
}

###############################################################################
# Pricing Zero Coupon Bond (i.e. yield to price)
# http://thinkanddone.com/finance/valuation-of-zero-coupon-bonds.html
###############################################################################
PricingZeroCouponBond <- function
( 
	yield, 
	timetomaturity, 
	parvalue = 100 
)
{
	parvalue / ( 1 + yield ) ^ timetomaturity  
}

###############################################################################
# Convert Historical TBills rates to Total Returns
# http://timelyportfolio.blogspot.com/2011/04/historical-sources-of-bond-returns_17.html
###############################################################################
processTBill <- function 
( 
	yields, 
	timetomaturity = 1/4
)
{
	yield = coredata(yields) / 100
	
	# price return
	pr = sapply( yield, function(x) PricingZeroCouponBond(x, timetomaturity) )
		pr = ROC(pr)
		pr[1] = 0

	# interest return
	ir = mlag(yield, nlag=1) / 12
		ir[1] = 0
	
	# total return
	tr = pr + ir
		
	#out = as.xts( cbind(pr, ir, tr), index(yields) )
	#	colnames(out) = spl('PR,IR,TR')
		
		
	close.price = cumprod(1 + pr)
	adjusted.price = cumprod(1 + tr)
		
	out = as.xts( cbind(close.price, adjusted.price), index(yields) )
		colnames(out) = spl('Close,Adjusted')
		
	return(out)
}

