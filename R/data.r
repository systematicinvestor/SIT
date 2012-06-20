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
			colnames(temp) = temp[(hasHeader + 0), ]
			temp = temp[-c(1:(hasHeader + 0)), ]
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

###############################################################################
# Load CRB Commodities Index 
# http://www.jefferies.com/cositemgr.pl/html/ProductsServices/SalesTrading/Commodities/ReutersJefferiesCRB/IndexData/index.shtml
###############################################################################
get.CRB <- function()
{
	load.packages('gtools,gdata')
	
	#http://www.jefferies.com/html/ProductsServices/SalesTrading/Commodities/scripts/genExcel.pl?Index=RJCRB_Excess&StartDate=19940103&EndDate=20111202
	url = paste('http://www.jefferies.com/html/ProductsServices/SalesTrading/Commodities/scripts/genExcel.pl?Index=RJCRB_Total&StartDate=19940101&EndDate=', format(Sys.Date(), '%Y%m%d'), sep='')	
  	temp = read.xls(url)
  	
  	temp = as.matrix(temp[-c(1:7),])
  	
	out = repmat(as.double(temp[,2]), 1, 6)
   		colnames(out) = spl('Open,High,Low,Close,Volume,Adjusted')
   		out[, 'Volume'] = 0
	out = make.xts( out,  as.Date(temp[,1], '%m/%d/%y'))
		
	return(out)
}

get.CRB.test <- function()	
{
	#*****************************************************************
	# Load historical data
	#****************************************************************** 	
	CRB = get.CRB()
		
	load.packages('quantmod')	
	# http://etfdb.com/
	tickers = spl('GSG,DBC')		
	getSymbols(tickers, src = 'yahoo', from = '1970-01-01')
	
	#*****************************************************************
	# Compare different indexes
	#****************************************************************** 	
	out = na.omit(merge(Cl(CRB), Cl(GSG), Cl(DBC)))
		colnames(out) = spl('CRB,GSG,DBC')
	temp = out / t(repmat(as.vector(out[1,]),1,nrow(out)))
		
	layout(1:2)
	plota(temp, ylim=range(temp))
		plota.lines(temp[,1],col=1)
		plota.lines(temp[,2],col=2)
		plota.lines(temp[,3],col=3)
	plota.legend(colnames(temp),1:3)
			
	temp = compute.cor(temp / mlag(temp)- 1, 'pearson')
			temp[] = plota.format(100 * temp, 0, '', '%')
	plot.table(temp)	
}	


###############################################################################
# Get Dow Jones Components
# http://finance.yahoo.com/q/cp?s=^DJI+Components
###############################################################################
dow.jones.components <- function()
{
	url = 'http://finance.yahoo.com/q/cp?s=^DJI+Components'
	txt = join(readLines(url))

	# extract table from this page
	temp = extract.table.from.webpage(txt, 'Symbol', hasHeader = T)
	tickers = temp[, 'Symbol']

	return(tickers)
}
	
###############################################################################
# Get NASDAQ 100 Components
# http://www.nasdaq.com/markets/indices/nasdaq-100.aspx
###############################################################################
nasdaq.100.components <- function()
{
	url = 'http://www.nasdaq.com/markets/indices/nasdaq-100.aspx'
	txt = join(readLines(url))

	# extract table from this page
	temp = extract.table.from.webpage(txt, 'Symbol', hasHeader = 2)
	tickers = temp[, 'Symbol']

	return(tickers)
}
	

###############################################################################
# Get Sector SPDR Components
# http://www.sectorspdr.com/spdr/composition/?symbol=XLE
# tickers = spl('XLY,XLP,XLE,XLF,XLV,XLI,XLB,XLK,XLU')
# tickers.desc = spl('ConsumerCyclicals,ConsumerStaples,Energy,Financials,HealthCare,Industrials,Materials,Technology,Utilities')
###############################################################################
sector.spdr.components <- function(sector.etf = 'XLE')
{
	url = paste('http://www.sectorspdr.com/spdr/composition/?symbol=', sector.etf, sep='')
	txt = join(readLines(url))

	# extract table from this page
	temp = extract.table.from.webpage(txt, 'Symbol', hasHeader = T)
	tickers = temp[, 'Symbol']

	return(tickers)
}


###############################################################################
# S&P 500 Components
# http://en.wikipedia.org/wiki/List_of_S%26P_500_companies
###############################################################################
sp500.components <- function()
{
	url = 'http://en.wikipedia.org/wiki/List_of_S%26P_500_companies'
	txt = join(readLines(url))
	
	# extract table from this page	
	temp = extract.table.from.webpage(txt, 'Ticker', hasHeader = T)
	tickers = temp[, 'Ticker symbol']
	sector = temp[, 'GICS Sector']

	return(list(tickers=tickers, sector=sector))
}

# List of sites that keep SP500 Components
# http://www.s-p-500.com/stocks-a-b/
#http://www.forexpros.com/indices/us-spx-500-components
#http://marketvolume.com/indexes_exchanges/sp500_components.asp
#http://en.wikipedia.org/wiki/List_of_S%26P_500_companies
#http://en.wikipedia.org/wiki/Dow_Jones_Index


###############################################################################
# S&P 100 Components
# http://www.barchart.com/stocks/sp100.php
###############################################################################
sp100.components <- function()
{
	url = 'http://www.barchart.com/stocks/sp100.php'
	txt = join(readLines(url))
	
	# extract table from this page	
	temp = extract.table.from.webpage(txt, 'Components', hasHeader = T)
		i.start = grep('Name', temp[,2])
		tickers = trim(temp[-c(1:i.start), 1])
		
	return(tickers)	
}

###############################################################################
# Download FX qoutes: end of day and hourly
# http://www.fxhistoricaldata.com/EURUSD/
###############################################################################
getSymbols.fxhistoricaldata <- function
(
	Symbols, 
	type = spl('hour,day'),
	env = .GlobalEnv, 
	auto.assign = TRUE,
	download = FALSE	
) 
{		
	type = type[1]
	
	# setup temp folder
	temp.folder = paste(getwd(), 'temp', sep='/')
	dir.create(temp.folder, F)
	
	# read all Symbols
	for (i in 1:len(Symbols)) {	
		if(download) {
			# http://www.fxhistoricaldata.com/download/EURUSD?t=hour
			url = paste('http://www.fxhistoricaldata.com/download/', Symbols[i], '?t=', type, sep='')
			filename = paste(temp.folder, '/', Symbols[i], '_', type, '.zip', sep='')			
			download.file(url, filename,  mode = 'wb')
			
			# unpack
			unzip(filename, exdir=temp.folder)	
		}
	
		filename = paste(temp.folder, '/', Symbols[i], '_', type, '.csv', sep='')

		temp = read.delim(filename, header=TRUE, sep=',')		
			colnames(temp) = gsub('[X\\.|\\.]', '', colnames(temp))			
		out = make.xts(temp[,spl('OPEN,LOW,HIGH,CLOSE')], 
			strptime(paste(temp$DATE, temp$TIME), format='%Y%m%d %H:%M:%S'))
			
cat(i, 'out of', len(Symbols), 'Reading', Symbols[i], '\n', sep='\t')					
			
		if (auto.assign) {		
			assign(paste(gsub('\\^', '', Symbols[i]), type, sep='_'), out, env)	
		}	
	}
	if (!auto.assign) {
		return(out)
	} else {		
		return(env)				
	}	
}



 

###############################################################################
# getSymbols interface to tradingblox free futures and forex data
# http://www.tradingblox.com/tradingblox/free-historical-data.htm
# http://www.tradingblox.com/Data/DataOnly.zip
# Date, Open, High, Low, Close, Volume (zero for forex cash markets), 
# Open Interest (futures only), Delivery Month ( YYYYMM futures only), 
# Unadjusted Close (zero for forex cash markets)
###############################################################################			
getSymbols.TB <- function(
	env = .GlobalEnv, 
	auto.assign = TRUE,
	download = FALSE,
	type = c('Both', 'Futures', 'Forex'),
	rm.index =  'PB', 	# remove Pork Bellies because not traded
	clean = FALSE
) 
{
	# download zip archive
	if(download) {
		download.file('http://www.tradingblox.com/Data/DataOnly.zip', 'DataOnly.zip')
	}

	# setup temp folder
	temp.folder = paste(getwd(), 'temp', sep='/')
	dir.create(temp.folder, F)
		
	##*****************************************************************
	##	Unzip
	##****************************************************************** 
	temp.folder = paste(getwd(), '/', 'temp', sep='')

	# clean temp
	if(clean) shell('del /F /S /Q temp\\*.*', wait = TRUE)
	
	# unpack
	files = unzip('DataOnly.zip', exdir=temp.folder)	
	
	# read definitions, based on Financial Instrument Model Infrastructure for R package from http://r-forge.r-project.org/R/?group_id=316
 	def1 = try(read.csv('http://www.tradingblox.com/tradingblox/CSIUA/FuturesInfo.txt',skip=1,header=FALSE, stringsAsFactors=F),TRUE) 
	if(inherits(def1, 'try-error')) def1 = read.csv('FuturesInfo.txt',skip=1,header=FALSE, stringsAsFactors=F)			
 		def1 = def1[-match(rm.index, def1[,1]),]	
 		def1[,3] = 'Futures'
 
 	def2 = try(read.csv('http://www.tradingblox.com/tradingblox/CSIUA/ForexInfo.txt',skip=1,header=FALSE, stringsAsFactors=F),TRUE)
 	if(inherits(def2, 'try-error')) def2 = read.csv('ForexInfo.txt',skip=1,header=FALSE, stringsAsFactors=F)	 		
 		def2[,3] = 'Forex'
 	
 	def = rbind(def1[,1:4], def2[,1:4]) 			
 	if(type[1] == 'Futures') def = def1[,1:4]
 	if(type[1] == 'Forex') def = def2[,1:4]	
 	
		
	# read all files
	for( i in 1:nrow(def) ) {
		symbol = def[i,1]
		
		filename = paste(temp.folder, '/', def[i,3], '/', def[i,4], sep='')
		if(file.exists(filename)) {										
			fr <- read.csv(filename, header = FALSE) 
			fr <- make.xts(fr[,-1], as.Date(as.character(fr[,1]),'%Y%m%d'))
			colnames(fr) <- spl('Open,High,Low,Close,Volume,OpenInterest,DeliveryMonth,Unadjusted')[1:ncol(fr)]
			fr$Adjusted = fr$Close
			if (auto.assign) assign(symbol, fr, env)
cat(i, 'out of', nrow(def), 'Reading', symbol, format(index.xts(fr)[1],'%Y%m%d'), format(index.xts(fr)[nrow(fr)],'%Y%m%d'), '\n', sep='\t')		
		} else {
cat('\t\t\t Missing data for ', symbol, '\n');
		}
	}

	#*****************************************************************
	# Add symbolnames, symbol.descriptions, and symbol.groups
	#****************************************************************** 		
	index = match(ls(env), def[,1])	
	
	temp = def[index,1]
		names(temp) = def[index,1]
    env$symbolnames = temp

	temp = def[index,2]
		names(temp) = def[index,1]
    env$symbol.descriptions = temp

	temp = def[index,3]
		names(temp) = def[index,1]
    env$symbol.groups = temp    	    	
			
	#*****************************************************************
	# Process symbol descriptions to be more readable
	#****************************************************************** 	
	names = trim(gsub(pattern = '\\(.*?\\)', replacement = '', env$symbol.descriptions, perl = TRUE))
		names = trim(gsub('-NYMEX','',names,ignore.case =T))
		names = trim(gsub('-COMEX','',names,ignore.case =T))
		names = trim(gsub('-CBT','',names,ignore.case =T))
		names = trim(gsub('-CME-','',names,ignore.case =T))
		names = trim(gsub('-CME','',names,ignore.case =T))
		names = trim(gsub('-NYCE','',names,ignore.case =T))
		names = trim(gsub('-Globex','',names,ignore.case =T))
		names = trim(gsub('-FINEX','',names,ignore.case =T))
		names = trim(gsub('-CSCE','',names,ignore.case =T))
		names = trim(gsub(' w/Prj A','',names,ignore.case =T))
	 
	env$symbol.descriptions.print = names
	
	#*****************************************************************
	# Custom adjustments 
	#****************************************************************** 			
	data = env
	
	# fix DX time series
	data$DX['::2007:04:04', 'Unadjusted'] = coredata(data$DX['::2007:04:04']$Unadjusted * 10)
	
	#*****************************************************************
	# To compute returns and backtest, recreate each futures series:
	#
	#						(unadjusted-futures[t-1] + (back-adjusted-futures[t] - back-adjusted-futures[t-1]))	
	# futures-return[t] =	--------------------------------------------------------------------------------------------------	- 1
	#						unadjusted-futures[t-1]	
	#****************************************************************** 			
	for(i in data$symbolnames[data$symbol.groups != 'Forex']) {
		# adjust spot for roll overs
		spot = as.vector(data[[i]]$Unadjusted)
			dspot = spot - mlag(spot)
		futures = as.vector(data[[i]]$Adjusted)
			dfutures = futures - mlag(futures)
		index = which(round(dspot - dfutures,4) != 0 )
	
		spot.adjust.roll = spot
			spot.adjust.roll[(index-1)] = spot.adjust.roll[index] - dfutures[index]
			
		# compute returns
		reta = (mlag(spot.adjust.roll) + futures - mlag(futures)) / mlag(spot.adjust.roll)
			reta[1] = 1
			n = len(spot)
		
		new.series = cumprod(reta)
		data[[i]]$Close = spot[n] * new.series / new.series[n]		
		data[[i]]$Adjusted	= data[[i]]$Close
	}
	
	
	#*****************************************************************
	# Done 
	#****************************************************************** 						
	if (!auto.assign) {
		return(fr)
	} else {		
		return(env)
	}	
}





###############################################################################
# Kenneth R. French - Data Library
# http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html
###############################################################################
# http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors.zip
# http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_weekly.zip
# http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_daily.zip
#
# data2 = get.fama.french.data('F-F_Research_Data_Factors', periodicity = 'weeks',download = F, clean = F)
# data3 = get.fama.french.data('6_Portfolios_2x3', periodicity = 'days',download = F, clean = F)	
###############################################################################
get.fama.french.data <- function(
	name = c('F-F_Research_Data_Factors', 'F-F_Research_Data_Factors'),
	periodicity = c('days','weeks', 'months'),
	download = FALSE,
	clean = FALSE
) 
{
	# map periodicity
	map = c('_daily', '_weekly', '')
		names(map) = c('days','weeks', 'months')
	
	# url
	filename.zip = paste(name[1], map[periodicity[1]], '.zip', sep='')
	filename.txt = paste(name[1], map[periodicity[1]], '.txt', sep='')
	url = paste('http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/', filename.zip, sep='')
				
	# download zip archive
	if(download) {
		download.file(url, filename.zip)
	}

	# setup temp folder
	temp.folder = paste(getwd(), 'temp', sep='/')
	dir.create(temp.folder, F)
		
	##*****************************************************************
	##	Unzip
	##****************************************************************** 
	temp.folder = paste(getwd(), '/', 'temp', sep='')

	# clean temp
	if(clean) shell('del /F /S /Q temp\\*.*', wait = TRUE)
	
	# unpack
	files = unzip(filename.zip, exdir=temp.folder)	

	# read data
	filename = paste(temp.folder, '/', filename.txt, sep='')
	out = readLines(filename)
		index = which(nchar(out) == 0)
	
	data.index = grep('^[ 0-9\\.\\+-]+$', out)
		temp.index = which(diff(data.index) > 1)
	data.index = matrix(data.index[sort(c(1, temp.index, temp.index+1, len(data.index)))], nc=2, byrow=T)

	# extract sections	
	data = list()	
	for(i in 1:nrow(data.index)) {
		start.index = index[which( index > data.index[i,1] ) - 1][1] + 1
		end.index = data.index[i,1] - 1
		n.index = end.index - start.index + 1

		# column names
		name = 'data'
		colnames = scan(text = out[start.index], what='', quiet=T)
		if(n.index == 2) {
			name = trim(out[start.index])
			colnames = scan(text = out[end.index], what='', quiet=T)
		} else if(n.index > 2) {
			name = trim(out[start.index])
			colnames0 = scan(text = out[(end.index-1)], what='', quiet=T)
			colnames1 = scan(text = out[end.index], what='', quiet=T)
			colnames = paste(rep(colnames0, each = len(colnames1) / len(colnames0)), colnames1, sep='.')			
		}
		colnames = gsub('-', '.', colnames)
		#out[start.index:end.index]

		# re-read data	
		temp =  matrix(scan(filename, what = double(), quiet=T,
			skip = (data.index[i,1]-1),  
			nlines = (data.index[i,2] - data.index[i,1]+1))
			, nc=len(colnames)+1, byrow=T)
		
		date.format = '%Y%m%d'	
		date.format.add = ''
		date.format.n = nchar(paste(temp[1,1]))
		
		if( date.format.n == 6 )
			date.format.add = '01'		
		else if( date.format.n == 4 )
			date.format.add = '0101'		
		
		data[[name]] = make.xts(temp[,-1], as.Date(paste(temp[,1], date.format.add, sep=''),date.format))
			colnames(data[[name]]) = colnames		
	}
	return( data )
}





