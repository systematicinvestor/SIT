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
			temp = temp[-c(1:(hasHeader + 0)), ,drop=F]
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
# http://timelyportfolio.blogspot.ca/2012/11/cashopportunity-lost-or-opportunity.html
###############################################################################
processTBill <- function 
( 
	yields, 
	timetomaturity = 1/4,
	frequency = 365
)
{
	yield = coredata(yields) / 100
	
	# price return
	pr = sapply( yield, function(x) PricingZeroCouponBond(x, timetomaturity) )
		pr = ROC(pr, type='discrete')
		pr[1] = 0

	# interest return
	ir = (1+mlag(yield, nlag=1))^(1 / frequency)-1
	#ir = mlag(yield, nlag=1) / frequency 
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


processTBill.test <- function()	
{
	#*****************************************************************
	# Get 1 year t-bill
	#****************************************************************** 	
	quantmod::getSymbols("GS1", src = "FRED")
	ir = (1 + mlag(GS1) / 100) ^ (1/12) - 1
		ir[1] = 0
		
	out = processTBill(GS1, timetomaturity = 1,12)

	plota(cumprod(1 + ir), type='l', log = 'y')
		plota.lines(Ad(out), type='l', col='red')

	#*****************************************************************
	# Get 3 years t-bill
	#****************************************************************** 	
	SHY = getSymbols('SHY', src='yahoo', auto.assign = FALSE)	
		
	tbill.m = quantmod::getSymbols('GS3', src='FRED', auto.assign = FALSE)	
	tbill.d = quantmod::getSymbols('DGS3', src='FRED', auto.assign = FALSE)	
	timetomaturity = 3
		
	compute.raw.annual.factor(tbill.d)
	compute.raw.annual.factor(tbill.m)
	
	# compute returns
	tbill.m = processTBill(tbill.m, timetomaturity = timetomaturity, 12)
		#index(tbill.m) = as.Date(paste('1/', format(index(tbill.m), '%m/%Y'), sep=''), '%d/%m/%Y')

	tbill.d[] = ifna.prev(tbill.d)		
	tbill.d = processTBill(tbill.d, timetomaturity = timetomaturity,261)

	
	# scale to start at 1	
	dates = '2003::'
	tbill.m = tbill.m[dates,2]
		tbill.m = tbill.m / as.double(tbill.m[1])
	tbill.d = tbill.d[dates,2]
		tbill.d = tbill.d / as.double(tbill.d[1])
	SHY = Ad(SHY[dates,])
		SHY = SHY / as.double(SHY[1])
		
	# plot
	plota(tbill.d, type='l')		
		plota.lines(tbill.m, type='s', col='blue')								
		plota.lines(SHY, type='l', col='red')
	plota.legend('Daily 3YR T-Bills,Monthly 3YR T-Bills,SHY','black,blue,red')

}


###############################################################################
# Load CRB Commodities Index 
# http://www.jefferies.com/cositemgr.pl/html/ProductsServices/SalesTrading/Commodities/ReutersJefferiesCRB/IndexData/index.shtml
###############################################################################
# ... parameters for read.xls function
# i.e. CRB = get.CRB(perl = 'c:/perl/bin/perl.exe')
###############################################################################
get.CRB <- function(...)
{
	load.packages('gtools,gdata')
	
	#http://www.jefferies.com/html/ProductsServices/SalesTrading/Commodities/scripts/genExcel.pl?Index=RJCRB_Excess&StartDate=19940103&EndDate=20111202
	url = paste('http://www.jefferies.com/html/ProductsServices/SalesTrading/Commodities/scripts/genExcel.pl?Index=RJCRB_Total&StartDate=19940101&EndDate=', format(Sys.Date(), '%Y%m%d'), sep='')	
  	temp = read.xls(url, ...)
  	
  	temp = as.matrix(temp[-c(1:7),])
  	
	out = repmat(as.double(temp[,2]), 1, 6)
   		colnames(out) = spl('Open,High,Low,Close,Volume,Adjusted')
   		out[, 'Volume'] = 0
	#out = make.xts( out, as.Date(temp[,1], '%m/%d/%y'))
	out = make.xts( out, as.POSIXct(temp[,1], tz = Sys.getenv('TZ'), format='%m/%d/%y'))	
		indexClass(out) = 'Date'	
	
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
	
	
	layout(1:3)	
	plota.matplot(CRB[,c('Close','Adjusted')])	
	plota.matplot(DBC[,c('DBC.Close','DBC.Adjusted')])	
	plota.matplot(GSG[,c('GSG.Close','GSG.Adjusted')])	
	
	
	layout(1)				
	comm = extend.data(DBC, CRB, scale=T)
	plota(comm, type='l', col=1)
		plota.lines(CRB*0.078, type='l', lwd=5, col=col.add.alpha(2,150))
		plota.lines(DBC, type='l', lwd=5, col=col.add.alpha(3,150))
		plota.lines(comm, type='l', col=1)
	plota.legend('comm,CRB,DBC', 1:3, list(comm,CRB,DBC))
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
# iShares FTSE 100 (ISF)
# http://uk.ishares.com/en/rc/products/ISF/all-holdings/
# http://www.londonstockexchange.com/exchange/prices-and-markets/stocks/indices/constituents-indices.html?index=UKX
# Yahoo ticker for UK stocks ABF.L
###############################################################################
ftse100.components <- function()
{
	# get holdings from uk.ishares.com
	url = 'http://uk.ishares.com/en/rc/products/ISF/all-holdings/'
	txt = join(readLines(url))
	
	# extract table from this page		
	txt = gsub('&#37;','%',txt)
	temp = extract.table.from.webpage(txt, 'Security', hasHeader = T)
	
	temp = trim(temp)
		colnames(temp) = temp[1,]
		temp = temp[-1,]		
	holdings = temp
		
	
	# get ISIN to ticker map from www.londonstockexchange.com
	page.label = ''	
	ticker2ISIN = c()
	for(i in 1:100) {	
		cat(i,'\n')
		
		# download
		url = paste('http://www.londonstockexchange.com/exchange/prices-and-markets/stocks/indices/constituents-indices.html?index=UKX&page=', i, sep='')
		txt = join(readLines(url))
	
		# get page label	
		pos = regexpr('Page [0-9]+ of [0-9]+', txt, ignore.case = T)
		page.label.new = substr(txt, pos, pos + attr(pos, 'match.length')-1)
		
		if(page.label == page.label.new) break
		page.label = page.label.new
		
		# extract table
		temp.table = extract.table.from.webpage(txt, 'Price', hasHeader = T)
			colnames(temp.table)[1] = 'tickers'

		# extract links
		temp = gsub(pattern = '<a', replacement = '<td>', txt, perl = TRUE)
		temp = gsub(pattern = '</a>', replacement = '</td>', temp, perl = TRUE)	
	
		temp = extract.table.from.webpage(temp, 'Price', hasHeader = T)
			pos = regexpr('fourWayKey=', temp[,2])
		ISIN = as.vector(sapply(1:nrow(temp), function(j) 
			substr(temp[j,2], pos[j] + attr(pos, 'match.length')[j], pos[j] + attr(pos, 'match.length')[j] + 12 - 1)
			))
		
		
		ticker2ISIN = rbind(ticker2ISIN, cbind(temp.table[,spl('ticker,Name,Price'), drop=F], ISIN))
	}
		
	ISIN = intersect(holdings[,'ISIN'],ticker2ISIN[,'ISIN'])
	holdings = cbind(holdings[match(ISIN, holdings[,'ISIN']), ],
				ticker2ISIN[match(ISIN, ticker2ISIN[,'ISIN']), spl('ticker,Name,Price')])

	return(apply(holdings, 2, list))
}


###############################################################################
# Get the latest prices from the Google finance:
# http://digitalpbk.com/stock/google-finance-get-stock-quote-realtime
#  http://finance.google.com/finance/info?client=ig&q=MSFT,AAPL,NYSE:RY
###############################################################################
#getQuote.google(spl('MSFT,AAPL,IBM'))
getQuote.google <- function(tickers) {
	url = paste('http://finance.google.com/finance/info?client=ig&q=', join(tickers,','), sep='')
	txt = join(readLines(url))	
		temp = gsub(':', ',', txt) 	
		temp = scan(text = temp, what='', sep=',', quiet=T)
	temp = matrix(trim(temp), nr=len(temp)/len(tickers), byrow=F)
	
	index = match(spl('t,l,lt'), tolower(temp[,1]))+1
		names(index) = spl('ticker,last,date')
	
	last = as.double(temp[index['last'],])
	date = strptime(temp[index['date'],],format=' %b %d, %H,%M')
	
	out = data.frame(last,date)
		rownames(out) = temp[index['ticker'],]
	out
}

# an xml alternative
# http://www.jarloo.com/google-stock-api/	
#  http://www.google.com/ig/api?stock=AAPL&stock=GOOG
#getQuote.google.xml(spl('MSFT,AAPL,NYSE:RY'))
getQuote.google.xml <- function(tickers) {
	url = paste('http://www.google.com/ig/api?', paste('stock=',tickers, '&', sep='', collapse=''), sep='')
	txt = join(readLines(url))	
	
		temp = txt		
		temp = gsub('<finance.*?>', '', temp, perl = TRUE) 
		temp = gsub('</finance>', '', temp, perl = TRUE) 
		temp = gsub('<xml.*?>', '', temp, perl = TRUE) 
		temp = gsub('</xml.*?>', '', temp, perl = TRUE) 
		temp = gsub('<\\?xml.*?>', '', temp, perl = TRUE) 
		temp = gsub('data=', '', temp, perl = TRUE) 
		temp = gsub('/><', ' ', temp) 	
		temp = gsub('>', '', temp) 	
		temp = gsub('<', '', temp) 	
	temp = scan(text = temp, what='', sep=' ', quiet=T)
	temp = matrix(trim(temp), nr=len(temp)/len(tickers), byrow=F)
	
	cnames = spl('trade_date_utc,trade_time_utc,symbol,last,high,low,volume,open,avg_volume,market_cap,y_close')
	index = match(cnames, tolower(temp[,1]))+1
		names(index) = cnames
		
	date = strptime(paste(temp[index['trade_date_utc'],], temp[index['trade_time_utc'],]), format='%Y%m%d %H%M%S',tz='UTC')
	date = as.POSIXct(date, tz = Sys.getenv('TZ'))
	
	out = data.frame(t(temp[index[-c(1:3)],]))
		colnames(out) = cnames[-c(1:3)]	
		rownames(out) = temp[index['symbol'],]
	out
}
	
###############################################################################
# extend GLD and SLV historical prices with data from KITCO
# http://wikiposit.org/w?filter=Finance/Commodities/
# http://www.hardassetsinvestor.com/interviews/2091-golds-paper-price.html
###############################################################################
extend.GLD <- function(GLD) {
	extend.data(GLD, KITCO.data('Gold.PM') / 10)
}
extend.SLV <- function(SLV) {
	extend.data(SLV, KITCO.data('Silver'))
}


KITCO.data <- function
(
	symbol = spl('Gold.AM,Gold.PM,Silver,Platinum.AM,Platinum.PM,Palladium.AM,Palladium.PM')
)
{
	url = 'http://wikiposit.org/w?action=dl&dltypes=comma%20separated&sp=daily&uid=KITCO'
	temp = read.csv(url, skip=4, header=TRUE, stringsAsFactors=F)
		
	#hist = make.xts(as.double(temp[,symbol]), as.Date(temp[,1], '%d-%b-%Y'))
	hist = make.xts(as.double(temp[,symbol]), as.POSIXct(temp[,1], tz = Sys.getenv('TZ'), format='%d-%b-%Y'))	
		indexClass(hist) = 'Date'
		colnames(hist)='Close'
	return( hist[!is.na(hist)] )
}


# gold = extend.GLD(data$GLD)
# comm = extend.data(data$DBC, get.CRB(), scale=T)
extend.data <- function
(
	current,
	hist,
	scale = F
) 
{
	# find Close in hist
	close.index = find.names('Close', colnames(hist))$Close
	if(is.na(close.index)) close.index = 1

	if(scale) {
		# find first common observation in current and hist series
		common = merge(Cl(current), hist[,close.index], join='inner')
		
		scale = as.numeric(common[1,1]) / as.numeric(common[1,2])
							
		hist = hist * scale
	}
	
	# subset history before current
	hist = hist[format(index(current[1])-1,'::%Y:%m:%d'),,drop=F]
	
	
	if( ncol(hist) != ncol(current) )	
		hist = make.xts( repCol(hist[,close.index], ncol(current)), index(hist))
	colnames(hist) = colnames(current)
		
	rbind( hist, current )
}


###############################################################################
# Bundes Bank - long history of gold prices
# http://www.bundesbank.de/Navigation/EN/Statistics/Time_series_databases/Macro_economic_time_series/its_list_node.html?listId=www_s331_b01015_3
# http://wikiposit.org/w?filter=Finance/Commodities/
###############################################################################  
bundes.bank.data <- function(symbol) {
	url = paste('http://www.bundesbank.de/cae/servlet/CsvDownload?tsId=', symbol, '&its_csvFormat=en&mode=its', sep='')
	temp = read.csv(url, skip=5, header=F, stringsAsFactors=F)

	#hist = make.xts(as.double(temp[,2]), as.Date(temp[,1], '%Y-%m-%d'))		
	hist = make.xts(as.double(temp[,2]), as.POSIXct(temp[,1], tz = Sys.getenv('TZ'), format='%Y-%m-%d'))
		indexClass(hist) = 'Date'			
		colnames(hist)='Close'
	return( hist[!is.na(hist)] )
}
bundes.bank.data.gold <- function() {
	bundes.bank.data('BBK01.WT5512')
}


###############################################################################
# Pacific Exchange Rate Service - FX rates
# Daily data is maximum for 4 years
# http://fx.sauder.ubc.ca/data.html
# http://fx.sauder.ubc.ca/cgi/fxdata?b=USD&c=AUD&c=GBP&c=CAD&c=NOK&c=EUR&c=JPY&c=NZD&c=SEK&c=CHF&rd=&fd=1&fm=1&fy=2011&ld=31&lm=12&ly=2012&y=daily&q=volume&f=csv&o= 
#
# Example
# base.cur = 'USD'
# target.curs = 'AUD,CAD,EUR'
# fx.data = rbind(fx.sauder.data(2000, 2003, base.cur, target.curs), 
#				fx.sauder.data(2004, 2007, base.cur, target.curs), 
#				fx.sauder.data(2008, 2011, base.cur, target.curs),
#				fx.sauder.data(2012, 2012, base.cur, target.curs))
###############################################################################  
fx.sauder.data <- function(start.year, end.year, base.cur, target.curs) {
	url = paste('http://fx.sauder.ubc.ca/cgi/fxdata?b=', base.cur, join(paste('&c=', spl(target.curs), sep='')), '&rd=&fd=1&fm=1&fy=', start.year, '&ld=31&lm=12&ly=', end.year, '&y=daily&q=volume&f=csv&o=', sep='')
	temp = read.csv(url, skip=1, header=T, stringsAsFactors=F)

	#hist = make.xts(as.matrix(temp[,-c(1:3)]), as.Date(temp[,2], '%Y/%m/%d'))		
	hist = make.xts(as.matrix(temp[,-c(1:3)]), as.POSIXct(temp[,2], tz = Sys.getenv('TZ'), format='%Y/%m/%d'))
		indexClass(hist) = 'Date'
		colnames(hist) = gsub(paste('.', base.cur, sep=''), '', colnames(hist))
		
	return( hist[!is.na(hist[,1]),] )
}


###############################################################################
# Download historical prices from Pi Trading - Free Market Data
# http://pitrading.com/free_market_data.htm
###############################################################################
getSymbols.PI <- function
(
	Symbols, 
	env = .GlobalEnv, 
	auto.assign = TRUE,
	download = TRUE	
) 
{
	# setup temp folder
	temp.folder = paste(getwd(), 'temp', sep='/')
	dir.create(temp.folder, F)
	
	# read all Symbols
	for (i in 1:len(Symbols)) {	
		if(download) {
			# http://pitrading.com/free_eod_data/SPX.zip
			url = paste('http://pitrading.com/free_eod_data/', Symbols[i], '.zip', sep='')
			filename = paste(temp.folder, '/', Symbols[i], '.zip', sep='')			
			download.file(url, filename,  mode = 'wb')
			
			# unpack
			unzip(filename, exdir=temp.folder)	
		}
		
		filename = paste(temp.folder, '/', Symbols[i], '.txt', sep='')
		
		temp = read.delim(filename, header=TRUE, sep=',')		
		#out = make.xts(temp[,-1], as.Date(temp[,1],'%m/%d/%Y'))
		out = make.xts(temp[,-1], as.POSIXct(temp[,1], tz = Sys.getenv('TZ'), format='%m/%d/%Y'))
			indexClass(out) = 'Date'
			out$Adjusted = out$Close
			
cat(i, 'out of', len(Symbols), 'Reading', Symbols[i], '\n', sep='\t')					
			
		if (auto.assign) {		
			assign(paste(gsub('\\^', '', Symbols[i]), sep='_'), out, env)	
		}	
	}
	if (!auto.assign) {
		return(out)
	} else {		
		return(env)				
	}	
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
# Download historical data for G10
# The PowerShares DB G10 Currency Harvest Fund
# http://www.invescopowershares.com/products/overview.aspx?ticker=DBV
#
# The G10 currency universe from which the Index selects currently includes 
# U.S. dollars, 
# euros, 
# Japanese yen, 
# Canadian dollars, 
# Swiss francs, 
# British pounds, 
# Australian dollars, 
# New Zealand dollars, 
# Norwegian krone and 
# Swedish krona
###############################################################################
get.G10 <- function
(
	type = spl('currency')
)
{
	if( type[1] != 'currency') {
		cat('Warning:', type[1], 'is not yet implemented in getG10 function\n')
		return()
	}

	# FRED acronyms for daily FX rates
map = '
FX          FX.NAME        
DEXUSAL     U.S./Australia 
DEXUSUK     U.S./U.K.      
DEXCAUS     Canada/U.S.    
DEXNOUS     Norway/U.S.    
DEXUSEU     U.S./Euro      
DEXJPUS     Japan/U.S.     
DEXUSNZ     U.S./NewZealand
DEXSDUS     Sweden/U.S.    
DEXSZUS     Switzerland/U.S.
'
	
	map = matrix(scan(text = map, what='', quiet=T), nc=2, byrow=T)
		colnames(map) = map[1,]
		map = data.frame(map[-1,], stringsAsFactors=F)

	# convert all quotes to be vs U.S.
	convert.index = grep('DEXUS',map$FX, value=T)	

    #*****************************************************************
    # Load historical data
    #****************************************************************** 
    load.packages('quantmod')

	# load fx from fred
	data.fx <- new.env()
	quantmod::getSymbols(map$FX, src = 'FRED', from = '1970-01-01', env = data.fx, auto.assign = T)		
		for(i in convert.index) data.fx[[i]] = 1 / data.fx[[i]]

	# extract fx where all currencies are available
	bt.prep(data.fx, align='remove.na')
	fx = bt.apply(data.fx, '[')
	
	return(fx)
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
	index = match(ls(env)[ na.omit(match(def[,1], ls(env))) ], def[,1])	

	
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
	
	# fix DX time series - fixed by the Trading Blox
	# if(!is.null(data$DX)) data$DX['::2007:04:04', 'Unadjusted'] = coredata(data$DX['::2007:04:04']$Unadjusted * 10)
	
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

