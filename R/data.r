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

#' @export 
find.tokens <- function
(
  txt,    # source text
  marker,   # key-phrase(s) to find
  pos = 1,  # position to start searching at
  pos.start = T
)
{
  # find location of data
  marker = spl(marker)
    
  for(i in 1:len(marker)) {
    if( pos < 2 )
      pos1 = regexpr(marker[i], txt) 
    else    
      pos1 = regexpr(marker[i], substr(txt, pos, nchar(txt))) 

    if( pos1 < 0 )  
      return(pos1)
    else {
      if( pos < 2 ) pos = pos1
      else pos = pos1 + pos - 1     
    }
    pos = pos + attr(pos1, 'match.length')
  } 
  if( pos.start ) pos = pos - attr(pos1, 'match.length')
    
  return(pos)
} 


#' @export 
extract.token <- function
(
  txt,    # source text
  smarker,  # start key-phrase(s) to find
  emarker,  # end key-phrase(s) to find
  pos = 1,  # position to start searching at
  keep.marker = F 
)
{
  pos1 = 1
  if (nchar(smarker) > 0)
    pos1 = find.tokens(txt, smarker, pos, pos.start = keep.marker)
  if( pos1 < 0 ) return("")
  pos1.marker = iif(keep.marker, pos1 + nchar(last(spl(smarker))), pos1)
  
  pos2 = nchar(txt)
  if (nchar(emarker) > 0)
    pos2 = find.tokens(txt, emarker, pos1.marker, pos.start = !keep.marker) - 1
  if( pos2 < 0 ) return("")
  
  return(substr(txt,pos1,pos2)) 
}

#' @export 
remove.tags <- function
(
  temp    # source text
)
{
  # remove all formating              
  temp = gsub(pattern = '<.*?>', replacement = '', temp, perl = TRUE) 
    
  temp = gsub(pattern = '\r', replacement = '', temp, perl = TRUE) 
  temp = gsub(pattern = '\n', replacement = '', temp, perl = TRUE) 
  temp = gsub(pattern = '\t', replacement = '', temp, perl = TRUE) 
  temp = gsub(pattern = '&nbsp;', replacement = '', temp, perl = TRUE) 
  temp = gsub(pattern = '&amp;', replacement = '', temp, perl = TRUE) 
  temp = gsub(pattern = '&raquo;', replacement = '', temp, perl = TRUE)     
  temp = gsub(pattern = '&#37;', replacement = '%', temp, perl = TRUE)      

  
  return(temp)
}

#' @export 
replace.token <- function
(
  txt,    # source text
  smarker,  # start key-phrase(s) to find
  emarker,  # end key-phrase(s) to find
  replacement,# replacement token
  pos = 1   # position to start searching at
)
{
  token = extract.token(txt, smarker, emarker, pos, keep.marker = T)
  if(nchar(token) == 0) 
    txt
  else
    replace.token(gsub(pattern = token, replacement = replacement, txt), smarker, emarker, replacement)
}


#' @export 
clean.table <- function
(
  temp    # extracted table
)
{
  temp = trim(temp)
  temp[nchar(temp)==0] = NA 
  temp = temp[ncol(temp) > rowSums(is.na(temp)),,drop=F]
  temp[,nrow(temp) > colSums(is.na(temp)),drop=F]
}

###############################################################################
# extract.table.from.webpage
#' @export 
###############################################################################
extract.table.from.webpage <- function
(
  txt,    # source text of webpage
  marker=NA,  # key-phrase(s) located in the table to extract
  has.header=T,# flag if table has a header
  end.marker=NA # additional end of token marker(s)
)
{
  tryCatch({    
    # find location of data   
    pos1=1
    
    if(!is.na(marker)) {
    marker = spl(marker)
    if(len(marker) > 0 && nchar(marker[1]) > 0)
      for(i in 1:len(marker))
        pos1 = regexpr(marker[i], substr(txt, pos1, nchar(txt))) + pos1
    }
    
    
    # find start/end of table
    pos0 = tail(gregexpr('<table', substr(txt, 1, pos1))[[1]], 1)
    if(pos0 == -1) pos0 = pos1
    pos2 = head(gregexpr('</table', substr(txt, pos1, nchar(txt)))[[1]], 1)
    if(pos2 == -1) pos2 = nchar(txt)+1
    temp =  substr(txt, pos0, pos1 + pos2 - 2)
  
    # remove all formating  
    temp = gsub(pattern = '<br>', replacement = '', temp, perl = TRUE) 
    
    temp = gsub(pattern = '</tr>', replacement = ';row;', temp, perl = TRUE) 
    temp = gsub(pattern = '</td>', replacement = ';col;', temp, perl = TRUE) 
    temp = gsub(pattern = '</th>', replacement = ';col;', temp, perl = TRUE) 
    if(!is.na(end.marker)) {
      marker = spl(end.marker)
      if(len(marker) > 0 && nchar(marker[1]) > 0)
        for(i in 1:len(marker))
          temp = gsub(pattern = marker[i], replacement = ';row;', temp, perl = TRUE) 
    }
            
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
    
    if(has.header) {
      colnames(temp) = trim(temp[(has.header + 0), ])
      temp = temp[-c(1:(has.header + 0)), ,drop=F]
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
  temp = extract.table.from.webpage(txt, 'Market Cap', has.header = F)
    temp = rbind(c('', Symbol), temp) # add header row

    
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
#' @export 
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
#' @export 
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
  # colnames(out) = spl('PR,IR,TR')
    
    
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
#
# This url is not working anymore, for updated example please see
#   bt.extend.DBC.update.test in bt.test.r 
#' @export 
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
      
  temp = cor(temp / mlag(temp)- 1, use='complete.obs', method='pearson')
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
#' @export 
###############################################################################
dow.jones.components <- function()
{
  url = 'http://money.cnn.com/data/dow30/'
  txt = join(readLines(url))
  
  # extract links
  temp = gsub(pattern = '">', replacement = '<td>', txt, perl = TRUE)
  temp = gsub(pattern = '</a>', replacement = '</td>', temp, perl = TRUE) 

  # extract table from this page
  temp = extract.table.from.webpage(temp, 'Volume', has.header = T)
  trim(temp[,'Company'])
}	

dow.jones.components.0 <- function()
{
  url = 'http://finance.yahoo.com/q/cp?s=^DJI+Components'
  txt = join(readLines(url))

  # extract table from this page
  temp = extract.table.from.webpage(txt, 'Volume', has.header = T)
  temp[, 'Symbol']
}

dow.jones.components.1 <- function()
{
  load.packages('readxl,httr')
  dir.create(paste(getwd(), 'temp', sep='/'), F)
  GET('http://www.djaverages.com/?go=export-components&symbol=DJI', write_disk('temp/DJI.xls', overwrite=TRUE))
  temp = read_excel('temp/DJI.xls')
  temp$Ticker
}
  
###############################################################################
# Get NASDAQ 100 Components
# http://www.nasdaq.com/markets/indices/nasdaq-100.aspx
#' @export 
###############################################################################
nasdaq.100.components <- function()
{
  url = 'http://www.nasdaq.com/quotes/nasdaq-100-stocks.aspx?render=download'
  temp = read.csv(url, header=TRUE, stringsAsFactors=F)   
    colnames(temp) = trim(colnames(temp))
  
  tickers = temp[, 'Symbol']
  return(tickers)
}

  

###############################################################################
# Get Sector SPDR Components
# http://www.sectorspdr.com/sectorspdr/IDCO.Client.Spdrs.Holdings/Export/ExportCsv?symbol=XLE
# tickers = spl('XLY,XLP,XLE,XLF,XLV,XLI,XLB,XLK,XLU')
# tickers.desc = spl('ConsumerCyclicals,ConsumerStaples,Energy,Financials,HealthCare,Industrials,Materials,Technology,U
#' @export
###############################################################################
sector.spdr.components <- function(sector.etf = 'XLE')
{
  url = paste('http://www.sectorspdr.com/sectorspdr/IDCO.Client.Spdrs.Holdings/Export/ExportCsv?symbol=', sector.etf, sep='')

  # extract table from this page
  temp = read.csv(url, skip=1, header=TRUE, stringsAsFactors=F)
  tickers = temp[, 'Symbol']

  return(tickers)
}


###############################################################################
# S&P 500 Components
# http://en.wikipedia.org/wiki/List_of_S%26P_500_companies
#' @export 
###############################################################################
sp500.components <- function()
{
  url = 'http://en.wikipedia.org/wiki/List_of_S%26P_500_companies'
  txt = join(readLines(url))
  
  # extract table from this page  
  temp = extract.table.from.webpage(txt, 'Ticker', has.header = T)
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
#' @export 
###############################################################################
sp100.components <- function()
{
  url = 'http://www.barchart.com/stocks/sp100.php'
  txt = join(readLines(url))
  
  # extract table from this page  
  temp = extract.table.from.webpage(txt, 'Components', has.header = T)
    i.start = grep('Name', temp[,2])
    tickers = trim(temp[-c(1:i.start), 1])
    
  return(tickers) 
}


###############################################################################
# iShares FTSE 100 (ISF)
# http://uk.ishares.com/en/rc/products/ISF/all-holdings/
# http://www.londonstockexchange.com/exchange/prices-and-markets/stocks/indices/constituents-indices.html?index=UKX
# Yahoo ticker for UK stocks ABF.L
#' @export 
###############################################################################
ftse100.components <- function()
{
  # get holdings from uk.ishares.com
  url = 'http://uk.ishares.com/en/rc/products/ISF/all-holdings/'
  txt = join(readLines(url))
  
  # extract table from this page    
  txt = gsub('&#37;','%',txt)
  temp = extract.table.from.webpage(txt, 'Security', has.header = T)
  
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
    temp.table = extract.table.from.webpage(txt, 'Price', has.header = T)
      colnames(temp.table)[1] = 'tickers'

    # extract links
    temp = gsub(pattern = '<a', replacement = '<td>', txt, perl = TRUE)
    temp = gsub(pattern = '</a>', replacement = '</td>', temp, perl = TRUE) 
  
    temp = extract.table.from.webpage(temp, 'Price', has.header = T)
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
# Get Dow Jones Components
# http://finance.yahoo.com/q/cp?s=^DJI+Components
# us.ishares.components(date='2008-02-01')
#' @export 
###############################################################################
us.ishares.components <- function(Symbol = 'DVY', date = NULL, debug = F)
{
  url = paste('http://us.ishares.com/product_info/fund/holdings/', Symbol, '.htm?periodCd=d', sep='')
  if( !is.null(date) )
    url = paste('http://us.ishares.com/product_info/fund/holdings/', Symbol, '.htm?asofDt=', date.end(date), '&periodCd=m', sep='')
  txt = join(readLines(url))

  # extract date from this page
  temp = remove.tags(extract.token(txt, 'Holdings Detail', 'Holdings subject to change'))
  date = as.Date(spl(trim(temp),' ')[3], '%m/%d/%Y')
  
  # extract table from this page
  temp = extract.table.from.webpage(txt, 'Symbol', has.header = T)
  
  colnames(temp) = trim(colnames(temp))
  temp = trim(temp)
  
  tickers = temp[, 'Symbol']
    keep.index = nchar(tickers)>1
  weights = as.double(temp[keep.index, '% Net Assets']) / 100
  tickers = tickers[keep.index]
    
  out = list(tickers = tickers, weights = weights, date = date)
  if(debug) out$txt = txt
  out
}


###############################################################################
# Get Google search results:
# https://gist.github.com/Daapii/7281439
# --- explanation of the parameters in the query ---
#
# ie = input encoding
# oe = output encoding
# q = query (our search term)
# num = amount of search results displayed at a time
# gws_rd=cr = redirects you to your countries version of google (required if you're not in the US)
# url encode our query
# query = "https://encrypted.google.com/search?ie=utf-8&oe=utf-8&q={0}&num=100&gws_rd=cr".format(query)
# google.search("r project")
#' @export 
###############################################################################
google.search <- function
(
  query
)
{  
  url = paste("http://google.com/search?ie=utf-8&oe=utf-8&q=", URLencode(query), "&num=10&gws_rd=cr", sep='')
  txt = join(readLines(url))

  tokens = spl(txt, '<li class="g">')

  if(len(tokens) < 2) return(NULL)

  records = matrix('', nrow=len(tokens)-1,nc=2)
    colnames(records) = c('label','url')
  for(i in 2:len(tokens)) {
    token = tokens[i] 
    token = extract.token(token, '<a href=', '</a>', keep.marker = T)
    url = extract.token(token, 'url\\?q=', '&amp;sa=U&amp;')  
    label = remove.tags(token) 
    records[i-1,] = c(label,url)
  }

  return(records)
}


###############################################################################
# Get the latest prices from the Google finance:
# http://digitalpbk.com/stock/google-finance-get-stock-quote-realtime
#  http://finance.google.com/finance/info?client=ig&q=MSFT,AAPL,NYSE:RY
#' @export 
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
#' @export 
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
# Download historical intraday prices from Google Finance
# http://www.mathworks.com/matlabcentral/fileexchange/32745-get-intraday-stock-price
# http://www.mathworks.com/matlabcentral/fileexchange/36115-volume-weighted-average-price-from-intra-daily-data
# http://www.codeproject.com/KB/IP/google_finance_downloader.aspx
# http://www.marketcalls.in/database/google-realtime-intraday-backfill-data.h
# getSymbol.intraday.google('GOOG','NASDAQ')
# getSymbol.intraday.google('.DJI','INDEXDJX')
#' @export 
###############################################################################
getSymbol.intraday.google <- function
(
  Symbol, 
  Exchange,
  interval = 60,  # 60 seconds
  period = '1d'
) 
{
  # download Key Statistics from yahoo  
  url = paste('http://www.google.com/finance/getprices?q=', Symbol,
    '&x=', Exchange,
      '&i=', interval,
      '&p=', period,
      '&f=', 'd,o,h,l,c,v', sep='')

  load.packages('data.table')
  out = fread(url, stringsAsFactors=F)
  
  if(ncol(out) < 5) {
    cat('Error getting data from', url, '\n')
    return(NULL)
  }
  
    setnames(out, spl('Date,Open,High,Low,Close,Volume'))
  
  # date logic
  date = out$Date
    date.index = substr(out$Date,1,1) == 'a'
    date = as.double(gsub('a','',date)) 
  temp = NA * date
    temp[date.index] = date[date.index]
    temp = ifna.prev(temp)
  date = temp + date * interval
    date[date.index] = temp[date.index] 
  class(date) = c("POSIXt", "POSIXct")
  
  date = date - (as.double(format(date[1],'%H')) - 9)*60*60
  
  make.xts(out[,-1,with=F], date)
}


###############################################################################
# getSymbols interface to Yahoo today's delayed qoutes
# based on getQuote.yahoo from quantmod package
#' @export 
###############################################################################            
getQuote.yahoo.today <- function(Symbols) {
  require('data.table')
    what = yahooQF(names = spl('Name,Symbol,Last Trade Time,Last Trade Date,Open,Days High,Days Low,Last Trade (Price Only),Volume,Previous Close'))
    names = spl('Name,Symbol,Time,Date,Open,High,Low,Close,Volume,Yesterday')
    
    all.symbols = lapply(seq(1, len(Symbols), 100), function(x) na.omit(Symbols[x:(x + 99)]))
    out = c()
    
    for(i in 1:len(all.symbols)) {
        # download
        url = paste('http://download.finance.yahoo.com/d/quotes.csv?s=',
            join( trim(all.symbols[[i]]), ','),
            '&f=', what[[1]], sep = '')
        
    txt = join(readLines(url),'\n') 
    data = fread(paste0(txt,'\n'), stringsAsFactors=F, sep=',')
          setnames(data,names)
          setkey(data,'Symbol')       
        out = rbind(out, data)
    }
    out
}

###############################################################################
# extend GLD and SLV historical prices
#' @export 
###############################################################################
extend.GLD <- function(GLD) {
  extend.data(GLD, bundes.bank.data.gold(), scale=T)
}

# gold = extend.GLD(data$GLD)
# comm = extend.data(data$DBC, get.CRB(), scale=T)
#' @export 
extend.data <- function
(
  current,
  hist,
  scale = F
) 
{
  colnames(current) = sapply(colnames(current), function(x) last(spl(x,'\\.')))
  colnames(hist) = sapply(colnames(hist), function(x) last(spl(x,'\\.')))

  # find Close in hist
  close.index = find.names('Close', hist)
  if(len(close.index)==0) close.index = 1 
  adjusted.index = find.names('Adjusted', hist)
  if(len(adjusted.index)==0) adjusted.index = close.index 

  if(scale) {
    cur.close.index = find.names('Close', current)
    if(len(cur.close.index)==0) cur.close.index = 1 
    cur.adjusted.index = find.names('Adjusted', current)
    if(len(cur.adjusted.index)==0) cur.adjusted.index = cur.close.index 
  
    # find first common observation in current and hist series
    common = merge(current[,cur.close.index], hist[,close.index], join='inner')
    
    scale = as.numeric(common[1,1]) / as.numeric(common[1,2])
      
    if( close.index == adjusted.index ) 
      hist = hist * scale
    else {
      hist[,-adjusted.index] = hist[,-adjusted.index] * scale
      
      common = merge(current[,cur.adjusted.index], hist[,adjusted.index], join='inner')
      scale = as.numeric(common[1,1]) / as.numeric(common[1,2])
      hist[,adjusted.index] = hist[,adjusted.index] * scale
    }
  }
  
  # subset history before current
  hist = hist[format(index(current[1])-1,'::%Y:%m:%d'),,drop=F]
  
  #hist = make.xts( rep.col(hist[,adjusted.index], ncol(current)), index(hist)) 
  if( ncol(hist) != ncol(current) ) 
    hist = rep.col(hist[,adjusted.index], ncol(current))
  else
    hist = hist[, colnames(current)]
  
  colnames(hist) = colnames(current)
    
  rbind( hist, current )
}



# extend data from the previously saved proxies
#' @export 
extend.data.proxy <- function(data, data.proxy = NULL, proxy.filename = 'data.proxy.Rdata') {
  if(is.null(data.proxy) && file.exists(proxy.filename))
    load(file=proxy.filename)
    
    if(!is.null(data.proxy))
    for(n in ls(data.proxy))
        if( !is.null(data[[n]]) )
          data[[n]] = extend.data(data[[n]], data.proxy[[n]], scale=T)
}  
  
###############################################################################
# Leveraged series
###############################################################################  
# Create Leveraged series with data from the unlevereged.
#
# Please use only with Adjusted time series. For example create.leveraged(data$QQQ, leverage=2)
# will produce erroneous values because QQQ had 2: 1 Stock Split on Mar 20, 2000 
# Hence at 2x leverage the value goes to zero.
#
# @example create.leveraged(tlt, 2)
# @example extend.data(data$UBT, create.leveraged(data$TLT, leverage=2), scale=T)
# @example extend.data(data$TMF, create.leveraged(data$TLT, leverage=3), scale=T)
#' @export 
create.leveraged = function(hist, leverage=2) {
  rets = 1 + leverage * (hist / mlag(hist) - 1)
  rets[1,] = 1  
  bt.apply.matrix(rets, cumprod)
} 


create.leveraged.test = function() {
    tickers = spl('TMF,UBT,TLT')
    data = new.env()
    
    getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)   

    
    test2 = extend.data(data$UBT, create.leveraged(data$TLT, leverage=2), scale=T)
    test3 = extend.data(data$TMF, create.leveraged(data$TLT, leverage=3), scale=T)    
    proxy.test(list(TLT=data$TLT, UBT=test2, TMF=test3),price.fn=Ad)
  
    test0 = create.leveraged(data$TLT, leverage=2)    
    proxy.test(list(UBT=data$UBT, EXTEND=test0),price.fn=Ad)
    
    test0 = create.leveraged(data$TLT, leverage=3)    
    proxy.test(list(TMF=data$TMF, EXTEND=test0),price.fn=Ad)

    # please note the difference in the above extension is due to difference in the
    # underlying benchmarks. I.e.
    #    
    # http://www.proshares.com/funds/ubt.html
    # ProShares Ultra 20+ Year Treasury (UBT) seeks daily investment results
    # that correspond to two times (2x) the daily performance of the Barclays U.S. 20+ Year Treasury Bond Index.
    #
    # http://www.direxioninvestments.com/products/direxion-daily-20-year-treasury-bull-3x-etf
    # Direxion Daily 20+ Yr Trsy Bull 3X ETF (TMF) seeks daily investment results
    # that correspond to three times (3x) the daily performance of the NYSE 20 Year Plus Treasury Bond Index (AXTWEN).
}

###############################################################################
# Bundes Bank - long history of gold prices
# http://www.bundesbank.de/Navigation/EN/Statistics/Time_series_databases/Macro_economic_time_series/its_list_node.html?listId=www_s331_b01015_3
#' @export 
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

#' @export 
bundes.bank.data.gold <- function() {
  bundes.bank.data('BBEX3.D.XAU.USD.EA.AC.C05')
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
#       fx.sauder.data(2004, 2007, base.cur, target.curs), 
#       fx.sauder.data(2008, 2011, base.cur, target.curs),
#       fx.sauder.data(2012, 2012, base.cur, target.curs))
#' @export 
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
#' @export 
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
#' @export 
###############################################################################
getSymbols.fxhistoricaldata <- function
(
  Symbols, 
  type = spl('hour,day'),
  env = .GlobalEnv, 
  auto.assign = TRUE,
  download = FALSE,
  name.has.type = TRUE
) 
{   
  type = type[1]
  type0 = paste0(type,'_')
  
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
      assign(paste0(gsub('\\^', '', Symbols[i]), iif(name.has.type,type0,'')), out, env)  
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
#' @export 
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
    for(i in ls(data.fx)) data.fx[[i]] = na.omit(data.fx[[i]])
    for(i in convert.index) data.fx[[i]] = 1 / data.fx[[i]]

  # extract fx where all currencies are available
  bt.prep(data.fx, align='remove.na')
  fx = bt.apply(data.fx, '[')
  
  return(fx)
}


###############################################################################
# Download Strategic Portfolios from wealthsimple.com
# 
#http://faq.wealthsimple.com/article/121-how-has-the-risk-level-1-portfolio-performed
#http://faq.wealthsimple.com/article/130-how-has-the-risk-level-10-portfolio-performed
#http://faq.wealthsimple.com/article/127-how-has-the-risk-level-7-portfolio-performed
#' @export 
###############################################################################     
wealthsimple.portfolio = function(portfolio.number = 10) {
	# download
	url = paste0('http://faq.wealthsimple.com/article/', 120+portfolio.number, '-how-has-the-risk-level-',portfolio.number,'-portfolio-performed')
	txt = join(readLines(url))
  
	# extract
	temp = extract.table.from.webpage(txt, 'Breakdown', has.header = F)

	# parse
	temp = gsub(pattern = '%', replacement = '', temp)
	temp  = trim(temp[,c(2,4)])
	temp  = temp[!is.na(temp[,1]),]

	# create output
	value = as.numeric(temp[,2])
	names(value) = temp[,1]
	value
}

 
wealthsimple.portfolio.test = function() {
	# create list of all portolios
	portfolios = list()
	for(i in 1:10)
		portfolios[[i]] = wealthsimple.portfolio(i)
		
	portfolios = t(sapply(portfolios, identity))
	
	# look at evolution of mixes
	plota.stacked(1:10, portfolios/100, flip.legend = T, type='s', xaxp=c(1,10,9), las=1,
		main='Wealthsimple Transition Matrix', xlab='Risk Portfolio')
}	
	

###############################################################################
# getSymbols interface to tradingblox free futures and forex data
# http://www.tradingblox.com/tradingblox/free-historical-data.htm
# http://www.tradingblox.com/Data/DataOnly.zip
# Date, Open, High, Low, Close, Volume (zero for forex cash markets), 
# Open Interest (futures only), Delivery Month ( YYYYMM futures only), 
# Unadjusted Close (zero for forex cash markets)
#' @export 
###############################################################################     
getSymbols.TB <- function(
  env = .GlobalEnv, 
  auto.assign = TRUE,
  download = FALSE,
  type = c('Both', 'Futures', 'Forex'),
  rm.index =  'PB',   # remove Pork Bellies because not traded
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
  ##  Unzip
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
  #           (unadjusted-futures[t-1] + (back-adjusted-futures[t] - back-adjusted-futures[t-1])) 
  # futures-return[t] = --------------------------------------------------------------------------------------------------  - 1
  #           unadjusted-futures[t-1] 
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
    data[[i]]$Adjusted  = data[[i]]$Close
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
# Data contains historical time series for both 
# * Spot ( Unadjusted - unadjusted-futures ) and 
# * Future ( Adjusted - back-adjusted-futures)
#
# First step, I updated Spot to include roll yield (i.e. spot.adjust.roll)
# Next, I computed returns as change in futures relative to the spot.adjust.roll level
#I.e. return is
#
# (unadjusted-futures[t-1] + (back-adjusted-futures[t] - back-adjusted-futures[t-1])) 
# ------------------------------------------------------------------------------------  - 1
# unadjusted-futures[t-1] 
#
#   Change in back-adjusted-futures
# = --------------------------------
#   Prior Unadjusted-futures level (which i adjusted for roll yield)
#
# http://www.automated-trading-system.com/crude-oil-contango-and-roll-yield-for-commodity-trading/
###############################################################################     
getSymbols.TB.test = function() {
	filename = 'temp/Futures/CL20_I0B.TXT'
	i = 'CL'
	data = env()
      fr <- read.csv(filename, header = FALSE) 
      fr <- make.xts(fr[,-1], as.Date(as.character(fr[,1]),'%Y%m%d'))     
      colnames(fr) <- spl('Open,High,Low,Close,Volume,OpenInterest,DeliveryMonth,Unadjusted')[1:ncol(fr)]
      fr$Adjusted = fr$Close
      data[[i]] = fr

	# Unadjusted is Spot (unadjusted-futures)
	# Adjusted is Close is Future (back-adjusted-futures)

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
    Close = spot[n] * new.series / new.series[n]    
    
	plot.data = as.xts(list(
		Unadjusted = data[[i]]$Unadjusted, 
		Adjusted = data[[i]]$Adjusted,
		Implied.Close = make.xts(Close, index(data[[i]]))
	))

png(filename = 'plot_CL_2009.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')			    

	plota.matplot( scale.one(plot.data['2009']), main='Crude oil, CL - 2009')
	
dev.off()	
	
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
#' @export 
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
  period = ifna(map[periodicity[1]], periodicity[1])
  filename.zip = paste(name[1], period, '_TXT.zip', sep='')
  filename.txt = paste(name[1], period, '.txt', sep='')
  url = paste('http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/', filename.zip, sep='')
        
  # download zip archive
  if(download) {
    download.file(url, filename.zip)
  }

  # setup temp folder
  temp.folder = paste(getwd(), 'temp', sep='/')
  dir.create(temp.folder, F)
    
  ##*****************************************************************
  ##  Unzip
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
    if(is.na(start.index)) start.index = index[len(index)] + 1
    end.index = data.index[i,1] - 1
    n.index = end.index - start.index + 1

    # column names
    name = 'data'
    colnames = scan(text = out[start.index], what='', quiet=T)
    if(n.index == 2) {
      name = trim(out[start.index])
      colnames = scan(text = out[end.index], what='', quiet=T)
      
      colnames1 = scan(text = out[end.index+1], what='', quiet=T)
      if(len(colnames) > len(colnames1)) {
        cindex = which(diff(gregexpr(' ',out[end.index+1])[[1]]) > 1)
        cindex = c(1, gregexpr(' ',out[end.index+1])[[1]][(cindex+1)], nchar(out[end.index])+1)
        colnames = rep('', len(cindex)-1)
        for(j in 2:len(cindex))
          colnames[j-1] = substr(out[end.index], cindex[j-1], cindex[j]-1)
        colnames = trim(colnames)
        colnames = colnames[nchar(colnames) > 0]        
      }
      
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
    
    if( date.format.n == 6 ) {
      date.format.add = '01'    
    } else if( date.format.n == 4 ) {
      date.format.add = '0101'  
    } 
    
    data[[name]] = make.xts(temp[,-1], as.Date(paste(temp[,1], date.format.add, sep=''),date.format))
      colnames(data[[name]]) = colnames   
  }
  return( data )
}


###############################################################################
# CBOE Futures
#' @export 
###############################################################################
download.helper <- function(url,download) {
  # setup temp folder
  temp.folder = paste(getwd(), 'temp', sep='/')
  dir.create(temp.folder, F)

  filename = paste0(temp.folder, '/', basename(url))
  if(download || !file.exists(filename))
    try(download.file(url, filename, mode='wb'), TRUE)
  filename
}


# Update files, download = T
#for(y in date.year(Sys.Date()):(1+date.year(Sys.Date())))
# for(m in 1:12) {
#   temp = getSymbol.CBOE('VX', m, y, T)
#' @export 
getSymbol.CBOE <- function
(
  Symbol, 
  Month,
  Year,
  download = FALSE
) 
{

  # month codes of the futures
  m.codes = spl('F,G,H,J,K,M,N,Q,U,V,X,Z')
  
  url = paste0("http://cfe.cboe.com/Publish/ScheduledTask/MktData/datahouse/CFE_",
    m.codes[Month], substring(Year,3,4), '_', Symbol, '.csv')
  
  filename = download.helper(url, download)   
  if(file.exists(filename) && file.info(filename)$size > 1)
    read.xts(filename, format='%m/%d/%Y')
  else
    NULL
}


# SPX Volatility Term Structure
#' @export 
cboe.volatility.term.structure.SPX <- function(make.plot = T) {
  url = 'http://www.cboe.com/data/volatilityindexes/volatilityindexes.aspx'
  txt = join(readLines(url))

  temp.table = extract.table.from.webpage(txt, 'Trade Date', has.header = T)
    colnames(temp.table) = gsub(' ','.',trim(tolower(colnames(temp.table))))
    temp.table = data.frame(temp.table)
    temp.table$trade.date = as.POSIXct(temp.table$trade.date, format="%m/%d/%Y %I:%M:%S %p")
    temp.table$expiration.date = as.Date(temp.table$expiration.date, "%d-%b-%y")
    temp.table[,3] = as.numeric(as.character(temp.table[,3]))
    temp.table[,4] = as.numeric(as.character(temp.table[,4]))
  temp.table

  if(make.plot) {
    plot(temp.table$expiration.date, temp.table$vix, type = 'b', 
      main=paste('VIX Term Structure, generated ',  max(temp.table$trade.date)), 
      xlab = 'Expiration Month', ylab='VIX Index Level')
    grid()
  }
  
  temp.table
}


#' @export 
load.VXX.CBOE <- function() {
  # https://r-forge.r-project.org/scm/viewvc.php/pkg/qmao/R/getSymbols.cfe.R?view=markup&root=twsinstrument
  index = "::2007-03-25"
  fields = spl('Open,High,Low,Close,Settle')
 
  # days remaining before settlement, on a given date
  dr <- function(index, date) sum(index>date)

  data <- new.env()
  futures = list()  
  i = 1
  for(y in 2004:(1+date.year(Sys.Date())))
    for(m in 1:12) {
      temp = getSymbol.CBOE('VX', m, y)
      if(is.null(temp)) next    
      
      temp = temp[temp$Settle > 0]
      if(nrow(temp)==0) next    
      if(len(temp[index,1])> 0)
            temp[index,fields] = temp[index,fields]/10
          
          label = paste0(y*100+m)
          dates = index(temp)
                                  
      futures[[ i ]] = list()
      futures[[ i ]]$data = temp
      futures[[ i ]]$label = label
      futures[[ i ]]$index = dates
      futures[[ i ]]$settle.date = last(dates)
      # set roll period of each future
      if(i==1)
        futures[[ i ]]$dt = len(dates)
      else  
        futures[[ i ]]$dt = dr(dates, futures[[ i-1 ]]$settle.date)
          
          temp$i = i
          #number of business days in roll period
          temp$dt = futures[[ i ]]$dt
          # number of remaining dates in the first futures contract
          temp$dr = (len(dates)-1):0
      data[[ label ]] = temp
                  
          i = i + 1    
    }
      
  bt.prep(data, align='keep.all')
  
  # debug
  # bt.apply(data, function(x) x[,'dr'])[1150:1160,48:55]
  # count(t(coredata(bt.apply(data, function(x) x[,'Settle']))))
  
  data
}
  
#' @export 
extract.VXX.CBOE <- function(data, field, index, exact.match=T) {
  map  = 1:ncol(data$prices)
  
  temp = bt.apply(data, function(x) x[,field])
    temp = coredata(temp)
    
  t(apply(temp, 1, function(x) {
    if(exact.match) {
      pos = map[!is.na(x)][1] - 1
      x[(index + pos)]
    } else {
      pos = map[!is.na(x)][index]
      x[pos]      
    }
  }))
}



# Reconstructing VXX from CBOE futures data 
# http://tradingwithpython.blogspot.ca/2012/01/reconstructing-vxx-from-cboe-futures.html
# for VXX calculation see http://www.ipathetn.com/static/pdf/vix-prospectus.pdf
# page PS-20
#' @export 
reconstruct.VXX.CBOE <- function(exact.match=T) {
  data = load.VXX.CBOE()

  dt = extract.VXX.CBOE(data, 'dt', 1, exact.match)[1,]
  dr = extract.VXX.CBOE(data, 'dr', 1, exact.match)[1,]
  x  = extract.VXX.CBOE(data, 'Settle', 1:2, exact.match)
    
  # for VXX calculation see http://www.ipathetn.com/static/pdf/vix-prospectus.pdf
  # page PS-20: 1/2 months    
  # VXX Short-Term
  w = cbind(dr / dt, (dt - dr) / dt)  
  val.cur = rowSums(x * mlag(w))
  val.yest = rowSums(mlag(x) * mlag(w))
  ret = val.cur / val.yest - 1
  
  # on roll it is simply future2's return
  index = ifna(mlag(dr) == 0, F)
  ret[index] = (x[,1] / mlag(x[,2]) - 1)[index]
  
  Close = cumprod(1+ifna(ret,0))
  VXX = make.xts(cbind(Close,x,dt,dr,ret), data$dates)
  
  # VXZ Mid-Term: 4,5,6,7 months
  x  = extract.VXX.CBOE(data, 'Settle', 4:7, exact.match)
  
  w = cbind(dr / dt, 1, 1, (dt - dr) / dt)  
  val.cur = rowSums(x * mlag(w))
  val.yest = rowSums(mlag(x) * mlag(w))
  ret = val.cur / val.yest - 1
  
  index = ifna(mlag(dr) == 0, F)
  ret[index] = (rowSums(x[,-4]) / mlag(rowSums(x[,-1])) - 1)[index]
    
  Close = cumprod(1+ifna(ret,0))  
  VXZ = make.xts(cbind(Close,x,dt,dr,ret), data$dates)
  
  # debug
  # plota(VXZ,type='l',lwd=2) 
  # write.xts(VXZ, file='vix-mid.txt')  
    
  list(VXX = VXX, VXZ = VXZ)
}  


###############################################################################
# Load FOMC dates
# http://www.federalreserve.gov/monetarypolicy/fomccalendars.htm
# http://www.federalreserve.gov/monetarypolicy/fomchistorical2008.htm
# http://quant.stackexchange.com/questions/141/what-data-sources-are-available-online
#' @export 
###############################################################################
get.FOMC.dates <- function
(
  download = TRUE,
  fomc.filename = 'FOMC.Rdata'
)
{
  if(!download && file.exists(fomc.filename)) {
    load(file=fomc.filename)
    return(FOMC)
  }
  
  # download data 
  url = 'http://www.federalreserve.gov/monetarypolicy/fomccalendars.htm'
  txt = join(readLines(url))

  # extract data from page
  data = c()
  for(year in 2009:(1 + date.year(Sys.Date()))) {
    temp = extract.table.from.webpage(txt, paste(year,'FOMC Meetings'))
    if(nrow(temp) == 0) next
    
    temp = tolower(trim(temp[,1:2]))
      temp = temp[nchar(temp[,1]) > 0,]
    month = temp[,1]
    day = gsub('\\(','',gsub('\\)','',temp[,2]))
      day = trim(day)
      status = rep('', len(day))
    index = grep('\\*',day)
      if(any(index)) status[index] = '*'    
    index = grep(' ',day)
      if(any(index)) for(j in index) status[j] = spl(day[j],' ')[2]
    day = gsub('\\*','', sapply(day,function(x) spl(x,' ')[1]))

    temp = apply(cbind(day, month, status), 1, function(x) paste(year, spl(x[2],'/'), spl(x[1],'-'), '|', x[3]) )
        
    data = cbind(data, trim(sapply(unlist(temp),spl,'\\|')))
  }
  
  recent.days = as.Date(data[1,],'%Y %B %d')
  status = as.vector(data[2,])
  
  # extract data from page
  data = c()
  for(year in 1936:2008) {
    cat(year,'\n')
    url = paste0('http://www.federalreserve.gov/monetarypolicy/fomchistorical', year, '.htm')
    txt = join(readLines(url))
      
    tokens = spl(txt,'<div id="historical">')
    days = c()
    for(token in tokens[-1])
      days = c(days,colnames(extract.table.from.webpage(token, 'table'))[1])
    data = rbind(data, cbind(year, days))
  }
  
  day = tolower(data[,2])
  day = gsub(',', '-', gsub('and', '', gsub('conference call', '', gsub('meeting','',day))))
  # remove last token
  day = unlist(lapply(day, function(x) join(rev(rev(spl(x,' '))[-1]),' ')))
  
  temp = unlist(apply(cbind(day,data[,1]),1, function(x) paste(trim(spl(x[1],'-')),x[2]) )) 
  temp = sapply(lapply(temp,spl,' '), function(x) iif(len(x)==3,x,c(NA,x)))
    temp[1,] = ifna.prev(temp[1,])
  days = as.Date(apply(temp,2,join,' '),'%B %d %Y ')
  
  FOMC = list(day = c(days, recent.days), status=c(rep('',len(days)), status))
  save(FOMC,file=fomc.filename)
  FOMC
} 


###############################################################################
# Get EDGAR info
# www.sec.gov/cgi-bin/browse-edgar?CIK=AAPL&Find=Search&owner=exclude&action=getcompany
# mktstk.wordpress.com/2015/03/03/sic-lookup-by-stock-symbol/
#' @export 
###############################################################################
edgar.info <- function(ticker)
{
  # please note readLines only works with http, for more detail please read
  # http://stackoverflow.com/questions/26540485/readlines-does-not-read-from-https-url-when-called-from-systemrscript
  url = paste0('http://www.sec.gov/cgi-bin/browse-edgar?CIK=', ticker, '&Find=Search&owner=exclude&action=getcompany')
  txt = join(readLines(url))
  out = list()

  # extract table from this page
  temp = extract.table.from.webpage(txt, 'seriesDiv,Filings', has.header = T)
    out$fillings= clean.table(temp)
  
  temp = extract.token(txt, 'contentDiv,mailer,Mailing Address','</div>')
  out$mailing = t(clean.table(extract.table.from.webpage(temp, has.header=F, end.marker='</span>')))
    colnames(out$mailing) = 'Mailing Address'

  temp = extract.token(txt, 'contentDiv,mailer,Business Address','</div>')
  out$business = t(clean.table(extract.table.from.webpage(temp, has.header=F, end.marker='</span>')))
    colnames(out$business) = 'Business Address'

  temp = extract.token(txt, 'contentDiv,companyInfo,>','</div>')
  temp = gsub('\\|', '</span>', replace.token(temp, '<br','>','</span>'))
  temp = clean.table(extract.table.from.webpage(temp, has.header=F, end.marker='</span>'))
  out$company = t(temp)
    colnames(out$company) = 'Company Info'
  
  out$sic = trim(spl(spl(temp[grep('SIC', temp)],':')[2],'-'))

  return(out)
}


###############################################################################
# Get Zacks info
# http://www.zacks.com/stock/research/IBM/earnings-announcements
#' @export
###############################################################################
zacks.info <- function(ticker = 'IBM')
{
  url = paste0('http://www.zacks.com/stock/research/', ticker, '/earnings-announcements')
  txt = join(readLines(url))
 
  out = list()
  require(jsonlite)
  
  for(i in spl('earnings,webcasts,revisions,splits,dividends,guidance')) {  
  	data = extract.token(txt,paste0('<script>,window.app_data_', i, ',=,"data"'),'</script>')
  	data = fromJSON(paste('{"data"', data))
  	out[[i]] = data$data
  }
  out
}


###############################################################################
# Get quantumonline info
# www.quantumonline.com/search.cfm?tickersymbol=458140100&sopt=cusip&1.0.1=Search    
# quantumonline.info(id = '458140100', type='cusip')
#' @export
###############################################################################
quantumonline.info <- function
(
    id,
    type=c(
        'cusip', # by CUSIP Number
        'symbol', # by Ticker Symbol
        'sname' # Symbol Lookup
    )
)
{
    # http://www.quantumonline.com/search.cfm?tickersymbol=458140100&sopt=cusip&1.0.1=Search    
    url = paste0('http://www.quantumonline.com/search.cfm?tickersymbol=', id, '&sopt=', type[1], '&1.0.1=Search')
    txt = join(readLines(url))
    out = list()

    # extract table from this page
    out$main = extract.table.from.webpage(gsub('&nbsp;', ',', txt), "Company's Online Profile", has.header = F)    
    out$address = extract.table.from.webpage( txt, 'Address:', has.header = F)

    return(out)
}


###############################################################################	
#' URL for various data providers
#' @export 
###############################################################################
hist.quotes.url <- function
(
	ticker = 'IBM',
	from = '1900-01-01', 
	to = Sys.Date(),
	src = spl('yahoo,google,quotemedia')
)
{
	if(class(from) != 'Date') from = as.Date(from, '%Y-%m-%d')	
	if(class(to) != 'Date') to = as.Date(to, '%Y-%m-%d')
	
	switch(src[1],
		yahoo = paste('http://ichart.finance.yahoo.com/table.csv?',
         's=', ticker,  
         '&a=', sprintf('%.2d', date.month(from) - 1),
         format(from, '&b=%d&c=%Y'),
         '&d=', sprintf('%.2d', date.month(to) - 1),
         format(to, '&e=%d&f=%Y'),
         '&g=d&q=q&y=0&z=file&x=.csv',
         sep=''),
         
		google = paste('http://finance.google.com/finance/historical?',
         'q=', ticker,  
         '&startdate=', format(from, '%b+%d+%Y'),
         '&enddate=', format(to, '%b+%d+%Y'),
         '&output=csv',
         sep=''),
         
		quotemedia = paste('http://app.quotemedia.com/quotetools/getHistoryDownload.csv?webmasterId=501&',
         'symbol=', ticker,  
         '&startMonth=', sprintf('%.2d', date.month(from) - 1),
         format(from, '&startDay=%d&startYear=%Y'),
         '&endMonth=', sprintf('%.2d', date.month(to) - 1),
         format(to, '&endDay=%d&endYear=%Y'),
         '&isRanged=true',
         sep=''),
         
    # default
    ''
 ) 	
}  

###############################################################################
# Remove extreme data points
#' @export 
###############################################################################
data.clean <- function
(
  data, 
  min.ratio = 2.5, 
  min.obs = 3*252,
  iqr.mult = 20
) 
{
  data$symbolnames = iif(is.null(data$symbolnames), ls(data), data$symbolnames)
  
  # remove all series that has less than minimum number of observations
if(min.obs > 0) {  
  index = names(which(sapply(data$symbolnames, function(x) as.numeric(count(Cl(data[[x]])))) < min.obs))
  if (len(index) > 0) {
    cat('Removing', index, 'have less than', min.obs, 'observations','\n')
    rm(list=index, envir=data)
    
    data$symbolnames = setdiff(data$symbolnames, index)
  }
}
    
  for(ticker in data$symbolnames)
    data[[ticker]] = data.clean.helper(data[[ticker]], ticker, min.ratio, iqr.mult)
}

data.clean.helper <- function
(
  data, 
  ticker,
  min.ratio = 2.5, 
  iqr.mult = 20
) 
{
    data = data[Cl(data) > 0 & Ad(data) > 0]
    
    nperiods = nrow(data)
    price = Ad(data)
    
    # forward ratio
    ratio = as.vector((price)/mlag(price))
    index = which(ratio > min.ratio)
        
    if(len(index) > 0)
      for(i in index) {
        cat('Abnormal price found for', ticker, format(index(data)[i],'%d-%b-%Y'),'Ratio :', round(ratio[i],1),'\n')
        for(name in find.names('Open,Close,High,Low,Adjusted', data)) 
          data[i:nperiods,name] = data[i:nperiods,name] / ratio[i]
      }
    
    price = Ad(data)
    ret = as.vector((price)/mlag(price)) - 1
    threshold = iqr.mult * IQR(ret, na.rm=T)
    index = which(ret > threshold | ret < -threshold)
    
    if(len(index) > 0)
      for(i in index) {
        cat('Abnormal price found for', ticker, format(index(data)[i],'%d-%b-%Y'),'based on IQR, Ratio :', round(ratio[i],1),'\n')
        for(name in find.names('Open,Close,High,Low,Adjusted', data)) 
          data[i:nperiods,name] = data[i:nperiods,name] / ratio[i]
      }
              
    # backward ratio
    price = Ad(data)
    ratio = as.vector(mlag(price)/(price))
    index = which(ratio > min.ratio)

    if(len(index) > 0)
      for(i in index) {
        cat('Abnormal price found for', ticker, format(index(data)[i],'%d-%b-%Y'),'Inverse Ratio :', round(ratio[i],1),'\n')        
        for(name in find.names('Open,Close,High,Low,Adjusted', data)) 
          data[i:nperiods,name] = data[i:nperiods,name] * ratio[i]
      }
      
    data      
}


###############################################################################
# Create data proxy, more details at
# http://systematicinvestor.github.io/Data-Proxy/
#' @export 
###############################################################################
make.data.proxy <- function() {
    #*****************************************************************
    # Load external data
    #******************************************************************   
    load.packages('quantmod')  

	raw.data = env()
    
	#--------------------------------   
    # TRJ_CRB file was downloaded from the 
    # http://www.corecommodityllc.com/CoreIndexes.aspx
    # select TR/CC-CRB Index-Total Return and click "See Chart"
    # on Chart page click "Download to Spreadsheet" link
    # copy TR_CC-CRB, downloaded file, to data folder
    filename = 'data/TR_CC-CRB'
    if(file.exists(filename)) {
    	temp = extract.table.from.webpage( join(readLines(filename)), 'EODValue' )
    	temp = join( apply(temp, 1, join, ','), '\n' )
    	raw.data$CRB = make.stock.xts( read.xts(temp, format='%m/%d/%y' ) )
    }
     
	#--------------------------------   
	# load 3-Month Treasury Bill from FRED (BIL)
	filename = 'data/TB3M.Rdata'
	if(!file.exists(filename)) {
		TB3M = quantmod::getSymbols('DTB3', src='FRED', auto.assign = FALSE)
		save(TB3M, file=filename)
	}
	load(file=filename)
	TB3M[] = ifna.prev(TB3M)
	#compute.raw.annual.factor(TB3M)
	raw.data$TB3M = make.stock.xts(processTBill(TB3M, timetomaturity = 1/4, 261))
	
	
	#--------------------------------   
	# load 3 years t-bill from FRED (BIL)
	filename = 'data/TB3Y.Rdata'
	if(!file.exists(filename)) {
		TB3Y = quantmod::getSymbols('DGS3', src='FRED', auto.assign = FALSE)
		save(TB3Y, file=filename)
	}
	load(file=filename)
	TB3Y[] = ifna.prev(TB3Y)
	#compute.raw.annual.factor(TB3Y)
	raw.data$TB3Y = make.stock.xts(processTBill(TB3Y, timetomaturity = 3, 261))

	#--------------------------------   
	# load 10 years t-bill from FRED (BIL)
	filename = 'data/TB10Y.Rdata'
	if(!file.exists(filename)) {
		TB10Y = quantmod::getSymbols('DGS10', src='FRED', auto.assign = FALSE)
		save(TB10Y, file=filename)
	}
	load(file=filename)
	TB10Y[] = ifna.prev(TB10Y)
	#compute.raw.annual.factor(TB10Y)
	raw.data$TB10Y = make.stock.xts(processTBill(TB10Y, timetomaturity = 10, 261))

	#--------------------------------   
	# load 20 years t-bill from FRED (BIL)
	filename = 'data/TB20Y.Rdata'
	if(!file.exists(filename)) {
		TB20Y = quantmod::getSymbols('GS20', src='FRED', auto.assign = FALSE)
		save(TB20Y, file=filename)
	}
	load(file=filename)

	TB20Y[] = ifna.prev(TB20Y)
  
	#compute.raw.annual.factor(TB10Y)
	raw.data$TB20Y = make.stock.xts(processTBill(TB20Y, timetomaturity = 20, 12))

	#--------------------------------
	filename = 'data/GOLD.Rdata'
	if(!file.exists(filename)) {
		GOLD = bundes.bank.data.gold()
		save(GOLD, file=filename)
	}
	load(file=filename)
	raw.data$GOLD = make.stock.xts(GOLD)

	#--------------------------------
	# FTSE NAREIT U.S. Real Estate Index monthly total return series
	# http://returns.reit.com/returns/MonthlyHistoricalReturns.xls
	# https://r-forge.r-project.org/scm/viewvc.php/pkg/FinancialInstrument/inst/parser/download.NAREIT.R?view=markup&root=blotter
	filename = 'data/NAREIT.xls'
	if(!file.exists(filename)) {
		url = 'http://returns.reit.com/returns/MonthlyHistoricalReturns.xls'
		download.file(url, filename,  mode = 'wb')
	}
	
	load.packages('readxl')	
	temp = read_excel(filename, sheet='Index Data', skip=7)
	NAREIT = make.xts(temp$Index, as.Date(temp$Date)) 

	raw.data$NAREIT = make.stock.xts(NAREIT)
	
	
	#--------------------------------


	tickers = '
COM = DBC;GSG + CRB

RExUS = [RWX] + VNQ + VGSIX
RE = [RWX] + VNQ + VGSIX
RE.US = [ICF] + VGSIX

EMER.EQ = [EEM] + VEIEX
EMER.FI = [EMB] + PREMX

GOLD = [GLD] + GOLD,
US.CASH = [BIL] + TB3M,
SHY + TB3Y,

US.HY = [HYG] + VWEHX

# Bonds
US.BOND = [AGG] + VBMFX
INTL.BOND = [BWX] + BEGBX

JAPAN.EQ = [EWJ] + FJPNX
EUROPE.EQ = [IEV] + FIEUX
US.SMCAP = IWM;VB + NAESX
TECH.EQ = [QQQ] + ^NDX
US.EQ = [VTI] + VTSMX + VFINX
US.MID = [VO] + VIMSX
EAFE = [EFA] + VDMIX + VGTSX

MID.TR = [IEF] + VFITX
CORP.FI = [LQD] + VWESX
TIPS = [TIP] + VIPSX + LSGSX
LONG.TR = [TLT] + VUSTX
'


	data.proxy = env()
	getSymbols.extra(tickers, src = 'yahoo', from = '1970-01-01', env = data.proxy, raw.data = raw.data, auto.assign = T)

	data.proxy.raw = raw.data
	save(data.proxy.raw, file='data/data.proxy.raw.Rdata',compress='gzip') 
	save(data.proxy, file='data/data.proxy.Rdata',compress='gzip') 
}



