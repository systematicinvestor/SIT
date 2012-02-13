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
# Collection of Utilities to work with Fundamental Data from advfn
# Copyright (C) 2012  Michael Kapler
#
# For more information please visit my blog at www.SystematicInvestor.wordpress.com
# or drop me a line at TheSystematicInvestor at gmail
###############################################################################


###############################################################################
# Get Fundamental Data from advfn
# http://advfn.com/p.php?pid=financials&symbol=CSCO&mode=quarterly_reports
# http://advfn.com/p.php?pid=financials&symbol=CSCO&mode=annual_reports
###############################################################################
fund.data <- function
(
	Symbol, 		# ticker 
	n=10, 			# number of periods
	mode=c('quarterly','annual'), # periodicity
	max.attempts=5	# maximum number of attempts to download before exiting
)
{
	all.data = c()
	option.value = -1
	
	repeat {
		# download Quarterly Financial Report data
		if(option.value >= 0) {
			url = paste('http://advfn.com/p.php?pid=financials&symbol=', Symbol, '&mode=', mode[1], '_reports&istart_date=', option.value, sep = '')	
		} else {
			url = paste('http://advfn.com/p.php?pid=financials&symbol=', Symbol, '&mode=', mode[1], '_reports', sep = '')
		}
		cat('Downloading', url, '\n')
		
		#txt = join(readLines(url))		
		for(iattempt in 1:max.attempts) { 
			flag = T
		    tryCatch({
		    	txt = join(readLines(url))
			}, interrupt = function(ex) {
				flag <<-  F
		  		Sys.sleep(0.1)
			}, error = function(ex) {
				flag <<-  F
				Sys.sleep(0.1)
			}, finally = {
				if(flag) break
			})
		}
		
		# extract table from this page
		data = extract.table.from.webpage(txt, 'INDICATORS', hasHeader = T)
			colnames(data) = data[1,]
			rownames(data) = data[,1]
			data = data[,-1,drop=F]
		
		# only add not already present data
		add.index = which( is.na(match( colnames(data), colnames(all.data) )) )			
		all.data = cbind(data[,add.index,drop=F], all.data)
	
		# check if it is time to stop
		if(ncol(all.data) >= n) break
		if(option.value == 0)  break
		
		# extract option value to go to the next page
		temp = gsub(pattern = '<option', replacement = '<tr>', txt, perl = TRUE)
		temp = gsub(pattern = '</option>', replacement = '</tr>', temp, perl = TRUE)	
		temp = extract.table.from.webpage(temp, 'All amounts', hasHeader = T)
		
		index.selected = grep('selected', temp[,1])
		option.value = as.double( gsub('.*value=\'([0-9]*).*', '\\1', temp[index.selected,1]) )
		
		if(option.value > 0) {
			# can only get 5 time periods at a time
			option.value = option.value - 5
			option.value = max(0, option.value)		
		} else {
			break
		}
	}
	
	if( ncol(all.data) > n ) {	
		return(all.data[,(ncol(all.data)-n+1):ncol(all.data)])
	} else {
		return(all.data)
	}
}


###############################################################################
# determine date when fundamental data is available
# use 'date preliminary data loaded' when available
# otherwise lag 'quarter end date' 2 months for Q1/2/3 and 3 months for Q4
###############################################################################		
date.fund.data <- function(data)
{
	# construct date
	quarter.end.date = as.Date(paste(data['quarter end date',], '/1', sep=''), '%Y/%m/%d')	
	quarterly.indicator = data['quarterly indicator',]
	date.preliminary.data.loaded = as.Date(data['date preliminary data loaded',], '%Y-%m-%d') + 1
	
	months = seq(quarter.end.date[1], tail(quarter.end.date,1)+365, by='1 month') 
	index = match(quarter.end.date, months)
	quarter.end.date = months[ iif(quarterly.indicator == '4', index+3, index+2) + 1 ] - 1
		
	fund.date = date.preliminary.data.loaded
		fund.date[is.na(fund.date)] = quarter.end.date[is.na(fund.date)] 

	return(fund.date)
}

###############################################################################
# Lookup fundamental data item index
###############################################################################		
get.fund.data.index <- function
(
	label, 	# label of fundamental data item you looking for
	fund,	# matrix with fundamental data 
	silent = T	# print debug information
) 
{			
	names = rownames(fund)
		
	# first try exact name match
	index = grep(label, names, ignore.case = T)
				
	# next try match all words in the label
	if( len(index) == 0 ) {
		labels = spl(label,' ')
		n = len(labels)
	
		temp.count = rep(0,nrow(fund))
		for(ilabel in labels) {
			index = grep(ilabel, rownames(fund), ignore.case = T)
			if(len(index)>0) temp.count[index] = temp.count[index]+1
		}
		index = which(temp.count == n)
		if( !silent ) cat('Exact label not found, trying partial match\n')			
	}
		
	if( len(index) > 0 ) {
		# use similar length match
		if( len(index) > 1 ) {
			if( !silent ) cat('Possible Matches', rownames(fund)[index], '\n', sep=' | ')
			index = index[ which.min(nchar(names[index]) - nchar(label)) ]
		}
		
		if( !silent ) cat('Match =', rownames(fund)[index], '\n')

		index[1]					
	} else {
		if( !silent ) cat('No Match Found for', label, '\n')
		c()
	}
}

###############################################################################
# Extract and process fundamental data item
###############################################################################		
get.fund.data <- function
(
	label, 	# label of fundamental data item you looking for
	fund,	# matrix with fundamental data 
	fund.date, # dates
	is.12m.rolling=F, 	# flag to compute 12 month rolling sum
	cash.flow=F			# flag to adjust cash flow data items
) 
{			
	index = get.fund.data.index(label, fund)		
	if( len(index) == 0 ) return(as.xts(rep(NA,len(fund.date)), fund.date))
	
	# remove commas
	temp.q = as.double(gsub(',', '', fund[index,]))
		
	# cash flow items, start fresh in Q1 and acummulate till Q4
	if(cash.flow) {
		quarterly.indicator = fund['quarterly indicator',]
		temp.q = iif(quarterly.indicator == '1', temp.q, temp.q - mlag(temp.q))			
	}
			
	temp.q = as.xts(temp.q, fund.date)
						
	iif(is.12m.rolling, runSum(temp.q, 4), temp.q)			
}
