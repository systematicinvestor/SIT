
bt.run.share.ex.example.match.adjusted.unadjusted = function()
{
	#*****************************************************************
	# To match backtest results using Adjusted data, the backtest using
	# UnAdjusted data must reinvest dividends right away. i.e.
	#   dividend.control = list(invest = 'rebalance')
	#*****************************************************************

	#*****************************************************************
	# Helper function
	#*****************************************************************
	load.data = function(adjusted = T) {
		tickers = 'SPY'
			
		data = env()
		getSymbols.extra(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)	
			# clone SPY
			data$SPY1 = data$SPY
			
			if(adjusted)
				for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
			else
				bt.unadjusted.add.div.split(data, infer.div.split.from.adjusted=T)	
			
		bt.prep(data, align='remove.na', fill.gaps = T)
		data
	}
	
	#*****************************************************************
	# Adjusted
	#*****************************************************************
	library(SIT)
	library(quantmod)
	
	data = load.data(T)
	
	period.ends = date.ends(data$prices,'years')
		nperiod.ends = len(period.ends)
		
	models = list()
	
	data$weight[] = NA
		data$weight[period.ends,] = matrix(c(0,1),nr=99,nc=2)[1:nperiod.ends,]
	models$a.basic = bt.run.share.ex(data, clean.signal=T, silent=F, adjusted = T
		)

	#*****************************************************************
	# UnAdjusted
	#*****************************************************************
	data = load.data(F)

	# must reinvest dividends right away to match performance of adjusted backtest
	dividend.control = list(invest = 'rebalance')
		
	data$weight[] = NA
		data$weight[period.ends,] = matrix(c(0,1),nr=99,nc=2)[1:len(period.ends),]
	models$basic = bt.run.share.ex(data, clean.signal=T, silent=F, adjusted = F,
		dividend.control = dividend.control)

	plotbt.strategy.sidebyside(models, make.plot=F, return.table=T)
	
	plotbt(models, plotX = T, log = 'y', LeftMargin = 3, main = NULL)	    	
}



bt.run.share.ex.example.commissions.adjusted.unadjusted = function()
{
	#*****************************************************************
	# Commissions make a big difference when working with 
	# UnAdjusted data, Lot size plays small role
	#*****************************************************************

	#*****************************************************************
	# Helper function
	#*****************************************************************
	load.data = function(adjusted = T) {
		tickers = 'MMM, AA, CAT, KO, HPQ'
			
		data = env()
		getSymbols.extra(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)	
			if(adjusted)
				for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
			else
				bt.unadjusted.add.div.split(data, infer.div.split.from.adjusted=T)	
			
		bt.prep(data, align='remove.na', fill.gaps = T)
		data
	}
	
	#*****************************************************************
	# Adjusted
	#*****************************************************************
	library(SIT)
	library(quantmod)
	
	data = load.data(T)
		
	period.ends = date.ends(data$prices,'months')
	  
	models = list()
	
	commission = list(cps = 0.01, fixed = 10.0, percentage = 0.0)
	
	n = ncol(data$prices)
	weights = rep.row(rep(1/n, n), len(period.ends))

	

	data$weight[] = NA
		data$weight[period.ends,] = weights
	models$a.base = bt.run.share.ex(data, clean.signal=F, silent=T, #commission=commission, 
		#lot.size=100,
		#control = list(round.lot = list(select = 'minimum.turnover', diff.target = 5/100)),
		adjusted = T
	)

	data$weight[] = NA
		data$weight[period.ends,] = weights
	models$a.base.com = bt.run.share.ex(data, clean.signal=F, silent=T, commission=commission, 
		#lot.size=100,
		#control = list(round.lot = list(select = 'minimum.turnover', diff.target = 5/100)),
		adjusted = T
	)

	#*****************************************************************
	# UnAdjusted
	#*****************************************************************
	data = load.data(F)
	
	# must reinvest dividends right away to match performance of adjusted backtest
	dividend.control = list(invest = 'rebalance')	
	dividend.control = list(invest = 'cash')	

	data$weight[] = NA
		data$weight[period.ends,] = weights
	models$u.base = bt.run.share.ex(data, clean.signal=F, silent=T, #commission=commission, 
		#lot.size=100,		
		#control = list(round.lot = list(select = 'minimum.turnover', diff.target = 5/100)),
		dividend.control = dividend.control,
		adjusted = F
	)

	data$weight[] = NA
		data$weight[period.ends,] = weights
	models$u.base.com = bt.run.share.ex(data, clean.signal=F, silent=T, commission=commission, 
		#lot.size=100,		
		#control = list(round.lot = list(select = 'minimum.turnover', diff.target = 5/100)),
		dividend.control = dividend.control,
		adjusted = F
	)

	
	
	

	plotbt.strategy.sidebyside(models, make.plot=F, return.table=T)
	
	plotbt(models, plotX = T, log = 'y', LeftMargin = 3, main = NULL)	
}

	