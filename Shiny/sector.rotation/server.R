
# Define server
shinyServer(function(input, output) {

	# Create an environment for storing data
	symbol_env <- new.env()
	
    #*****************************************************************
    # Shared Reactive functions
  	# http://rstudio.github.com/shiny/tutorial/#inputs-and-outputs
    #******************************************************************    	
    # Get stock data
  	getData <- reactive({  	
  		cat('getData was called\n')
  	
  		data <- new.env()
  		for(symbol in getStocks() ) {
  			if (is.null(symbol_env[[symbol]]))
			tryCatch({
				symbol_env[[symbol]] = getSymbols(symbol, from='1970-01-01', src='yahoo', auto.assign = FALSE)
			}, error = function(e) { stop(paste('Problem getting prices for',symbol)) })
  			data[[symbol]] = adjustOHLC(symbol_env[[symbol]], use.Adjusted=T)  			
  		}
  		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
  		bt.prep(data, align='keep.all', dates='2000::')
		data		
	})
  	
	# Helper fns
	getStocks <- reactive({ spl(toupper(gsub('\n',',',input$symbols))) })

	getBackTest <- reactive({ 
		#*****************************************************************
		# Load historical data
		#******************************************************************  
		data = getData()

	tryCatch({		
		#*****************************************************************
		# Code Strategies
		#****************************************************************** 
		prices = data$prices   
			nperiods = nrow(prices)
			n = ncol(prices)
		
		# find period ends
		period.ends = endpoints(prices, 'months')
			period.ends = period.ends[period.ends > 0]
	 		
		models = list()

		#*****************************************************************
		# Code Strategies
		#****************************************************************** 
		dates = '2001::'
					
		# Equal Weight
		data$weight[] = NA
			data$weight[period.ends,] = ntop(prices, n)[period.ends,]	
		models$equal.weight = bt.run.share(data, clean.signal=F, dates=dates)
		
		# model parameters	
		momLen = as.numeric(input$momLen) * 22
		topn = floor(as.numeric(input$topn))
		keepn = floor(as.numeric(input$keepn))
		
		# Rank on momLen month return
		position.score = prices / mlag(prices, momLen)	
		
		# Select Top topn funds
		data$weight[] = NA
			data$weight[period.ends,] = ntop(position.score[period.ends,], topn)	
		models[[ paste('top', topn, sep='') ]] = bt.run.share(data, clean.signal=T, trade.summary=T, dates=dates)
	
		# Seletop Top topn funds,  and Keep then till they are in 1:keepn rank
		data$weight[] = NA
			data$weight[period.ends,] = ntop.keep(position.score[period.ends,], topn, keepn)	
		models[[ paste('top', topn, '.keep', keepn, sep='') ]] = bt.run.share(data, clean.signal=T, trade.summary=T, dates=dates)
	
		
		
		rev(models)
	}, error = function(e) { stop(paste('Problem running Back Test:', e)) })
	})
	
		
	# Make table
	makeSidebysideTable <- reactive({
		models = getBackTest()
		plotbt.strategy.sidebyside(models, return.table=T, make.plot=F)
	})
	
		
    #*****************************************************************
    # Not Reactive helper functions
	#*****************************************************************
	# Make table
	makeTradesTable <- function(i = 1) {
		models = getBackTest()
		model = models[[i]]
		
		if (!is.null(model$trade.summary)) {
			ntrades = min(20, nrow(model$trade.summary$trades))		
			last(model$trade.summary$trades, ntrades)
		}
	}
	
	# Make table
	makeAnnualTable <- function(i = 1) {
		models = getBackTest()
		plotbt.monthly.table(models[[i]]$equity, make.plot = F)
	}
	
    #*****************************************************************
    # Update plot(s) and table(s)
    #******************************************************************    	
	# Generate a plot
	output$strategyPlot <- renderPlot({
		models = getBackTest()		
		plotbt.custom.report.part1(models)  					
			plota.add.copyright()
	}, height = 400, width = 600)

	# Generate a table
  	output$sidebysideTable <- reactive({
		temp = makeSidebysideTable()	
		tableColor(as.matrix(temp))		
	})
	
	# Generate a table
  	output$annualTable <- reactive({
		tableColor(as.matrix(makeAnnualTable(1)))
	})

	# Generate a plot
	output$transitionPlot <- renderPlot({
		models = getBackTest()
		plotbt.transition.map(models[[1]]$weight)
			plota.add.copyright()
	}, height = 400, width = 600)
		
	# Generate a table
  	output$tradesTable <- reactive({
  		tableColor(as.matrix(makeTradesTable(1)), include.rownames=FALSE)
	})
	
	
	
    #*****************************************************************
    # Download
    #******************************************************************    
    # Download pdf report
	output$downloadReport <- downloadHandler(
    	filename = 'report.pdf',
    	content = function(file) {
    		pdf(file = file, width=8.5, height=11)
      			
    		models <<- getBackTest()
    		
    		plotbt.custom.report.part1(models)
    			plota.add.copyright()
    		
    		plotbt.custom.report.part2(models[1])
    			plota.add.copyright()
    		plotbt.custom.report.part3(models[1], trade.summary=T)
    			plota.add.copyright()
    		
    		plotbt.custom.report.part2(models[2])
    			plota.add.copyright()
    		plotbt.custom.report.part3(models[2], trade.summary=T)
    			plota.add.copyright()
    		
			# Plot Portfolio Turnover for each strategy
			data = getData()
			layout(1)
			barplot.with.labels(sapply(models, compute.turnover, data), 'Average Annual Portfolio Turnover')    		
				plota.add.copyright()
      		
		    dev.off()
    	}
	)	
		
	# Download csv data
	output$downloadData <- downloadHandler(
    	filename = 'data.csv',
    	content = function(file) {
    		models = getBackTest()
    	
    		cat('Summary Performance:\n', file=file, append=F)
      		write.table(makeSidebysideTable(), sep=',', col.names=NA, quote=F, file=file, append=T)      		
      		
      	for(i in 1:2) {
      		cat('\n\n', names(models)[i] ,'Annual Perfromance:\n', file=file, append=T)
      		write.table(makeAnnualTable(i), sep=',', col.names=NA, quote=F, file=file, append=T)      

      		cat('\n\n', names(models)[i] ,'Last 20 Trades:\n', file=file, append=T)
      		write.table(makeTradesTable(i), sep=',', col.names=NA, quote=F, file=file, append=T)      
      	}
    	}
  	)	
	
    #*****************************************************************
    # Update status message 
    #******************************************************************    
	output$status <- renderUI({
		out = tryCatch( getData(), error=function( err ) paste(err))	    				
		if( is.character( out ) ) 
			HTML(paste("<b>Status</b>: <b><font color='red'>Error:</font></b>",out))
		else
			HTML("<b>Status</b>: <b><font color='green'>Ok</font></b>")		
	})
	
	
})
