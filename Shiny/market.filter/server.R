
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
  		for(symbol in c(getStock(),getCash()) ) {
  			if (is.null(symbol_env[[symbol]]))
			tryCatch({
				symbol_env[[symbol]] = getSymbols(symbol, from='1970-01-01', src='yahoo', auto.assign = FALSE)
			}, error = function(e) { stop(paste('Problem getting prices for',symbol)) })
  			data[[symbol]] = symbol_env[[symbol]]
  		}
  		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
  		bt.prep(data, align='remove.na')
		data		
	})
  	
	# Helper fns
	getStock <- reactive({ toupper(input$symbol) })
	getCash <- reactive({ toupper(input$cash) })

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
		
		# find period ends
		period.ends = endpoints(prices, 'months')
			period.ends = period.ends[period.ends > 0]
	 		
		models = list()
		
		stock = getStock()
		cash = getCash()
		
		#*****************************************************************
		# Buy & Hold
		#****************************************************************** 
		data$weight[] = NA
			data$weight[,stock] = 1
		models$buy.hold = bt.run.share(data, clean.signal=T)
	
		#*****************************************************************
		# MA cross-over strategy
		#****************************************************************** 
		sma = SMA(prices[, stock], as.double(input$smaLen))
		signal = (prices[, stock] > sma)[period.ends]
		
		data$weight[] = NA
			data$weight[period.ends, stock ] = iif(signal, 1, 0)
			data$weight[period.ends, cash] = iif(signal, 0, 1)
		models$market.filter = bt.run.share(data, clean.signal=T, trade.summary = T)
		
		temp = rep(NA, nperiods)
			temp[period.ends] = signal
		models$market.filter$highlight = ifna.prev(temp)
		
		rev(models)
	}, error = function(e) { stop(paste('Problem running Back Test:', e)) })
	})
	
		
	# Make table
	makeSidebysideTable <- reactive({
		models = getBackTest()
		plotbt.strategy.sidebyside(models, return.table=T, make.plot=F)
	})

	# Make table
	makeAnnualTable <- reactive({
		models = getBackTest()
		plotbt.monthly.table(models[[1]]$equity, make.plot = F)
	})
	
	# Make table
	makeTradesTable <- reactive({
		models = getBackTest()
		model = models[[1]]
		
		if (!is.null(model$trade.summary)) {
			ntrades = min(20, nrow(model$trade.summary$trades))		
			last(model$trade.summary$trades, ntrades)
		}
	})
		
    #*****************************************************************
    # Not Reactive helper functions
	#*****************************************************************
	
	
    #*****************************************************************
    # Update plot(s) and table(s)
    #******************************************************************    	
	# Generate a plot
	output$strategyPlot <- renderPlot({
		models = getBackTest()
		
		plota.theme(col.x.highlight = col.add.alpha('green',50))
		plotbt.custom.report.part1(models, x.highlight = models$market.filter$highlight)  					
	}, height = 400, width = 600)

	# Generate a table
  	output$sidebysideTable <- reactive({
		temp = makeSidebysideTable()	
		tableColor(as.matrix(temp))		
	})
	
	# Generate a table
  	output$annualTable <- reactive({
		temp = makeAnnualTable()	
		tableColor(as.matrix(temp))		
	})

	# Generate a plot
	output$transitionPlot <- renderPlot({
		models = getBackTest()
		plotbt.transition.map(models[[1]]$weight)	
	}, height = 400, width = 600)
		
	# Generate a table
  	output$tradesTable <- reactive({
		temp = makeTradesTable()	
		tableColor(as.matrix(temp))		
	})
	
	
	
    #*****************************************************************
    # Download
    #******************************************************************    
    # Download pdf report
	output$downloadReport <- downloadHandler(
    	filename = 'report.pdf',
    	content = function(file) {
    		pdf(file = file, width=8.5, height=11)
      			
    		models = getBackTest()
    		
			plota.theme(col.x.highlight = col.add.alpha('green',50))
    		plotbt.custom.report(models, trade.summary = T, x.highlight = models$market.filter$highlight)
				plota.add.copyright()
      		
		    dev.off()
    	}
	)	
		
	# Download csv data
	output$downloadData <- downloadHandler(
    	filename = 'data.csv',
    	content = function(file) {
    		cat('Summary Performance:\n', file=file, append=F)
      		write.table(makeSidebysideTable(), sep=',', col.names=NA, quote=F, file=file, append=T)      		
      		
      		cat('\n\nAnnual Perfromance:\n', file=file, append=T)
      		write.table(makeAnnualTable(), sep=',', col.names=NA, quote=F, file=file, append=T)      

      		cat('\n\nLast 20 Trades:\n', file=file, append=T)
      		write.table(makeTradesTable(), sep=',', col.names=NA, quote=F, file=file, append=T)      
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
