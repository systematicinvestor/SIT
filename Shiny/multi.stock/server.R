
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
  		for(symbol in spl(toupper(input$symbols))) {
  			if (is.null(symbol_env[[symbol]]))
			tryCatch({
				symbol_env[[symbol]] = getSymbols(symbol, from='1970-01-01', src='yahoo', auto.assign = FALSE)
			}, error = function(e) { stop(paste('Problem getting prices for',symbol)) })
  			data[[symbol]] = symbol_env[[symbol]]
  		}
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)  			  				
  		bt.prep(data, align='keep.all')
		data		
	})
  	
	# Determine dates range
	getDateRange <- reactive({  	
		data = getData()
		max(1,nrow(data$prices) - input$dateRange) : nrow(data$prices)
	})
		
	# Make table
	makeCorTable <- reactive({  	
		out = getData()
		
		prices = out$prices[getDateRange(),]
		if( ncol(prices) == 1) return(NULL)

		# compute correlation
		ret = prices / mlag(prices) - 1
		100 * cor(coredata(ret), use='complete.obs',method='pearson')	
	})
	
    #*****************************************************************
    # Not Reactive helper functions
	#*****************************************************************
	# Make stock plot
	makeStockPlot <- function() {  	
		out = getData()
		
		prices = out$prices[getDateRange(),]
		if( input$plotReturnsFlag ) prices = scale.one(prices)
		
		plota.matplot(prices)
		plota.add.copyright()
	}
	
	
    #*****************************************************************
    # Update plot(s) and table(s)
    #******************************************************************    	
	# Generate a plot
	output$stockPlot <- renderPlot({
		makeStockPlot()
	}, height = 400, width = 600)

	# Generate a table
  	output$corTable <- reactive({
		temp = makeCorTable()	
		tableColor(as.matrix(temp),digits=1)		
	})
	
    #*****************************************************************
    # Download
    #******************************************************************    
    # Download pdf report
	output$downloadReport <- downloadHandler(
    	filename = 'report.pdf',
    	content = function(file) {
    		pdf(file = file, width=8.5, height=11)
      			
    		makeStockPlot()
			plot.table(round(makeCorTable(),1), 'Correlation')
				plota.add.copyright()
      		
		    dev.off()
    	}
	)	
		
	# Download csv data
	output$downloadData <- downloadHandler(
    	filename = 'data.csv',
    	content = function(file) {
      		write.csv(makeCorTable(), file)
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
