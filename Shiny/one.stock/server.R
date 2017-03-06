
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
  	  		
	  		symbol = toupper(input$symbol)
  			if (is.null(symbol_env[[symbol]]))
			tryCatch({
				symbol_env[[symbol]] = getSymbols(symbol, from='1970-01-01', src='yahoo', auto.assign = FALSE)
			}, error = function(e) { stop(paste('Problem getting prices for',symbol)) })
			adjustOHLC(symbol_env[[symbol]], use.Adjusted=T)			
	})
  	
	# Determine dates range
	getDateRange <- reactive({  	
		data = getData()
		max(1,nrow(data) - input$dateRange) : nrow(data)
	})
		
	# Make table
	makeStatsTable <- reactive({  	
	tryCatch({
		# download Key Statistics from yahoo
		url = paste('http://finance.yahoo.com/q/ks?s=', input$symbol, sep = '')
		txt = join(readLines(url))
		
		# extract Valuation Measures table from this page
		temp = extract.table.from.webpage(txt, 'Market Cap', has.header = F)
			colnames(temp) = c('Key Statistics', input$symbol)
		temp
	}, error = function(e) { stop(paste('Problem getting Key Statistics for',input$symbol)) })
	})
	
    #*****************************************************************
    # Not Reactive helper functions
	#*****************************************************************
	# Make stock plot
	makeStockPlot <- function() {  	
		out = getData()
		
		y = out
		sma = SMA(Cl(y), as.double(input$smaLen))[getDateRange(),]
		rsi = RSI(Cl(y), 20)[getDateRange(),]		
		y = y[getDateRange(),]		

		# plote candles and volume and table
		layout(c(1,1,1,1,2,3))

		plota(y, type = 'candle', main = input$symbol, plotX = F)
			plota.lines(sma, col='blue')
			plota.legend(c(input$symbol, paste('SMA', input$smaLen)), 'green,blue', list(y,sma))
		
		# plot volume
		y = plota.scale.volume(y)
		plota(y, type = 'volume', plotX = F)
		
		# plot rsi
		plota(rsi, type = 'l', ylim=c(0,100))	
			col = col.add.alpha(spl('green,red'),100)
			plota.y.highlight(col=col[1], highlight=c(70,100))	
			plota.y.highlight(col=col[2], highlight=c(0,30))	
		abline(h = 50, col = col.add.alpha('gray20',100))
		plota.legend('RSI(20)', 'black', rsi)
				
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
  	output$statsTable <- reactive({
		temp = makeStatsTable()	
		tableColor(as.matrix(temp),include.rownames=FALSE)		
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
			plot.table(makeStatsTable())
				plota.add.copyright()
      		
		    dev.off()
    	}
	)	
		
	# Download csv data
	output$downloadData <- downloadHandler(
    	filename = 'data.csv',
    	content = function(file) {
      		write.csv(makeStatsTable(), file=file, row.names = F)
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
