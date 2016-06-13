
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

	# Find Januaries with return greater than threshhold
  	get.january.seasonality <- reactive({  	
  		price = getData()
	  
		# convert to monthly
		price = Cl(to.monthly(price, indexAt='endof'))				
		ret = price / mlag(price) - 1
	
		#*****************************************************************
		# Find Januaries with return > 4%
		#****************************************************************** 
		index =  which( date.month(index(ret)) == 1 & ret > input$min.january / 100 )
		
		# create summary table with return in January and return for the whole year
		temp = c(coredata(ret),rep(0,12))
		out = cbind(ret[index], sapply(index, function(i) prod(1 + temp[i:(i+11)])-1))
			colnames(out) = spl('January,Year')
			  		
		100 * out
	})

	# Make table
	makeSeasonalityTable <- reactive({  			
  		temp = get.january.seasonality()
  			years = date.year(index(temp))
  	  	temp = as.matrix(temp)
  			rownames(temp) = years
  		temp
  	})
  				
	# Make table
	makeStatsTable <- reactive({  	
		out = get.january.seasonality()
		as.matrix(compute.stats( as.list(out),
			list(Min=function(x) min(x,na.rm=T),
				Max=function(x) max(x,na.rm=T),
				Avg=function(x) mean(x,na.rm=T),
				Med=function(x) median(x,na.rm=T),
				StDev=function(x) sd(x,na.rm=T)
				)
			))
	})		
  	
    #*****************************************************************
    # Not Reactive helper functions
	#*****************************************************************
	# Make plot
	makePlot <- function() {  	
		out = get.january.seasonality()
		
		col=col.add.alpha(spl('black,gray'),200)
		pos = barplot(out, border=NA, beside=T, axisnames = F, axes = FALSE,
			col=col, main=paste('Annual Return When', input$symbol, 'Rises More than', input$min.january, '% in January'))
			axis(1, at = colMeans(pos), labels = date.year(index(out)), las=2)
			axis(2, las=1)
		grid(NA, NULL)
		abline(h= mean(out$Year), col='red', lwd=2)		
		plota.legend(spl('January,Annual,Annual Average'),  c(col,'red'))
				
		plota.add.copyright()
	}
	
    #*****************************************************************
    # Update plot(s) and table(s)
    #******************************************************************    	
	# Generate a plot
	output$seasonalityPlot <- renderPlot({
		makePlot()
	}, height = 400, width = 600)

	# Generate a table
  	output$seasonalityTable <- reactive({
  		temp = cbind(	
	  		tableColor(makeSeasonalityTable(), digits=1),
			tableColor(makeStatsTable(), digits=1)
		)
		colnames(temp) = c('Details', 'Summary')
		tableColor(temp, include.rownames=FALSE, sanitize.text.function=identity, border=0)
	})
	
	
    #*****************************************************************
    # Download
    #******************************************************************    
    # Download pdf report
	output$downloadReport <- downloadHandler(
    	filename = 'report.pdf',
    	content = function(file) {
    		pdf(file = file, width=8.5, height=11)
      			
    		layout(matrix(c(1,1,2,3),2,2,byrow=T))
    		makePlot()
			plot.table(round(makeSeasonalityTable(),1))
				plota.add.copyright()
			plot.table(round(makeStatsTable(),1))
				plota.add.copyright()
				      		
		    dev.off()
    	}
	)	
		
	# Download csv data
	output$downloadData <- downloadHandler(
    	filename = 'data.csv',
    	content = function(file) {
    		cat('File:',file, '\n')
    		cat('Seasonality:\n', file=file, append=F)
      		write.table(makeSeasonalityTable(), sep=',', col.names=NA, quote=F, file=file, append=T)      		
      		
      		cat('\n\nStats:\n', file=file, append=T)
      		write.table(makeStatsTable(), sep=',', col.names=NA, quote=F, file=file, append=T)      		
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
