
# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(

	headerPanel(""),

	# Sidebar with a slider input for number of observations
	sidebarPanel(   
		# make a non reactive text input with Update button
		createNonReactiveTextInput("symbols", "Yahoo Ticker(s) separated by comma:", value = "AAPL,GOOG", "Update"),
		br(),
		htmlOutput("status")
	),

 
	# Show a plot of the generated distribution
	mainPanel(
		tabsetPanel(
			tabPanel("Main", 
				plotOutput("stockPlot"),
				HTML('<table border=0 width="100%"><tr bgcolor="#f5f5f5"><td>'), 
				div(style="width:80%;max-width:300px;",  	   				
          			sliderInput(inputId="dateRange", label=strong('Date Range(# days): '), min=100, max=5000, value=2000, step=100)
          		), HTML('</td><td>'), 
          		checkboxInput(inputId="plotReturnsFlag", label=strong('Plot Returns'), value=FALSE),
          		HTML('</td></tr></table>'), 
          		br(),
				tableOutput("corTable"),
				br(),
				downloadButton("downloadReport", "Download Report"),
				downloadButton("downloadData", "Download Data")
			),			
        
			tabPanel("About",
				p('This application demonstrates how to download and plot multiple stocks time series using',
				a("Shiny", href="http://www.rstudio.com/shiny/", target="_blank"), 'framework and',
				a("Systematic Investor Toolbox", href="http://systematicinvestor.wordpress.com/systematic-investor-toolbox/", target="_blank"),
				'.'),				
				br(),
				
				strong('Author'), p('Michael Kapler', a('Systematic Investor Blog', href="http://systematicinvestor.wordpress.com", target="_blank")),
				
				br(),
				
				strong('Code'), p('Original source code for this application at',
				a('GitHub', href='https://github.com/systematicinvestor/SIT/Shiny/multi.stock')),
				 				
				br(),
				
				strong('References'),
				p(HTML('<ul>'),
        			HTML('<li>'),'The web application is built with the amazing', a("Shiny.", href="http://www.rstudio.com/shiny/", target="_blank"),HTML('</li>'),
        			HTML('<li>'),a('SIR application by Samuel M. Jenness', href="http://glimmer.rstudio.com/smjenness/SIR/", target="_blank"),HTML('</li>'),
        			HTML('<li>'),a('SIR application code by Samuel M. Jenness', href="https://github.com/smjenness/Shiny/tree/master/SIR", target="_blank"),HTML('</li>'),
				HTML('</ul>'))
			)    
		)
	)
))

