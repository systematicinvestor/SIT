
# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(

	headerPanel(""),

	# Sidebar with a slider input for number of observations
	sidebarPanel(   
		textInput("symbol", "Stock - Yahoo Ticker:", value = "SPY"),
		textInput("cash", "Cash - Yahoo Ticker:", value = "TLT"),		
		br(),
		selectInput("smaLen", strong("Moving Average:"), choices =  seq(20,200,by=10),selected=100),
		br(),
		submitButton("Run"),
		htmlOutput("status")
	),

 
	# Show a plot of the generated distribution
	mainPanel(
		tabsetPanel(
			tabPanel("Main", 
				plotOutput("strategyPlot"),
				br(),
				tableOutput("sidebysideTable"),
				h4("Annual Perfromance"),
				tableOutput("annualTable"),
				h4("Transition Map"),
				plotOutput("transitionPlot"),
				h4("Last 20 Trades"),
				tableOutput("tradesTable"),				
				downloadButton("downloadReport", "Download Report"),
				downloadButton("downloadData", "Download Data"),
				br(),
				br()	
			),			
        
			tabPanel("About",
				p('This application demonstrates how to back-test a Market Filter strategy using ',
				a("Shiny", href="http://www.rstudio.com/shiny/", target="_blank"), 'framework and',
				a("Systematic Investor Toolbox", href="http://systematicinvestor.wordpress.com/systematic-investor-toolbox/", target="_blank"),
				'.
				The Market Filter strategy invests in stock while the stock price is above the
				moving average and goes in cash otherwise. The periods where Market Filter strategy is
				invested are highlighted with green.'
				),
								
				br(),
				
				strong('Author'), p('Michael Kapler', a('Systematic Investor Blog', href="http://systematicinvestor.wordpress.com", target="_blank")),
				
				br(),
				
				strong('Code'), p('Original source code for this application at',
				a('GitHub', href='https://github.com/systematicinvestor/SIT/Shiny/market.filter')),
				
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

