
# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(

	headerPanel(""),

	# Sidebar with a slider input for number of observations
	sidebarPanel(   
		tags$label("Yahoo Ticker(s) separated by comma or new line:"),
		tags$textarea(id = "symbols", rows=10, cols=10, "XLY,XLP,XLE,XLF\nXLV,XLI,XLB,XLK\nXLU"),
		#createNonReactiveTextInputCustom("symbols", "Yahoo Ticker(s) separated by comma or new line:", "textarea", "Update", enableEnter=F, opts=list(rows=10, cols=10, "XLY,XLP,XLE,XLF\nXLV,XLI,XLB,XLK\nXLU")),
		
		br(),
		selectInput("momLen", strong("Momentum Length (months):"), choices =  1:12,selected=6),
		numericInput("topn", "Invest in top # funds:", 2),
		numericInput("keepn", "Keep position till rank is at least:", 6),				
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
				p('This application demonstrates how to back-test a ETF Sector strategy using ',
				a("Shiny", href="http://www.rstudio.com/shiny/", target="_blank"), 'framework and',
				a("Systematic Investor Toolbox", href="http://systematicinvestor.wordpress.com/systematic-investor-toolbox/", target="_blank"),				
				'. This example is based on the',
				a('Multi-Asset Backtest : Rotational Trading Strategies', href="http://systematicinvestor.wordpress.com/2011/12/06/multi-asset-backtest-rotational-trading-strategies/", target="_blank"),
				'post.'), 				
				p('The Sector Rotation strategy selects top N funds (i.e. 2 funds) based on the momentum (i.e 6 month returns)
				and adjusts the holdings only if these funds drop their momentum rank below 
				a threshold (i.e. replace all holdings with momentum rank greater than 6).
				This study is based on the',		
				a('ETF Sector Strategy', href="http://www.etfscreen.com/sectorstrategy.php", target="_blank"),
				'post by', a('ETF Screen', href="http://www.etfscreen.com", target="_blank")),				
				
				br(),
				
				strong('Author'), p('Michael Kapler', a('Systematic Investor Blog', href="http://systematicinvestor.wordpress.com", target="_blank")),
				
				br(),
				
				strong('Code'), p('Original source code for this application at',
				a('GitHub', href='https://github.com/systematicinvestor/SIT/Shiny/sector.rotation')),
				
				br(),
				
				strong('References'),
				p(HTML('<ul>'),
					HTML('<li>'),a('Multi-Asset Backtest : Rotational Trading Strategies', href="http://systematicinvestor.wordpress.com/2011/12/06/multi-asset-backtest-rotational-trading-strategies/", target="_blank"),HTML('</li>'),
        			HTML('<li>'),'The web application is built with the amazing', a("Shiny.", href="http://www.rstudio.com/shiny/", target="_blank"),HTML('</li>'),
        			HTML('<li>'),a('SIR application by Samuel M. Jenness', href="http://glimmer.rstudio.com/smjenness/SIR/", target="_blank"),HTML('</li>'),
        			HTML('<li>'),a('SIR application code by Samuel M. Jenness', href="https://github.com/smjenness/Shiny/tree/master/SIR", target="_blank"),HTML('</li>'),
				HTML('</ul>'))
			)    
		)
	)
))

