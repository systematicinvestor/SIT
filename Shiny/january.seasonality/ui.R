
# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(

	headerPanel(""),

	# Sidebar with a slider input for number of observations
	sidebarPanel(   
		# make a non reactive text input with Update button
		createNonReactiveTextInput("symbol", "Yahoo Ticker:", value = "SPY", "Update"), 
		br(),
		numericInput("min.january", strong("Minimum January Return %:"), 2),
		br(),
		br(),
		htmlOutput("status") 
	),

	# Show a plot of the generated distribution
	mainPanel(
		tabsetPanel(
			tabPanel("Main", 
				h3("Big January", align="center"), 
  					
				plotOutput("seasonalityPlot"),
				tableOutput("seasonalityTable"),
				
				downloadButton("downloadReport", "Download Report"),
				downloadButton("downloadData", "Download Data")	 
			),
        
			tabPanel("About",
				p('This application demonstrates how to create a seasonality analysis study, 
				and produce sample summary report using',
				a("Shiny", href="http://www.rstudio.com/shiny/", target="_blank"), 'framework and',
				a("Systematic Investor Toolbox", href="http://systematicinvestor.wordpress.com/systematic-investor-toolbox/", target="_blank"),
				'. This example is based on the',
				a('An Example of Seasonality Analysis', href="http://systematicinvestor.wordpress.com/2013/02/04/an-example-of-seasonality-analysis/", target="_blank"),
				'post.'), 
				
				p('The idea is to first focus on returns in the month of January 
				that are greater than given treshold and next study the returns for the whole year. This study is based on the',		
				a('S&P Annual Performance After a Big January', href="http://www.avondaleam.com/2013/02/s-annual-performance-after-big-january.html", target="_blank"),
				'post by', a('Avondale Asset Management', href="http://www.avondaleam.com", target="_blank")),
				
				br(),
				
				strong('Author'), p('Michael Kapler', a('Systematic Investor Blog', href="http://systematicinvestor.wordpress.com", target="_blank")),
				
				br(),
				
				strong('Code'), p('Original source code for this application at',
				a('GitHub', href='https://github.com/systematicinvestor/SIT/Shiny/january.seasonality')),
				
				br(),
				
				strong('References'),
				p(HTML('<ul>'),
        			HTML('<li>'),a('An Example of Seasonality Analysis', href="http://systematicinvestor.wordpress.com/2013/02/04/an-example-of-seasonality-analysis/", target="_blank"),HTML('</li>'),
        			HTML('<li>'),'The web application is built with the amazing', a("Shiny.", href="http://www.rstudio.com/shiny/", target="_blank"),HTML('</li>'),
        			HTML('<li>'),a('SIR application by Samuel M. Jenness', href="http://glimmer.rstudio.com/smjenness/SIR/", target="_blank"),HTML('</li>'),
        			HTML('<li>'),a('SIR application code by Samuel M. Jenness', href="https://github.com/smjenness/Shiny/tree/master/SIR", target="_blank"),HTML('</li>'),
				HTML('</ul>'))
			)    
		)
	)
))

