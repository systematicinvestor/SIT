library(shiny)

# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(

  # Application title
  headerPanel("Retirement : simulating wealth with random returns, inflation and withdrawals"),

  # Sidebar with a slider input for number of observations
  sidebarPanel(
    sliderInput("n.obs", 
                "Number of observations (in Years):", 
                min = 0, 
                max = 40, 
                value = 20),

    sliderInput("start.capital", 
                "Initial capital invested :", 
                min = 100000, 
                max = 10000000, 
                value = 2000000,
                step = 100000,
                format="$#,##0",
                locale="us"),

    sliderInput("annual.mean.return", 
                "Annual return from investments (in %):", 
                min = 0.0, 
                max = 30.0, 
                value = 5.0,
                step = 0.5),

    sliderInput("annual.ret.std.dev", 
                "Annual volatility from investments (in %):", 
                min = 0.0, 
                max = 25.0, 
                value = 7.0, 
                step = 0.1),

    sliderInput("annual.inflation", 
                "Annual inflation (in %):", 
                min = 0, 
                max = 20, 
                value = 2.5,
                step = 0.1),

    sliderInput("annual.inf.std.dev", 
                "Annual inflation volatility. (in %):", 
                min = 0.0, 
                max = 5.0,
                value = 1.5,
                step = 0.05),

    sliderInput("monthly.withdrawals", 
                "Monthly capital withdrawals:", 
                min = 1000, 
                max = 100000, 
                value = 10000,
                step = 1000,
                format="$#,##0",
                locale="us",),
                
    sliderInput("n.sim", 
                "Number of simulations:", 
                min = 0, 
                max = 2000, 
                value = 200)
                
  ),

  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("distPlot", height = "700px")
  )
))
