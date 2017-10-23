library(httr)
library(jsonlite)
library(shiny)
library(shinythemes)
library(dygraphs)
library(xts)

# Load GDAX public client API functions
source("rgdax/public_client.R")

# Use a proxy (NULL for no proxy)
proxy <- NULL

# Load GDAX logo
logo <- img(src ="gdax-logo-dark.svg",
            align = "top",
            width = "15%",
            height = "15%",
            style = "margin-left:20px;
                     margin-right:20px;
                     margin-bottom:20px")
            
# Application UI
ui <- fluidPage(
  
  # Define theme
  theme = shinytheme("flatly"),
  
  # Create title panel
  titlePanel(title = logo, windowTitle = "Shiny-GDAX"),
  tags$h1(),
    
  # Live data tab
  tabsetPanel(
    tabPanel("Monitor",
             tags$h1(),
             tags$h4("Options"),
             tags$h1(),
             fluidRow(
               column(2,
                      selectInput("baseCurrency",
                                  label = "Base currency:",
                                  choices = NULL)),
               column(2,
                      selectInput("quoteCurrency",
                                  label = "Quote currency:",
                                  choices = NULL)),
               column(2,
                      selectInput("granularity",
                                  label = "Granularity:",
                                  choices = list("1 min" = 60,
                                                 "5 min" = 300,
                                                 "15 min" = 900,
                                                 "1 h" = 3600,
                                                 "6 h" = 21600,
                                                 "1 d" = 86400),
                                  selected = 900)),
               column(2,
                      selectInput("graphType",
                                  label = "Graph type:",
                                  choices = list("Line" = "line",
                                                 "Candle" = "candle"),
                                  selected = "line"))
               ),
             tags$hr(),
             tags$h1(),
             tags$h4("Price graph"),
             tags$h1(),
             textOutput("priceLegend"),
             dygraphOutput("priceGraph", height = "250px"),
             tags$hr(),
             tags$h1(),
             tags$h4("Order depth graph"),
             tags$h1(),
             dygraphOutput("orderDepthGraph", height = "250px"),
             tags$hr()
             ),
    tabPanel("About",
             tags$h1(),
             tags$h4("Shiny-GDAX"),
             tags$h1(),
             tags$div(
               "The Global Digital Asset Exchange (GDAX) is a regulated U.S. 
               based exchange service for institutions and professional traders 
               offering digital currencies like Bitcoin, Ethereum and Litecoin 
               for fiat currency."
             ),
             tags$h1(),
             tags$div(
               "Shiny-GDAX is an interactive market visualization tool for GDAX 
               built using the GDAX API and the Shiny R package. The tool 
               allows price and order book monitoring with more features coming 
               soon."
             ),
             tags$h1(),
             tags$div(
               "The currencies and their minimum sizes currently in GDAX are"
             ),
             dataTableOutput("currencyTable"),
             tags$h1(),
             tags$div(
               "The products in GDAX available for trading are"
             ),
             dataTableOutput("productTable"),
             tags$hr()
             )
    )
)

server <- function(input, output, session) {
  source("reactives.R", local = T)
}

shinyApp(ui = ui, server = server)
