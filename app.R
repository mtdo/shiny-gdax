library(httr)
library(jsonlite)
library(shiny)
library(shinythemes)
library(dygraphs)
library(xts)

# Load Coinbase Pro public client API functions
source("coinbasepror/public_client.R")

# Use a proxy (NULL for no proxy)
proxy <- NULL

# Load Coinbase logo
logo <- img(src ="coinbase.svg",
            align = "top",
            width = "10%",
            height = "10%",
            style = "margin-left:0px;
                     margin-right:20px;
                     margin-bottom:20px")
            
# Application UI
ui <- fluidPage(
  
  # Define theme
  theme = shinytheme("flatly"),
  
  # Create title panel
  titlePanel(title = logo, windowTitle = "Shiny Coinbase Pro"),
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
             tags$h4("Shiny Coinbase Pro"),
             tags$h1(),
             tags$div(
               "Coinbase Pro is a regulated U.S. 
               based exchange service for institutions and professional traders 
               offering digital currencies like Bitcoin, Ethereum and Litecoin 
               for fiat currency."
             ),
             tags$h1(),
             tags$div(
               "Shiny Coinbase Pro is an interactive market visualization tool for Coinbase Pro 
               built using the Coinbase Pro API and the Shiny R package. The tool 
               allows price and order book monitoring with more features coming 
               soon."
             ),
             tags$h1(),
             tags$div(
               "The currencies and their minimum sizes currently in Coinbase Pro are"
             ),
             dataTableOutput("currencyTable"),
             tags$h1(),
             tags$div(
               "The products in Coinbase Pro available for trading are"
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
