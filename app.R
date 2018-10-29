library(shiny)
library(quantmod)


ui <- fluidPage(
  titlePanel("Stock Data Technical Indicators with Shiny"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("symb", "Enter Valid Stock Symbol:", value = "", width = NULL, placeholder = NULL),
      checkboxInput("VOL", "Show Volume", FALSE),
      checkboxInput("SMA50", "Show SMA(50) in Red", FALSE),
      checkboxInput("SMA200", "Show SMA(200) in Blue", FALSE),
      checkboxInput("BB", "Show Bollinger Bands", FALSE),
      checkboxInput("MACD", "Show Moving Average Convergence Divergence (MACD)", FALSE),
      sliderInput("integer", "Number of Months of Data:", min = 1, max = 60, value = 12),
      numericInput("obs", "Last Number of Days to Show:", 5)
      
    ),
    mainPanel(textOutput("text1"), textOutput("text2"), plotOutput("plot"), tableOutput("view")))
)

server <- function(input, output) {
  data <- ""
  output$plot <- renderPlot({
    output$text1 <- renderText({paste("Output: ", input$symb)})
    validate(
      need(input$symb != "", "Please enter a valid stock symbol")
    )
    
    tryCatch({
      data <- getSymbols(input$symb, src = "yahoo", to = Sys.Date(), auto.assign = FALSE)
    },
    error=function(e) {
      output$text1 <- renderText({paste(input$symb, " is not a valid symbol.")})
      return(NULL) 
    }
    )
    
    output$view <- renderTable({
      tail(data, n = input$obs)
    }, include.rownames = TRUE)
    
    # Quantmod chart
    m <- paste0("last ",input$integer, " months")
    output$text2 <- renderText({paste("Currenty Showing: " , m)})
    a <- ""
    if(input$VOL){a <- paste0(a, "addVo()")}
    if(input$SMA50){a <- paste0(a, if(a != "") {"; "}, "addSMA(n = 50, col = 'red')")} 
    if(input$SMA200){a <- paste0(a, if(a != "") {"; "}, "addSMA(n = 200, col = 'blue')")} 
    if(input$BB){a <- paste0(a, if(a != "") {"; "}, "addBBands()")} 
    if(input$MACD){a <- paste0(a, if(a != "") {"; "}, "addMACD()")} 
    if(a == ""){a <- list(NULL)}
    tryCatch({
      chartSeries(data, 
                  theme = chartTheme("white"), 
                  type = "line", 
                  subset = m,
                  TA = a)
      },
    error=function(e) {
      output$text2 <- renderText({paste("")})
      return(NULL) 
    }
     
    )
    
  }
  
  ) 
  
}
  
# Run the application 
shinyApp(ui = ui, server = server)

