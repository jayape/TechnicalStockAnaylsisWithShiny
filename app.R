library(shiny)
library(quantmod)


ui <- fluidPage(
  titlePanel("Stock Data Analysis - Shiny App"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Please select a symbol."),
      textInput("symb", "Enter Valid Stock Symbol:", value = "AAPL", width = NULL, placeholder = NULL),
      checkboxInput("VOL", "Show Volume", FALSE),
      checkboxInput("SMA20", "Show SMA(20) in Red", TRUE),
      checkboxInput("SMA200", "Show SMA(200) in Blue", FALSE),
      checkboxInput("BB", "Show Bollinger Bands", FALSE),
      sliderInput("integer", "Last Months Data:", min=1, max=100, value=12),
      
      numericInput("obs", "Number of Data to Show:", 5)
      
    ),
    mainPanel(textOutput("text1"),textOutput("text2"), textOutput("text3"), plotOutput("plot"), tableOutput("view")))
)

server <- function(input, output) {
  output$plot <- renderPlot({
    output$text1 <- renderText({paste("Output: ", input$symb)})
    validate(
      need(input$symb != "", "Please enter Valid Stock Symbol")
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
    if(input$SMA20){a <- paste0(a, if(a != "") {"; "}, "addSMA(n = 20, col = 'red')")} 
    if(input$SMA200){a <- paste0(a, if(a != "") {"; "}, "addSMA(n = 200, col = 'blue')")} 
    if(input$BB){a <- paste0(a, if(a != "") {"; "}, "addBBands()")} 
    tryCatch({
      chartSeries(data, 
                  theme = chartTheme("white"), 
                  type = "line", 
                  subset = m,
                  TA = a)
      #if(input$VOL){addVo()}
      #if(input$SMA20){addSMA(n = 20, col = "red")} 
    
      #if(input$SMA200){addSMA(n = 200, col = "blue")}
      #if(input$BB){addBBands()}
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

