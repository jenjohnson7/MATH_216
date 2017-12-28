# 30 Oct

library(shiny)

ui <- fluidPage(
  
  actionButton("button1", "Normal"),
  actionButton("button2", "Uniform"),
  plotOutput("Histogram")
)

server <- function(input, output){
  v <- reactiveValues(data = NULL) 
  
  observeEvent(input$button1, v$data <- rnorm(1000))
  
  observeEvent(input$button2, v$data <- runif(1000))
  
  output$Histogram <- renderPlot({
    if(is.null(v$data) == TRUE) {return()}
    hist(v$data)
  })
}

shinyApp(ui = ui, server = server)