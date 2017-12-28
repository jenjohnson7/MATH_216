# 25 Oct

library(shiny)
library(ggplot2)

ui <- fluidPage(
  sliderInput("Slider1", "Select a Value", 0, 10000, 5000),
  plotOutput("Histogram")
)

server <- function(input, output){
  output$Histogram <- renderPlot({
    random.variables <- data.frame(numbers = rnorm(input$Slider1))
    ggplot(random.variables, aes(numbers)) + geom_histogram()
  })
}

shinyApp(ui = ui, server = server)