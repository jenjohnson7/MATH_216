# 25 Oct

library(shiny)
library(ggplot2)

ui <- fluidPage(
  # order here does not matter. only for ui design
  
  sliderInput("mean", "Select a mean", -50, 50, 0),
  sliderInput("sd", "Select a standard deviation", 0, 10, 5),
  actionButton("new.data.button", "Update Data"),
  textInput("title", "Enter your desired title", "Default Title"),
  actionButton("new.title.button", "Update Title"),
  selectInput("color", "Select a color", c("black", "red", "green")),
  plotOutput("Histogram")
)

server <- function(input, output){
  
  random.variables <- reactive({
    input$new.data.button
    isolate(data.frame(numbers = rnorm(10000, input$mean, input$sd)))
  })
  # make reactive object random.variables so that you can access input$variables
  # random variables is now a function. use () when calling in ggplot
  
  output$Histogram <- renderPlot({
    input$new.title.button
    ggplot(random.variables(), aes(numbers)) + 
      geom_histogram(fill = input$color) + 
      ggtitle(isolate(input$title))
    
  })
}

shinyApp(ui = ui, server = server)