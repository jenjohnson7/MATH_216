# 30 Oct

library(shiny)

ui <- fluidPage(
  sliderInput("number", "Select a sample size", 0, 50, 25),
  textInput("title", "Enter your desired title", "Default Title"),
  actionButton("button", "Go!"),
  plotOutput("histogram")
)

server <- function(input, output) {

  # when you press button, run code
  observeEvent(input$button, {
    output$histogram <- renderPlot(hist(rnorm(input$number), main = input$title))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)