# 30 Oct

library(shiny)

ui <- fluidPage(
  sliderInput("number", "Select a sample size", 0, 50, 25),
  textInput("title", "Enter your desired title", "Default Title"),
  actionButton("button", "Go!"),
  plotOutput("histogram")
)

server <- function(input, output) {

  # store code as a reactive function
  histogram <- eventReactive(input$button, {
                              hist(rnorm(input$number))
                            })

  # use () when calling histogram since it is a reactive function
  output$histogram <- renderPlot(histogram())
}

# Run the application 
shinyApp(ui = ui, server = server)