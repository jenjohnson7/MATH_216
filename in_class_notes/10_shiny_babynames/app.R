# 27 Oct

library(shiny)
library(babynames)
library(tidyverse)
library(ggplot2)

ui <- fluidPage(
  sliderInput("year", "Select a year", 1880, 2015, 1947.5),
  plotOutput("Names")
)

server <- function(input, output){
  
  output$Names <- renderPlot({
    sorted <- babynames %>% 
      filter(year == input$year) %>%
      arrange(desc(prop)) %>%
      select(sex, name, prop)
    
    top.10 <- data.frame(sorted[1:10, ])
    
    ggplot(top.10, aes(x = reorder(name, desc(prop)), y = prop)) + geom_bar(stat = "identity", aes(fill = sex)) + scale_fill_manual(values = c("red", "black")) + xlab("Name")
    
  })
}

shinyApp(ui = ui, server = server)