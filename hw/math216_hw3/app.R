library(shiny)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(plyr)

# Got health and wealth data from:
# http://www.gapminder.org/data/

# for each dataset, melt into variables country, year, variable where all 3 of these variables are not NA

health.data <- read_csv("indicator life_expectancy_at_birth.csv")
colnames(health.data)[1] <- "country"
melted.health <- melt(health.data, id = c("country"))
complete.health <- melted.health[complete.cases(melted.health), ]
colnames(complete.health) <- c("country", "year", "expectancy")

wealth.data <- read_csv("indicator gdp_per_capita_ppp.csv")
colnames(wealth.data)[1] <- "country"
melted.wealth <- melt(wealth.data, id = c("country"))
complete.wealth <- melted.wealth[complete.cases(melted.wealth), ]
colnames(complete.wealth) <- c("country", "year", "gdp")

# add first round of pop.data for decades
# if year is not a decade, it will be NA
population.data <- read_csv("indicator gapminder population.csv")
colnames(population.data)[1] <- "country"
melted.pop <- melt(population.data, id = c("country"))
complete.pop <- melted.pop[complete.cases(melted.pop), ]
colnames(complete.pop) <- c("country", "year", "pop.size")

# Got region data from: 
# https://github.com/lukes/ISO-3166-Countries-with-Regional-Codes/tree/master/all

region.data <- read_csv("Country2.csv")
clean.region.data <- region.data %>% 
  select(name, region) 
colnames(clean.region.data)[1] <- "country"

health.wealth <- join(complete.wealth, complete.health)
health.wealth.pop <- join(health.wealth, complete.pop)
all.data <- join(health.wealth.pop, clean.region.data)

# convert to numeric to use mod
all.data$year <- as.numeric(as.character(all.data$year))

# make a new column for non-decade years
data.to.merge <- all.data %>%
  mutate(mod = year%%10) %>%
  mutate(adjusted.year = year - mod) %>%
  mutate(year.to.merge = ifelse(is.na(pop.size), adjusted.year, year)) %>%
  select(country, year, year.to.merge, gdp, expectancy, region)

colnames(complete.pop)[2] <- "year.to.merge" 

# merge
final.data <- merge(data.to.merge, complete.pop)

final.data$expectancy <- as.numeric(as.character(final.data$expectancy))
final.data$gdp <- as.numeric(as.character(final.data$gdp))
final.data$pop.size <- as.numeric(as.character(final.data$pop.size))

final.data <- final.data %>% filter(!is.na(region)) %>%
  filter(!is.na(expectancy)) %>%
  filter(!is.na(gdp))

ui <- fluidPage(
   titlePanel("Global Health and Wealth"),
   sliderInput("year", "Select a Year", 1810, 2009, 1045, animate = TRUE),
   plotOutput("plot")
)

# I chose to scale the life expectancy axis by 85 and the wealth axis by 50000 so that *most* points fit on the graph. I found these but looking at the one.year.data df and sorting by gdp. I decided that the points lost on the gdp axis for the more recent years were worth it so that the majority of points were visible.

# I did not have population data for some dates, but I still had the gdp and expectancy values. Therefore, I plotted without using the size. 

# Define server logic
server <- function(input, output) {
  
  output$plot <- renderPlot({
    one.year.data <- final.data %>% filter(year == input$year)
    
    ggplot() + geom_point(data = one.year.data, aes(x = gdp, y = expectancy, size = pop.size, color = region)) + theme(axis.text.x = element_text(angle = 45)) + scale_y_continuous(limits = c(0, 85)) + scale_x_continuous(limits = c(0, 50000)) + ggtitle("Life expectancy in years vs GDP per person in dollars")
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

