library(shiny)
library(tidyverse)
library(reshape2)
library(plyr)
library(leaflet)
library(mapview)
library(ggplot2)
library(leaflet.minicharts)

# get all datasets into a single dataframe
disease.directory <- "contagious-diseases"
disease.files <- list.files(disease.directory)
all.diseases <- data.frame()

for (i in 1:length(disease.files)){
  print(disease.files[i])
  current.data <- read.csv(paste(disease.directory, disease.files[i], sep = "/"))
  all.diseases <- rbind(all.diseases, current.data)
}

# convert year-week format into year
all.diseases <- all.diseases %>%
  mutate(year = round(week/100)) %>%
  mutate(new.week = week%%year)

# group by week and merge, retaining all original columns
all.diseases$incidence_per_capita <- as.numeric(as.character(all.diseases$incidence_per_capita))
temp <- aggregate(incidence_per_capita ~ year + state + disease, all.diseases, sum)
all.diseases <- merge(all.diseases, temp, by = c("year", "state", "disease"))

all.diseases <- all.diseases %>% select(year, state, disease, state_name, incidence_per_capita.y)

# add lat and long labels for plotting
statelatlong <- read.csv("statelatlong.csv")
colnames(statelatlong)[1] <- "state"

all.diseases <- join(all.diseases, statelatlong, by = "state")

ui <- fluidPage(
  
  titlePanel("Location and Scaled Prevance of Diseases in the US over time"),
  
  tabPanel(
    "2 Columns",
    fluidRow(
      column(width = 6,
        sliderInput("year", "Year", 1966, 2002, 1970, animate = TRUE),
        checkboxGroupInput("diseases", "Diseases to show:", 
                           c("Hepatitis" = "HEPATITIS A", 
                             "Measles" = "MEASLES", 
                             "Mumps" = "MUMPS", 
                             "Whooping Cough" = "PERTUSSIS", 
                             "Polio" = "POLIO", 
                             "Rubella" = "RUBELLA"), 
                           selected = c("MEASLES"), inline = TRUE),
        leafletOutput("mymap")
      ),
      column(width = 6,
        sliderInput("year2", "Year", 1966, 2002, 1970),
        checkboxGroupInput("diseases2", "Diseases to show:", 
                           c("Hepatitis" = "HEPATITIS A", 
                             "Measles" = "MEASLES", 
                             "Mumps" = "MUMPS", 
                             "Whooping Cough" = "PERTUSSIS", 
                             "Polio" = "POLIO", 
                             "Rubella" = "RUBELLA"), 
                           inline = TRUE),
        leafletOutput("mymap2")
        
      )
    )
  )
)

server <- function(input, output) {
  
  output$mymap <- renderLeaflet({
    
    if (length(input$diseases) == 1){
      
      current <- all.diseases %>%
        filter(disease == input$diseases[1]) %>%
        filter(year == input$year)

      current <- current %>%
        mutate(radius = cut(as.integer(current$incidence_per_capita.y), 5, labels = FALSE))

      leaflet(current) %>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)) %>%
        addCircleMarkers(label = paste(current$incidence_per_capita.y),
                         radius = current$radius * 2,
                         opacity = 1) %>%
        fitBounds(lng1 = -125,
                  lat1 = 50,
                  lng2 = -70,
                  lat2 = 25) # prevent moving around as it plays
      
    } else if(length(input$diseases)==0){
      leaflet(all.diseases) %>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)) %>%
        fitBounds(lng1 = -125,
                  lat1 = 50,
                  lng2 = -70,
                  lat2 = 25)
    } else {
      
      current <- all.diseases %>%
        filter(disease %in% input$diseases) %>%
        filter(year == input$year)
      
      total.cases <- current %>% select(disease, incidence_per_capita.y, state)
      state.loc <- current %>% select(state, Longitude, Latitude)
      state.loc.clean <- unique(state.loc)
      
      # get col == disease
      temp <- dcast(total.cases, disease~state, value.var = "incidence_per_capita.y")
      d_names <- unique(temp$disease)
      temp2 <- t(temp)
      temp3 <- as.data.frame(temp2)
      temp4 <- tail(temp3, -1)
      
      # merge with state.loc.clean to get the right number of states
      temp4$state <- rownames(temp4)
      temp5 <- join(temp4, state.loc.clean, type = "right")
      
      temp6 <- temp5[, -1]
      temp7 <- temp6[, !(colnames(temp6) %in% c("Latitude", "Longitude"))]
      
      # convert to numeric + set colnames for legend
      temp8 <- lapply(temp7, function(x) as.numeric(as.character(x)))
      temp9 <- as.data.frame(temp8)
      
      colnames(temp9) <- d_names
      
      leaflet(current) %>%
        addProviderTiles(providers$Stamen.TonerLite, 
                         options = providerTileOptions(noWrap = TRUE)) %>%
        addMinicharts(state.loc.clean$Longitude, state.loc.clean$Latitude,
                      type = "bar",
                      chartdata = temp9,
                      showLabels = TRUE) %>%
        fitBounds(lng1 = -125,
                  lat1 = 50,
                  lng2 = -70,
                  lat2 = 25) # prevent moving around as it plays
    }
  })
  output$mymap2 <- renderLeaflet({
    
    if (length(input$diseases2) == 1){
      
      current <- all.diseases %>%
        filter(disease == input$diseases2[1]) %>%
        filter(year == input$year2)
      
      current <- current %>%
        mutate(radius = cut(as.integer(current$incidence_per_capita.y), 5, labels = FALSE))
      
      leaflet(current) %>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)) %>%
        addCircleMarkers(label = paste(current$incidence_per_capita.y),
                         radius = current$radius * 2,
                         opacity = 1) %>%
        fitBounds(lng1 = -125,
                  lat1 = 50,
                  lng2 = -70,
                  lat2 = 25) # prevent moving around as it plays
      
    } else if(length(input$diseases2)==0){
      leaflet(all.diseases) %>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)) %>%
        fitBounds(lng1 = -125,
                  lat1 = 50,
                  lng2 = -70,
                  lat2 = 25)
    } else {
      
      current <- all.diseases %>%
        filter(disease %in% input$diseases2) %>%
        filter(year == input$year2)
      
      total.cases <- current %>% select(disease, incidence_per_capita.y, state)
      state.loc <- current %>% select(state, Longitude, Latitude)
      state.loc.clean <- unique(state.loc)
      
      # get col == disease
      temp <- dcast(total.cases, disease~state, value.var = "incidence_per_capita.y")
      d_names <- unique(temp$disease)
      temp2 <- t(temp)
      temp3 <- as.data.frame(temp2)
      temp4 <- tail(temp3, -1)
      
      # merge with state.loc.clean to get the right number of states
      temp4$state <- rownames(temp4)
      temp5 <- join(temp4, state.loc.clean, type = "right")
      
      temp6 <- temp5[, -1]
      temp7 <- temp6[, !(colnames(temp6) %in% c("Latitude", "Longitude"))]
      
      # convert to numeric + set colnames for legend
      temp8 <- lapply(temp7, function(x) as.numeric(as.character(x)))
      temp9 <- as.data.frame(temp8)
      
      colnames(temp9) <- d_names
      
      leaflet(current) %>%
        addProviderTiles(providers$Stamen.TonerLite, 
                         options = providerTileOptions(noWrap = TRUE)) %>%
        addMinicharts(state.loc.clean$Longitude, state.loc.clean$Latitude,
                      type = "bar",
                      chartdata = temp9,
                      showLabels = TRUE) %>%
        fitBounds(lng1 = -125,
                  lat1 = 50,
                  lng2 = -70,
                  lat2 = 25) # prevent moving around as it plays
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)