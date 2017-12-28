library(tidyverse)
library(reshape2)
library(plyr)
library(leaflet)
library(mapview)
library(ggplot2)

# first graph
data <- read.csv("Death_Rates1900-2013.csv")

ggplot(data, aes(x = Year, y = Age.Adjusted.Death.Rate)) + geom_freqpoly(aes(color = Leading.Causes), stat = "identity")

# second dataset

disease.directory <- "contagious-diseases"
disease.files <- list.files(disease.directory)
all.diseases <- data.frame()

for (i in 1:length(disease.files)){
    print(disease.files[i])
    current.data <- read.csv(paste(disease.directory, disease.files[i], sep = "/"))
    all.diseases <- rbind(all.diseases, current.data)
}

# for both methods
all.diseases <- all.diseases %>%
  mutate(year = round(week/100)) %>%
  mutate(new.week = week%%year)

all.diseases$incidence_per_capita <- as.numeric(as.character(all.diseases$incidence_per_capita))

temp <- aggregate(incidence_per_capita ~ year + state + disease, all.diseases, sum)

all.diseases <- merge(all.diseases, temp, by = c("year", "state", "disease"))

all.diseases <- all.diseases %>% select(year, state, disease, state_name, incidence_per_capita.y)

# only for method 1
statelatlong <- read.csv("statelatlong.csv")
colnames(statelatlong)[1] <- "state"
multiple.diseases <- join(all.diseases, statelatlong, by = "state")

# intro graph

j <- aggregate(incidence_per_capita.y ~ year + disease, all.diseases, mean)

ggplot(j) + geom_line(aes(x = year, y = incidence_per_capita.y, color = disease))

ggplot(j) + 
  geom_line(aes(x = year, y = incidence_per_capita.y, color = disease)) +
  ylim(0, 150) +
  xlim(1966, 2002)

# only for method 2
library(ggthemes)
state_map <- map_data('state')

all.diseases$region <- tolower(all.diseases$state_name)

single.diseases <- all.diseases %>%
  group_by(region) %>%
  right_join(state_map, by = "region")

# for testing
d <- c("HEPATITIS A", "MEASLES","MUMPS", "WHOOPING COUGH")
d <- c("HEPATITIS A")
current.year = 1970

# for multiple
current <- multiple.diseases %>%
  filter(disease %in% d) %>%
  filter(year == current.year)

# scaling for radius of dots for single disease
current <- current %>%
  mutate(radius = cut(as.integer(current$incidence_per_capita.y), 5, labels = FALSE))
  
# for single disease
leaflet(current) %>%
  addTiles() %>%
  addMarkers(label = paste(current$incidence_per_capita.y))

# for single disease with different marker size
leaflet(current) %>%
  addTiles() %>%
  addCircleMarkers(label = paste(current$incidence_per_capita.y), radius = current$larger.scale * 2, opacity = 1) 

# for single, method 2 heatmap
current <- single.diseases %>%
  filter(disease %in% d) %>%
  filter(year == current.year)

ggplot(current, aes(x = long, y = lat, group = group, fill = incidence_per_capita.y)) + 
  geom_polygon() + 
  geom_path(color = "white") + 
  theme_map() + 
  scale_fill_gradientn(colors = c("white", "darkblue"))

# miniplot library

# https://cran.r-project.org/web/packages/leaflet.minicharts/vignettes/introduction.html

# https://francoisguillem.shinyapps.io/shiny-demo/

library(leaflet.minicharts)

total.cases <- current %>% select(disease, incidence_per_capita.y, state)
state.loc <- current %>% select(state, Longitude, Latitude)
state.loc.clean <- unique(state.loc)

# get col == disease
temp <- dcast(total.cases, disease~state, value.var = "cases.y")
temp2 <- t(temp)
temp3 <- as.data.frame(temp2)

d_names <- unique(temp$disease)
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
  addTiles()%>%
  addMinicharts(state.loc.clean$Longitude, state.loc.clean$Latitude,
                type = "bar",
                chartdata = temp9,
                showLabels = TRUE)
