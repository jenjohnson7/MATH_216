# 3 Nov
# Web Scraping

# we want long format (general format)
# datasets come in wide format

library(tidyverse)
library(rvest)

stocks <- tibble(
  time = c(1:6),
  stockA = c(2:7),
  stockB  = c(4:9)
)

# method 1 tidyverse
# gather but ignore the time column
View(gather(stocks, key = "t", value = "value", -time))

# method 2 reshape 2 melt, merge, join

brady.url <- "https://en.wikipedia.org/wiki/Tom_Brady"

brady.table <- brady.url %>% 
  read_html() %>%
  html_nodes("table") %>%
  # inspect source to find the correct index
  .[[7]] %>%
  # single brackets --> list
  # double brackets --> as an object. Or get a number instead of an object. 
  html_table(fill = TRUE, header = TRUE) # deal with multiple header columns 

# Cleaning
colnames(brady.table) <- brady.table[2, ] # reset col names
brady.table <- brady.table[-(1:2), ] # delete first 2 rows
brady.table <- brady.table[ , -ncol(brady.table)] # delete last empty column

brady.table$Yds <- gsub(",", "", brady.table$Yds) # find and replace commas with empty string...
hist(as.numeric(brady.table$Yds)) # ... so you are able to convert to numeric and plot

# Volcano Map

volcano.url <- "https://www.ngdc.noaa.gov/nndc/struts/results?type_0=Like&query_0=&op_8=eq&v_8=&type_10=EXACT&query_10=None+Selected&le_2=&ge_3=&le_3=&ge_2=&op_5=eq&v_5=&op_6=eq&v_6=&op_7=eq&v_7=&t=102557&s=5&d=5"

volcano.table <- volcano.url %>%
  read_html() %>%
  html_nodes("table") %>%
  .[[3]] %>%
  html_table(fill = TRUE)

library(leaflet)

leaflet(volcano.table) %>%
  addTiles() %>%
  addMarkers()
