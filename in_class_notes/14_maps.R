# 1 Nov

library(leaflet)
library(tidyverse)

# Mark a single location
leaflet() %>%
  addTiles() %>%
  addMarkers(lng = - 73.1755, lat = 44.01005, popup = "Warner Hall")

data <- data.frame(lat=1:10, long=rnorm(10, sd = 10))

leaflet(data) %>%
  addTiles() %>%
  addCircles()

data2 <- data.frame(y=1:10, x=rnorm(10, sd = 10))

leaflet(data2) %>%
  addTiles() %>%
  addCircles(lng = ~x, lat = ~-y)

new.data <- data.frame(lat = 53, long = 21)

leaflet(new.data) %>%
  addTiles()%>%
  addCircleMarkers(color = "red")

# set of points
new.data.2 <- data.frame(y = c(53, 51, 54), x = (c(21, 24, 36)))

leaflet(new.data.2) %>%
  addTiles()%>%
  addCircleMarkers(lng = ~x, lat = ~y, color = "red") # assign lat and long

# Single Icon
ladybug <- makeIcon(iconUrl = "https://kidsgrowingstrong.org/wp-content/uploads/2016/03/ldebug-650.png", iconWidth = 70, iconHeight = 90)

new.data.3 <- data.frame(y = c(53, 51, 54), x = (c(21, 24, 36)), cat.variable = c("bug", "bug", "plant"))

leaflet(new.data.3) %>%
  addTiles()%>%
  addMarkers(lng = ~x, lat = ~y, icon = ~ladybug)

# Multiple Icons
flower <- makeIcon(iconUrl = "https://static.pexels.com/photos/36764/marguerite-daisy-beautiful-beauty.jpg", iconWidth = 70, iconHeight = 90)

icons <- iconList(
  bug = ladybug,
  plant = flower
)

leaflet(new.data.3) %>%
  addTiles()%>%
  addMarkers(lng = ~x, lat = ~y, icon = ~icons[cat.variable])

# Midd Map
# http://www.mapcoordinates.net/en

midd.data <- data.frame(y = c(44.0048685, 44.01341192, 44.0080743), x = c(-73.1769778, -73.18125131, -73.180963), location = c("Nelson", "Bihall", "Xenia"))

badminton <- makeIcon(iconUrl = "https://shopbadmintononline.com/images/badminton-shuttlecocks-badminton-shuttles-AYQD016-4_D.jpg", iconWidth = 50, iconHeight = 50)

beaker <- makeIcon(iconUrl = "https://media.istockphoto.com/photos/test-tubes-picture-id176862377", iconWidth = 50, iconHeight = 50)

pineapple <- makeIcon(iconUrl = "https://static1.squarespace.com/static/570ea986a3360c29db3c44ec/t/5727ee264d088eb3ad9276bd/1462235896339/?format=500w", iconWidth = 50, iconHeight = 50)

midd.icons <- iconList(
  Nelson = badminton,
  Bihall = beaker,
  Xenia = pineapple
)

leaflet(midd.data) %>%
  addTiles() %>%
  addMarkers(lng = ~x, lat = ~y, icon = ~midd.icons[location])

