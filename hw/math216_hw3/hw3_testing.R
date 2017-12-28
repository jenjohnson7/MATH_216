library(tidyverse)
library(ggplot2)
library(reshape2)
library(plyr)

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

# make a new year.to.merge column for non-decade years
data.to.merge <- all.data %>%
  mutate(mod = year%%10) %>%
  mutate(adjusted.year = year - mod) %>%
  mutate(year.to.merge = ifelse(is.na(pop.size), adjusted.year, year)) %>%
  select(country, year, year.to.merge, gdp, expectancy, region)

colnames(complete.pop)[2] <- "year.to.merge" 
 
final.data <- merge(data.to.merge, complete.pop)

final.data$expectancy <- as.numeric(as.character(final.data$expectancy))
final.data$gdp <- as.numeric(as.character(final.data$gdp))
final.data$pop.size <- as.numeric(as.character(final.data$pop.size))

# PER SLIDER
# filter to only include 1 year

one.year.data <- final.data %>% filter(year == "2000")

ggplot() + geom_point(data = one.year.data, aes(x = gdp, y = expectancy, size = pop.size, color = region)) + theme(axis.text.x = element_text(angle = 45)) + scale_y_continuous(limits = c(0, 85)) + scale_x_continuous(limits = c(0, 50000)) + ggtitle("Life expectancy in years vs GDP per person in dollars")
