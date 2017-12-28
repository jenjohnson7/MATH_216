# 22 Sept

library(tidyverse)

# subset(diamonds, color == "E" & carat >1)
filter(diamonds, color == "E", carat >1)

# between is <= and >=
filter(diamonds, between(price, 100, 150))

# selecting columns
select(diamonds, carat, price)

# carat, color, cut, clarity
select(diamonds, starts_with("c"))

# 2 step filtering
expensive_diamonds <- filter(diamonds, price > 10000)
select(expensive_diamonds, color, price)

# pipe function
x <- c(1, 2, 3, 4, 5)
x %>% mean()
mean(x)

filter(diamonds, price > 10000)
diamonds %>% filter(price >10000)

# 2 step pipe filtering
diamonds %>%
  filter(price > 10000) %>%
  select(color, price)
  
# sorting
arrange(diamonds, carat)
arrange(diamonds, desc(carat))

diamonds %>%
  filter(price > 10000) %>%
  select(color, price, carat, depth) %>%
  arrange(carat, depth)

# Practice
data<-read.table("house.txt", header = TRUE)

# dataset with beds, baths, price, and cheap houses
data %>%
  select(Beds, Baths, Price) %>%
  filter(Price < 100000)

# dataset with only columns starting with B and whose values for all cells >=2 and <=4

b_columns <- data %>%
  select(starts_with("B")) %>%
  filter(between(Beds, 2, 4), between(Baths, 2, 4))

# find most expensive house with either 2 or 3 beds.
# scatterplot all houses with 2 or 3 beds Prices vs taxes
# highight the most expensive house

data %>% 
  filter(between(Beds, 2, 3)) %>%
  arrange(Price)

# make tiny single row dataset
expensive.house <- filter(data, case == 6)

data %>% 
  filter(between(Beds, 2, 3)) %>%
  arrange(Price) %>%
  ggplot(aes(x = Taxes, y = Price)) + geom_point() + geom_point(data = expensive.house, color = "red")

# 25 Sept

# add columns to df
data %>%
  mutate(BedBathBeyond = Beds + Baths)

# make single column
data %>%
  transmute(BedBathBeyond = Beds + Baths)

# adds a summary column
diamonds %>%
  summarize(mean.price = mean(price))
#mean(diamonds$price)

# use group by and summarize together
diamonds %>%
  group_by(color) %>%
  summarize(mean.price = mean(price))

# n() is count/num rows
diamonds %>%
  group_by(color) %>%
  summarize(num.of.diamonds = n())

# vector of booleans/1s and 0s
diamonds$price > 10000
# mean of vector of boolenas/1s and 0s
# proportion of diamonds > 10000
mean(diamonds$price > 10000)
# proportion of expensive diamonds by color
diamonds %>%
  group_by(color) %>%
  summarize(proportion.expensive = mean(price >10000))

library(nycflights13)
flights <- flights

# mean arrival delay for flights into Burlington vs flights into Boston
flights %>%
  filter(dest == "BTV" | dest == "BOS") %>%
  group_by(dest) %>%
  summarize(mean.delay = mean(dep_delay, na.rm = TRUE))

# 27 Sept

# bar graph of Top 10 carriers with highest proportion of flights delayed more than 1 hr.
flights %>%
  group_by(carrier) %>%
  summarize(mean = mean(dep_delay > 60, na.rm = TRUE)) %>% # get proportion
  #arrange(desc(mean)) %>% #just for looking at. unneccessary
  mutate(rank.column.name = rank(-mean)) %>% # use negative to change the ranking
  filter(rank.column.name <=10) %>%
  ggplot(aes(x = reorder(carrier, desc(mean)), mean)) + geom_bar(stat = "identity") 
  # stat = "identity" means x --> y instead of x --> count
  # reorder variable. reorder carrier by desc(mean). 
