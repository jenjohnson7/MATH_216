# 27 Nov

library(stringr)
library(tidyverse)

fruit <- c("apple", "banana", "pear", "orange", "pineapple")

str_detect(fruit, "a") # contains a
str_detect(fruit, "^a") # starts with a
str_detect(fruit, "a$") # ends with a
str_detect(fruit, "e|b") # e or b
str_detect(fruit, "ap.*") # loops for ap followed by anything

str_which(fruit, "e") # return indices
str_count(fruit, "e") # return occurences
str_locate(fruit, "e") # returns instances
str_locate(fruit, "ap") # returns first instances: beginning and end
str_locate_all(fruit, "ap") # returns all instances: beginning and end. nested list.

str_replace(fruit, "orange", "Orange")

raw.profiles <- read.csv("profiles.csv")

library(okcupiddata)
data(profiles)

profiles$essay0 <- raw.profiles$essay0

# which profiles have the word sex
x <- profiles %>%
  filter(str_detect(essay0, "sex"))

x2 <- profiles %>%
  filter(str_sub(essay0, "sex"))

# where the word sex starts and ends
indexes <- str_locate(x$essay0, "sex")

# getting some context 20 letters before and after
x3 <- str_sub(x$essay0, indexes[,1]-20, indexes[,2]+20)

# removing html tags
x4 <- str_replace(x3, "[^A-Za-z0-9]", "")
# TO DO

# 29 Nov Cleaning Data

Grades <- read.csv("VeryRealGrades.csv")
View(Grades)

split.by.space <- str_split_fixed(Grades$NameGrade, " ", 3)
colnames(split.by.space) <- c("first", "last", "grade")

for (i in 1:nrow(split.by.space)){
  split.by.space[i, 1] <- str_replace(split.by.space[i, 1], split.by.space[i, 2], "")
}
