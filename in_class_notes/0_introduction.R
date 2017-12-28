# Week 1 Introduction
# 13 Sept
# use periods as spaces

# 3 types of objects

# vector: series of numbers
single.num.vector <- 27
multiple.num.vector <- c(1:5)

# matrix: series of objects that are all the same type (num or char)

# 1 3
# 2 4
# default byrow. fill down first.
matrix <- matrix(1:4, nrow = 2, ncol = 2)
# label all arguments except data

# 1 2
# 3 4
# by column. fill across first
matrix2 <- matrix(1:4, nrow = 2, byrow = TRUE)
# only need to provide 1 dimension

# df

# paste function to concatenate string and changing variable
jen <- 11
paste( c("Jen's", "favorite number is", jen), collapse=" ")
# use a vector as the first argument to be able to contain str and int

# Read
data <- read.csv("read_csv_example.csv")
# data <- read.csv(file.choose())
#View(data)

# 15 Sept

# Write 
write.csv(mtcars, file = "mtcars.csv")

cars.data <- read.csv("mtcars.csv")
# cars_data now has column X for unlabelled car name column

cars.data[1, 2]
# rows, columns
# numbering from 1 instead of 0 

# columns are variables
# rows are individuals

# access variables by name
cars.data$mpg

# use head(data) to figure out first couple of rows
head(cars.data)

# declare variable
v <- NULL

# loop

for (i in 1:10){
  v[i]<-i
}

# make last 2 csv files
write.csv(Titanic, file = "titanic.csv")
write.csv(iris, file = "iris.csv")

# vector of filenames
file_names <- c("mtcars.csv", "titanic.csv", "iris.csv")

# vector of datasets
datasets <- list()

# for loop that reads all 3
for (i in 1:length(file_names)){
  v <- read.csv(file_names[i])
  datasets[i] <- v
}

print(datasets)

# assign function
assign("five", 5)

# alternative for a pattern in dataset names
for (i in 1:3){
  assign(paste(c("new_dataset", i), collapse = ""), read.csv(file_names[i]))
}

# alternative for a "super dataset"
# must have same number and column names
# super.dataset <- NULL
# for (i in 1:3){
#   current.dataset <- read.csv(file_names[i])
#   
#   super.dataset <- rbind(super.dataset, current.dataset)
# }
