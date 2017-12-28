# 1 Dec

# k-nearest neighbors

library(okcupiddata)
data <- profiles

library(ggplot2)

# no relationship b/w age and height
ggplot(data, aes(height, age)) + geom_point() + geom_jitter()

# compare to k nearest neighbors and use majority label of k

# when using a categorical/binary variable, normalize the data so that the distance between the categorical values is not penalized too much

# ie height vs drinks_a_lot_boolean

# ie 0-1 scaling
# if the same category, distance in that axis == 0
# if different category, distance in that axis == 1

# add third dimension by adding labels

# take (weighted) average of neighbors to determine a value for the added person

library(VIM)

profiles.subset <- profiles[1:10, ]

kNN.profiles <- kNN(profiles.subset) # impute all missing values

View(kNN.profiles)
