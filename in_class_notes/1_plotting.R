# Week 2 Plotting
# 18 Sept, 20 Sept

# ggplot
library(ggplot2)

# scatterplot: 2 quantitative variables
# boxplot: 1 quantitative variable OR 1 quan (y), 1 categorical variable (x)
# histogram: 1 quan (x), freq (y)

# carat vs price scatter of diamonds data
# aesthetics = variables assigned to axes
# just the base
ggplot(diamonds, mapping = aes(x = carat, y = price))
# add the points
ggplot(diamonds, mapping = aes(x = carat, y = price)) + geom_point()
# alternative code, same plot
ggplot(diamonds) + geom_point(aes(x=carat, y = price))

# changing axes, scale, and tick marks
ggplot(diamonds, mapping = aes(x = carat, y = price)) + geom_point() +
scale_x_continuous(limits = c(0, 10), breaks = c(0, 2, 4, 6, 8, 10))

# color of all points
ggplot(diamonds, mapping = aes(x = carat, y = price)) + geom_point(color = "red")

# multiple variables
# color of points based on a variable color (3rd variable to the plot)
ggplot(diamonds, mapping = aes(x = carat, y = price)) + geom_point(aes(color = color))

# box plots

ggplot(diamonds, mapping = aes(x = color, y = price)) + geom_boxplot()
# why are the most expensive diamonds bad colors?
ggplot(diamonds, aes(carat, price)) + geom_point(aes(color=color, size = depth))

# use factor(0) to only do 1 quantitative variable (nothing on the x axis)
ggplot(diamonds, mapping = aes(x = factor(0), y = price)) + geom_boxplot() + ggtitle("Single Box Plot of Diamond Price") + xlab("Diamonds")

# Code to center the title
#theme_update(plot.title = element_text(hjust = 0.5))

# multiple layers on a scatter plot

# subset of diamonds using [rows, columns]
diamonds2 <- diamonds[c(1, 63, 295, 25836, 42592), ]

# use different datasets for different layers.
# must use "data =" argument when multiple datasets
ggplot() + geom_point(data = diamonds, aes(x=carat, y = price)) + geom_point(data = diamonds2, aes(x = carat, y = price), color = "red", size = 5)

# subset of diamonds depending on value in "color" column
diamonds3 <- subset(diamonds, color == "J")

# plot again
ggplot() + geom_point(data = diamonds, aes(x=carat, y = price)) + geom_point(data = diamonds3, aes(x = carat, y = price), color = "red", size = 1)

# multiple layers on a histogram

d.diamonds <- subset(diamonds, color == "D")
j.diamonds <- subset(diamonds, color == "J")

ggplot() + geom_histogram(data = d.diamonds, aes(price), fill = "red", alpha = 0.5) + geom_histogram(data = j.diamonds, aes(price), fill = "blue", alpha = 0.5)

# faceting. breaking layers into different subplots
# makes it easier to see?
# x and y axes are standardized for comparisons
# variables must be categorical.

# Single variable.
ggplot() + geom_point(data = diamonds, aes(x=carat, y = price)) + facet_wrap(~cut)

# 2+ variables. Every combination is seen in a graph. 
ggplot() + geom_point(data = diamonds, aes(x=carat, y = price)) + facet_grid(color~cut)
