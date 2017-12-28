# 13 Nov

# Correlations
  # r 
    # correlation coefficient
    # values range from -1:1
  #cor(x, y)
  #cor.test(x, y)

p.values <- c()

for (i in 1:100000){
  x = sample(1:100, 100)
  y = sample(1:100, 100)
  a <- cor.test(x, y)
  p.value <- a$p.value
  p.values <- c(p.values, p.value)
}

hist(p.values)
# if null hypothesis is true
# p values follow a normal distribution

num.sig <- p.values[p.values < 0.05]
prop.sig <- length(num.sig)/length(p.values)

print(prop.sig)
# for the null hypothesis
# 5 percent of the time, we have a significant p value

# Simple Linear Regression
  # use an x variable to predict a y variable
    # x can be called a predictor variable, independent variable, or covariate
    # y can be called a response variable, dependent variable
  # y = mx + b + Error (True Population Equation)
  # regression equation to estimate the true population equation
    # y = mx + b
  # aka Least Squares Regression Line
    # minimize distance b/w line and points
    # square each distance so that low/negative errors and high/positive errors do not cancel each other out
    
library(ggplot2)
ggplot(faithful, aes(waiting, eruptions)) + geom_point()

cor.test(faithful$waiting, faithful$eruptions)
# p value is tiny --> significant

# linear model(y, x)
model1 <- lm(faithful$eruptions ~ faithful$waiting)
#lm(eruptions ~ waiting, data = faithful)

summary(model1)

# y = b + mx
# y = -1.87 + 0.075 * waiting

# Pr(>|t|) column
# how different are these values from 0?
# small p value --> there is a lot of evidence that this regression is not 0

# (Multiple) R squared
# 0.811
# If you use this model, you will be ~81% better than if you use a random model
# for comparing models

# manual lines
ggplot(faithful, aes(waiting, eruptions)) + geom_point() + geom_abline(slope = 0.075628, intercept = -1.874016, color = "red")

# automated method with confidence bands
ggplot(faithful, aes(waiting, eruptions)) + geom_point() + geom_smooth(method = "lm")
  
# Example
bball <- (read.csv(file.choose()))
  
model2 <- lm(Sagarin.Rating~Blocks, data = bball)
summary(model2)

# predict a variable using the other variable in the model
predict.lm(model2, data.frame(Blocks = 100))

predict.lm(model2, data.frame(Blocks = 100:200))

predict.lm(model2, data.frame(Blocks = 100), interval = "confidence")
# Based on the model, the value will probably fall at "fit", but it could vary down to "lwr" and up to "upr"
# Where the line will be. 

# plot the confidence intervals (95%) for the entire range of x values
ggplot(bball, aes(Blocks, Sagarin.Rating)) + geom_point() + geom_smooth(method = "lm")

# If 100 Blocks, there is a 95% percent chance that Sagarin.Rating will be between "lwr" and "upr".
# For a single point.
predict.lm(model2, data.frame(Blocks = 100), interval = "prediction")

# tabular form
library(broom)
library(knitr)

kable(tidy(model2))

# Linear Regression Over Multiple Levels of a Variable

library(corrplot)
library(tidyverse)

bball2 <- bball %>%
  select(Points, Blocks, Steals, Sagarin.Rating, OREB, DREB)

# compare all variables to each other
corrplot(cor(bball2))

# do() is a complicated version of summary
# subset the dataset according to Year and perform multiple lm
# use the . to use the subsetted dataset
bball.model <- bball %>%
  group_by(Year) %>%
  do(model.output = lm(Sagarin.Rating ~ Blocks, data = .))

bball.model %>% summarize(r.sq.values = summary(model.output)$r.squared)
# look at all r squared values over the levels of the variable Year

# Making New Variables

# sometimes the variables are not good predictors, but a function of existing variables may be a good predictor

# free throws made / free throws attempted
bball %>%
  mutate(free.throw.percent = FTM/FTA)

# Use Multiple Variables in a Linear Regression

multiple.model <- lm(Sagarin.Rating~ Blocks + Steals + OREB, data = bball)
# find the variables that represent the variability of Sagarin Rating

# add all variables
giant.model <- lm(Sagarin.Rating ~ ., data = bball)

# add variables randomly
x <- rnorm(219)
y <- runif(219, 0, 5)
z <- 1:219

# attach so that they can be from different datasets
# careful with masking!
attach(bball)

# adding random variables will increase r squared so much that the model is no longer applicable to a larger dataset
model3 <- lm(Sagarin.Rating~x+y+z)

# Alternatives to limit the number of variables
# Adjusted R Squared. (does not penalalize enough for adding random variables)
# AIC
# BIC
# lower numbers are better
# for comparing 2 models that are predicting the same variable
# cannot be used for comparing models that are predicting different variables

AIC(model3)
AIC(model1)
BIC(model3)
BIC(model1)

# So how do you pick the variables to use?

# step function
# assess variables by p value and AIC
# remove the worst variable
# rerun/repeat until all variables are significant
# may get stuck in a local min

# starting point
model4 <- lm(Sagarin.Rating ~ Points + Steals + Blocks + OREB + DREB + FTA, data = bball)
summary(model4)

step(model4)

# copy this output fromt step()
step.model <- lm(formula = Sagarin.Rating ~ Points + DREB + FTA, data = bball)
summary(step.model)

AIC(step.model) # lower

# How to use categorical variables in a multiple regression

Taboo <- read.csv(file.choose())

ggplot(Taboo, aes(Sex, Happiness)) + geom_point()

happiness.model <- lm(Happiness~Sex, data = Taboo)
summary(happiness.model)

ggplot(Taboo, aes(Sex, Happiness)) + geom_point() + geom_smooth(method = "lm")
# Happiness = intercept + slope(sex)

ggplot(Taboo, aes(Sex, Happiness)) + geom_point(aes(color = Gender)) + geom_smooth(method = "lm")
# when you color by Gender, the line is underpredicting for M and overpredicting for F

# Add categorical variable Gender
happiness.gender.model <- lm(Happiness~Sex + Gender, data = Taboo)
summary(happiness.gender.model)
# converts to cat variable to 0 and 1
# Happiness.for.zero.cat = intercept + slope(sex) + slope2(0)
# Happiness.for.one.cat = intercept + slope(sex) + slope2(1)

ggplot(Taboo, aes(Sex, Happiness)) + 
  geom_point(aes(color = Gender)) + 
  geom_abline(slope = happiness.gender.model$coefficients[[2]], intercept = 53, color = "red") +
  geom_abline(slope = happiness.gender.model$coefficients[[2]], intercept = 58, color = "blue")

# make interaction "term" between Sex and Gender
happiness.model3 <- lm(Happiness~Sex + Gender + Sex:Gender, data = Taboo)
summary(happiness.model3)
# Happiness = intercept + slope(sex) + slope2(gender) + slope3(sex)(gender)

ggplot(Taboo, aes(Sex, Happiness)) + 
  geom_point(aes(color = Gender)) + 
  geom_abline(slope = 2.5, intercept = 56, color = "red") +
  geom_abline(slope = 4.4, intercept = 57, color = "blue")

# 20 November

# Logistic Regression: predict or model a binary response variable using some number of predictor variables.

# log e (p / 1-p) = equation for a line
# p = pr(email == spam)
# "log odds" aka logit function == left side of the equation
# ensure that p is between 0 and 1
# p = (e ^ linear model) / (1 + e ^ linear model)

library(openintro)
data(email)

# an email can be either spam or not (binary)
hist(email$spam)

# linear model + plot does not make any sense
model1 <- lm(spam ~ attach, data = email)
summary(model1)
ggplot(email, aes(attach, spam)) + geom_point() + geom_smooth(method = "lm")

# generalized linear model uses a special transformation function (logit function) of y (response variable)
# family = binomial since spam is binary
logistic.model1 <- glm(spam ~ attach, data = email, family = binomial)
summary(logistic.model1)

# intercept = -2.2
# attach slope = 0.11
# Every new attachment increases log odds by 0.11
# num.attach = 10 

# manual
exp(-2.2 + 0.11*10)/(1 + exp(-2.2 + 0.11*10))
# if 10 attachments, there is a 24% chance that it is spam

# using R
predict.glm(logistic.model1, data.frame(attach = 10), type = "response")

# finding the best variables

table(email$viagra) # use table to look for "perfect" variables that may overfit model

lm <- glm(spam ~ attach + winner, data = email, family = binomial)
summary(lm) # AIC: 2414

lm <- glm(spam ~ attach, data = email, family = binomial)

# how to plot
range(email$attach) # 0 21
xattach <- seq(0, 20, 2)
yweight <- predict(lm, list(attach = xattach), type = "response")

ggplot(email, aes(x = attach, y = spam)) + geom_point()
