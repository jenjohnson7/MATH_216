# 9 October

# probability distribution (continuous)
#   curve that describes the probability that a random variable falls in any interval of values.
#   distribution of ages at Midd is a bell curve
#   what is the probability that people are between 19 and 21? 
#   Pr(19<x<21)? Must be between 0 and 1.
#   Area under the curve by integrating where lower limit = 19 and upper limit = 21

# population distribution 
#   special type of probability distribution
#   applies only for selecting a subject or "thing" at random from the population

# parameters
#   numerical summaries of the population (mean, sd)

# sampling distribution
#   applies for a sample from a population

# statistics
#   numerical summaries of a sample

# Where does the normal distribution come from?

# Central Limit Theorem
# For a random sample of size n 
# from a population with mean mu and sd sigma,
# the mean of the random sample approaches mu as n approaches infinity.

# the sampling distribution of x bar sub n is Normal with mean mu and sd = sigma/sqrt(n)

sat <- rnorm(100000, 600, 100)

r = c()

for (i in c(1:100)) {

  current <- sample(sat, 30)
  r <- c(r, mean(current))
}

mean(r) # ~ 60
sd(r) # ~ 18

# also works if sat vector is NOT normal.

# 11 Oct
# breaking CLT?
# if BOTH small sample size and weird distribution

test.scores <- c(98, 95, 92, 88, 86, 84, 84, 20, 14)
hist(test.scores) # weird distribution

r = c()

for (i in c(1:100)) {
  
  current <- sample(test.scores, 2, replace =TRUE)
  r <- c(r, mean(current))
}

hist(r) # not normal

# as sample size increases (approaches infinity), it beomes normally distributed

# how do you know if n is "big enough" ?

# Question 5 from Test 1
library(nycflights13)
flights <- flights
library(ggplot2)
library(tidyverse)

ggplot(flights, aes(dep_delay)) + geom_histogram() # not normal

# Assumptions met: randomly collected data
#                  CLT holds
#                     the distribution is not normal ....
#                     ... but as long as the sample size is big enough the distribution will become normal

# Null Hyp: mean delay = 5 minutes
# Alt Hyp: mean delay != 5 minutes

flights %>%
  filter(carrier == "DL") %>%
  summarize(mean.delay = mean(dep_delay, na.rm = TRUE), n = n(), sd = sd(dep_delay, na.rm = TRUE))
  
# mean 9.26
# sample size 48110
# sd 39

# Is the difference b/w 9.26 and 5 significant?
# CLT says that the there is a subset of size 48110 with normal distribution 
#                                                   with mean 9.26 and 
#                                                       sd 39/sqrt(48110) -->0.18


pnorm(9.26, 5, 0.18, lower.tail = FALSE)
# calculate the probability under normal distribution
# how likely is 9.26 to fall under the normal distribution curve?
# super low p value. There is a tiny percent chance that this difference is due to chance.
# lower.tail = FALSE bc 9.26 > 5

# another approach: t.test
delta.delays <- flights %>%
  filter(carrier == "DL") %>%
  select(dep_delay)
delta.delays <- as.data.frame(delta.delays)
t.test(delta.delays, mu = 5)
# super low p value. There is a tiny percent chance that this difference is due to chance.

# Another Example
# Call center: how long do people wait on hold before hanging up?

# Assumption: random data and CLT holds

# Null Hyp: They will wait for a mean of 15 minutes
# Alt Hyp: They will not wait for 15 minutes. 

data <- c(17, 13, 5, 24, 16, 29, 35)

mean(data) # 19.85
sd(data) # 10.17

# is this difference due to chance or is it significant?

# Assuming the CLT holds for n = 7
# Normal distribution of mean = 15 and sd = 10.17/sqrt(7) = 3.84

pnorm(19.86, 15, 3.84, lower.tail = FALSE)
# 19 > 15 so lower.tail is FALSE
# then multiply by 2 to account for both sides

# t.test (alternative to normal distribution)
# t distribution (df = n-1)
# CLT is supposed to work, but I have a small sample size. 
# use t.test instead of pnorm bc sample size is never infinity
# as sample size increases to infinity, t test becomes normal distribution
# with smaller samples, there is wider variability bc outliers have more impact on the mean

# t stat = sample mean - expected mean / (sample sd / sqrt(n))
#       19.86-15/[10.17/sqrt(7)] -->1.26
# then multiply by 2 to account for both sides
pt(1.26, 6, lower.tail = FALSE) * 2
# 0.25

# same result as t.test(times, mu = 15)
t.test(data, mu = 15)

# 13 October

# Suppose the lifespan of lightbulbs follows a distribution with mean 5 yrs and sd 4 yrs. What is the probability that you buy 100 lightbulbs and their average lifespan exceeds 6 yrs?

# CLT states that a sample of 100 should have normal distribution with mean 5 and sd 4/sqrt(100)

pnorm(6, mean = 5, sd = 4/sqrt(100), lower.tail=FALSE)
# 6 is mean of the sample of size 100

# if sample size is TOO small, use t test instead of pnorm.
# if distribution is weird, use t test instead of pnorm.
