# 27 Sept

# Declaring a function

favorite_number <- function(x){
  string<- paste(c("My favorite number is ", x, "."), collapse = "")
  return(string)
} 

# t test function

# use negative numbers to account for LEFT tailed test.
data <- -c(1, 2, 3, 4, 5)
# left tailed argument
t.test(data, alternative = "l")

t_test <- function(vector, null.hyp = 0) {
  numerator <- mean(vector, na.rm = TRUE) - null.hyp
  denominator <- sd(vector) / sqrt(length(vector))
  t.stat <- numerator / denominator
  print(t.stat) # good
  degrees.freedom <- length(vector)-1
  p.value <- pt(t.stat, degrees.freedom)
  return(p.value) # not good
}
  
t_test(data)
# use my test function

# 29 Sept

# if else
posOrNeg <- function(x){
  # return(abs(x)==x)
  if (x > 0){
    message <- paste(x, "is greater than 0")
  } else if (x <0) {
    message <- paste(x, "is less than 0")
  } else {
    message <- "It's 0!"
  }
  return(message)
}

posOrNeg(1)
posOrNeg(-1)
posOrNeg(0)

# median value

d1 <- c(1:4)
d2 <- c(1:5)

findMedian <- function(vector) {
  vector <- sort(vector)
  oddOrEven <- length(vector) %% 2 
  if (oddOrEven == 1){
    result <- vector[(length(vector)/2) +1]
  } else {
    result <- (vector[(length(vector)/2)] + vector[(length(vector)/2) +1]) / 2
  }
  return(result)
}

findMedian(d2)
findMedian(d1)

# 2 Oct types of t-tests

# components of all tests
# assumptions
#   sample size
#       n > 30 (if you know distribution is crazy) OR population distribution is unimodal
#       central limit theorem: if sample size is large enough, then t stat will be normal
#   data were collected using randomization
# hypotheses
#   H0 (h not, null hypothesis) mean happiness of men = 70
#   H1 or Ha != 70
# test statistic
#   this is where the 3 tests are different
#   t = [observed mean - hypothesis mean] / [stdev/sqrt(num samples)]
# p value
#   from t.test
#   or calculate from test stat and degrees of freedom pt(tstat, df)
#     multiple by 2 if two-sided t test
# conclusion in context
#   originally: is p value < alpha --> significant
#   now: alpha values are stupid! random threshold
#   pvalue 0.08 --> there is an 8% chance that the observed mean (or something more extreme/further from the null hypothesis) occured due to RANDOM chance
#   pvalue 0.01 --> there is a 1% chance that the observed mean occured due to RANDOM chance

# single mean/one sample t test
# mean happiness of men who do not smoke marijuana
library(tidyverse)
data <- read.csv("Taboo.csv")

# find mean, sd, and num to calculate it from formula
data %>%
   filter(Gender == "M", Marijuana ==0) %>%
   summarize(mean.happiness = mean(Happiness)) # 66

data %>%
   filter(Gender == "M", Marijuana ==0) %>%
   summarize(sd.happiness = sd(Happiness)) # 11

data %>%
   filter(Gender == "M", Marijuana ==0) %>%
   summarize(num.happiness = n()) # 37

# or make a vector with the data and plug it into the t.test function
t.test.data <- data %>%
  filter(Gender == "M", Marijuana ==0) %>%
  select(Happiness)

t.test(t.test.data, mu = 70)
# t.stat = -1.79
# p value 0.08
# There is a 8% chance that the observed results from t.test.data are different from the null hypothesis 70 occured by chance.
# 8% is quite low, so this suggests that there is evidence to reject the null hypothesis.

# two means/two sampled t test
# assumption: the two groups are independent. 
# mean number of sexual encounters per week between men and women
# null hypothesis: there is no difference

mens.data <- data %>%
  filter(Gender == "M") %>%
  select(Sex)

womens.data <- data %>%
  filter(Gender == "F") %>%
  select(Sex)

t.test(mens.data, womens.data)
# p value = 0.25
# there is a 25% chance that this difference is due to chance.
# since 25% is reasonable, there is no evidence to reject the null hypothesis.

# matched-pairs t test
# two variables are dependent on each other.

# reaction time between dominant and nondominant
dominant = 357
nondom = 345

dominant <- c(293, 278, 390, 287, 316, 318, 216, 380)
nondominant <- c(250, 292, 423, 294, 316, 318, 292, 440)

diff <- dominant - nondominant

# null hypothesis: there is no difference between dom and non dom, so diff should be 0
mean(diff) # negative. This is the observed result.
t.test(diff, mu = 0) 
#0.2 --> there is 20% chance that the observed result is due to chance
# --> there is a 80% chance that the observed result is due to chance
# --> one in 5 is not that rare. no evidence against null hypothesis

# if you do t.test(dominant, nondominant), the p value is larger.
# lose information. not as accurate.
# reduces sd of test statistic
