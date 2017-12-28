# 4 Oct
# Test Chp 1-5 
# t tests. "Make a formal comparison..." Can use t.test
# proportion tests

# Proportion Tests

# Single Proportion

# Null Hypothesis: The claimed proportion p is 75% / 0.75
# Hypothesis: The claimed proportion p is not 75%
# 10/14 people have read >=4 HP Books
p.hat <- 10/14

# Assumptions: 
# Random Sample
# n*p >=10 AND n*(1-p) >=10
# n = number of samples
# p = expected num successes
# 14 * 0.75 and 14 * 0.25
#               FALSE

# Distribution of p hat?
# if both statements above TRUE: Normal
# else: Binomial (14, 0.75)

# test stat for binomial

# what is a binomial distribution?
# centered histogram at 0.75
hist(rbinom(10000, 14, 0.75))
# close to normal, but not quite.

# divide to get proportions instead of numbers
hist(rbinom(10000, 14, 0.75)/14)

# p value
# What is the probability of getting 0.71 or more extreme?
p <- binom.test(10, 14, 0.75)
# succeses, trials, null hypothesis
# 76% chance that results (p.hat) happened by chance assuming null hypothesis is true.
# There is no evidence against the null hypothesis.

# shortcut/corrected method
p_real_binom <- pbinom(10, 14, 0.75) * 2
# 71% chance that results (0.71) happened by chance. 
# multiply by 2 to look at both tails/sides

# test stat for nomal dist

sd <- sqrt(0.75*0.25/14) # calculate sd = sqrt(p.observed * 1-p.observed)/num_trials
p_norm <- pnorm(0.71, mean = 0.75, sd) * 2
# observed result, null hypothesis, sd
# multiply by 2 to look at both tails/sides

# 2 Proportions

library(tidyverse)
data <- read.csv("Taboo.csv")

# NULL: pr(male marijuana) == pr(female marijuana)
# Hyp:                     !=

# Assumptions. Data collected randomly.
#               np >=10 AND n(1-p) >=10 for both groups.
#               NEW FOR 2PROP. TEST: The two groups are independent/unpaired.

# test stat and p value

# get vectors of 0s and 1s (failures and successes)
males <- data %>% filter(Gender == "M") %>% select(Marijuana)
females <- data %>% filter(Gender == "F") %>% select(Marijuana)

# need vectors of successes and total sample sizes
num_smoke <- c(sum(males), sum(females))
total <- c(nrow(males), nrow(females))

prop.test(num_smoke, total)
#p-value = 1
# there is a 100% chance that the results observed due to chance.
# there is no evidence of difference b/w men and women
