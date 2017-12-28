# 13 Oct
# Multiple Testing Correction

library(ggplot2)
library(tidyverse)

bball <- read.csv("basketball.csv")

# Avg PPG = 66

UGA <- bball %>% 
  group_by(Team) %>%
  filter(Team == "Georgia") %>%
  select(Team, PointsPerGame) 

t.test(UGA$PointsPerGame, mu = 66)

# Now for all teams

points.test <- bball %>%
  group_by(Team) %>%
  summarize(pval = t.test(PointsPerGame, mu = 66)$p.val) %>%
  arrange(pval)

# so many t tests and pvals! how to adjust them so we can interpret them correctly?

# 16 Oct
# In March Madness, 19% of D1 teams are represented.
# Use NCAA boolean to see which teams went to the NCAA a different percent than 19%
# sort by p values to get on the most significant ones

ncaa.test <- bball %>% 
  group_by(Team) %>%
  summarize(NCAA.prop = sum(NCAA), NCAA.prop.test = prop.test(NCAA.prop, 3, p = 0.196)$p.val) %>%
  # each team had 3 rows in this df
  filter(NCAA.prop.test<0.05)
  # threshold/alpha value of 0.05

# lots of t tests. how to adjust/correct?

# Bonferroni
# make threshold = alpha/number of hypothesis tests

threshold = 0.05/73
# games.accepted has 73 observations
# 0.00068

# conservative. even teams that made the NCAA all 3 years are not significant bc of the small sample size.

# Null Hypothesis: The average number of blocks for a team is 130.

blocks.test <- bball %>%
  group_by(Team) %>%
  summarize(blocks.t.test.pval = t.test(Blocks, mu = 130)$p.val, t.stat = t.test(Blocks, mu = 130)$statistic)
  
uncorrected_blocks <- blocks.test %>% filter(blocks.t.test.pval<0.05) %>% arrange(blocks.t.test.pval)

bonferroni.threshold <- 0.05/73

corrected_blocks <- blocks.test %>% filter(blocks.t.test.pval<bonferroni.threshold)

mean(UMich$Blocks)
# much different tahn 130

Kentucky <- bball %>% select(Team, Blocks) %>% filter(Team == "Kentucky")
# large variability in 3 data points --> large t.stat due to high sd

# Another correction of p value instead of alpha value.

p.adjust(c(0.05, 0.01, 0.00000001), method = "bonferroni")

p.adjust(c(0.05, 0.04, 0.03, 0.02, 0.01), method = "bonferroni")
# alpha = 0.05
# bonferroni alpha = 0.05/5 = 0.01
# new mapping where only the last p value SHOUD be significant
# 0.25 0.20 0.15 0.10 0.05

blocks.test <- bball %>%
  group_by(Team) %>%
  summarize(blocks.t.test.pval = t.test(Blocks, mu = 130)$p.val, t.stat = t.test(Blocks, mu = 130)$statistic)

p.adjust(blocks.test$blocks.t.test.pval, method = "bonferroni")

ggplot(bball, aes(FTM/FTA, PointsPerGame)) + geom_point()

# 18 Oct

# Benjamini-Hochberg (BH) Method
# Set False Discovery Rate (FDR) to alpha
# (I want the pr(FDR) to be low for any given discovery or set of n discoveries)
# "liberal" approach

# Order the n p-values from largest to smallest.
# Evaluate Pk <= k/n * alpha
# compare the nth p value to a changing threshold.

# 0.06, 0.01, 0.002

# 0.06 <= 1/3 * 0.05 NO # the first threshold == bonferronni threshold
# 0.01 <= 2/3 * 0.05 YES
# 0.002 <= 3.3 *0.05 YES

p.values <- c(0.06, 0.01, 0.002)
# do it manually
# or adjust p values and interpret normally
p.adjust(p.values, method = 'BH')
