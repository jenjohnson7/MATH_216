# 18 Oct
# ANOVA: analysis of variance

# to compare the mean value of a quantitative variable across >=3 groups
  # 1 group: single mean t test. 2 groups: 2 mean t test

# call center data
# calls randomly selected
# subjects listened to an ad, muzak, or classical music

ad <- c(5, 1, 11, 2, 8)
muzak <- c(0, 1, 4, 6, 3)
classical <- c(13, 9, 8, 15, 7)

# Assumptions

# independent groups
# randomly collected data
# CLT holds for *each* group (if small sample size, not crazy distribution, sd are close)

# Null: mean(ad) == mean(muzak) == mean(classical)
# Alt: at least 2 means are significantly different

# test statistic F =  between-group variability / within-group variabilty 
    # if Null: F = 0 

# X = mean(all points from all groups)
# between= [n * [(mean(group1)-X)**2 + (mean(group2)-X)**2 + mean((group3)-X)**2] ] / num_groups-1
# multiply numerator by n to *amplify* the differences 
# within-group variability = mean( sd(group1)**2, sd(group2)**2, sd(group3)**2)

wait.times <- c(ad, muzak, classical)
music <- rep(c("Ad", "Muzak", "Classical"), each = 5) # make categories

music.data <- as.data.frame(cbind(wait.times, music)) # convert to df

music.data$wait.times <- as.numeric(wait.times) # convert str --> int

# anova model

# aov(quantitative~categorical)
model <- aov(wait.times~music, data = music.data)
summary(model)

# F value = 6.4
# Pr(>F)/p value = 0.0126 *
