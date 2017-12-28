library(okcupiddata)
library(ggplot2)
library(tidyverse)

profiles <- profiles

# Examples

ggplot(profiles, aes(height)) + geom_histogram() + xlim(c(50, 80))

contains.white.boolean <- grepl("white", profiles$ethnicity)
# find people who used "white" somewhere in the ethnicity category

View(profiles %>% filter(contains.white.boolean))
# includes white, white/asian, white/mix, etc.

# combine into one line
profiles %>% filter(grepl("white", ethnicity))

# Graph 1. Diet by Freq.

diet.freq <- data.frame(table(profiles$diet))

colnames(diet.freq) <- c("diet", "freq")

ggplot(diet.freq) + 
  geom_point(aes(x = diet, y = freq)) + 
  theme(axis.text.x = element_text(angle=60, hjust=1)) + 
  ggtitle("Frequency of Diet Preferences") + 
  theme(plot.title = element_text(hjust = 0.5)) 

# Graph 2. Diet By Freq. with Sex

diet.by.sex <- data.frame(table(profiles$diet, profiles$sex))

colnames(diet.by.sex) <- c("diet", "sex", "freq")

ggplot(diet.by.sex) + 
  geom_point(aes(x = diet, y = freq, colour = sex)) + 
  theme(axis.text.x = element_text(angle=60, hjust=1)) + 
  ggtitle("Frequency of Diet Preferences by Sex") + 
  theme(plot.title = element_text(hjust = 0.5)) 

# Graph 3. Diet by Scaled Freq with Sex.

num.males <- count(profiles %>% filter(sex == "m"))
num.females <- count(profiles %>% filter(sex == "f"))

diet.by.sex <- diet.by.sex %>%
  mutate(scaled.freq = freq / (ifelse(sex == "m", num.males[[1]], num.females[[1]])))

ggplot(diet.by.sex) + 
  geom_point(aes(x = diet, y = scaled.freq, colour = sex)) + 
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  ggtitle("Scaled Frequency of Diet Preferences by Sex") + 
  theme(plot.title = element_text(hjust = 0.5)) 

# Get significance using a proportion test
diets <-levels(diet.by.sex$diet)
total.trials <- c(num.females[[1]], num.males[[1]])

proportion.test.col <- c()

for (i in 1:length(diets)){
  current.diet <- diets[i]
  current.boolean <- diet.by.sex %>% filter(diet == current.diet)
  result <- prop.test(current.boolean$scaled.freq, total.trials)
  proportion.test.col <- c(proportion.test.col, result$p.value)
}

# No results compared by gender are significant since all p values == 1

# Graph 4. Looking at strict vs not strict vs strictly anything.

strict.array <- profiles %>% filter(grepl("strictly", profiles$diet))

strictly.anything <- strict.array %>% 
  filter(diet == "strictly anything") %>%
  select(age, diet) %>%
  mutate(diet.cat = "strictly anything")
  
strict.array <- strict.array %>% 
  filter(diet != "strictly anything") %>%
  select(age, diet) %>%
  mutate(diet.cat = "strictly")

anything.array <- profiles %>% 
  filter(grepl("anything", profiles$diet)) %>%
  filter(diet != "strictly anything") %>%
  select(age, diet) %>%
  mutate(diet.cat = "anything")

full.data <- rbind(strictly.anything, strict.array, anything.array)

ggplot(full.data) + 
  geom_histogram(data = full.data, aes(x=age, fill = diet.cat), alpha = 0.5) + 
  theme(legend.title = element_blank()) + 
  ggtitle("Diet Preferences by Age") + 
  theme(plot.title = element_text(hjust = 0.5)) 

# Graph 5. Age, Sex, and Self-claimed body type.

confidence.data <- data.frame(table(profiles$body_type, profiles$sex))

colnames(confidence.data) <- c("desc", "sex", "freq")

ggplot(confidence.data) + 
  geom_point(aes(x = desc, y = freq, color = sex)) + 
  theme(axis.text.x = element_text(angle=60, hjust=1)) + 
  ggtitle("Frequency of Body Type Descriptors By Sex") + 
  theme(plot.title = element_text(hjust = 0.5)) 

confidence.data <- confidence.data %>%
  mutate(scaled.freq = freq / (ifelse(sex == "m", num.males[[1]], num.females[[1]])))

ggplot(confidence.data) + 
  geom_point(aes(x = desc, y = scaled.freq, colour = sex)) + 
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  ggtitle("Scaled Frequency of Body Type Descriptors by Sex") + 
  theme(plot.title = element_text(hjust = 0.5)) 

# Get significance using a proportion test
body.types <-levels(confidence.data$desc)
total.trials <- c(num.females[[1]], num.males[[1]])

proportion.test.col <- c()

for (i in 1:length(diets)){
  current.body.type <- body.types[i]
  current.boolean <- confidence.data %>% filter(desc == current.body.type)
  result <- prop.test(current.boolean$scaled.freq, total.trials)
  proportion.test.col <- c(proportion.test.col, result$p.value)
}

# No results compared by gender are significant since all p values == 1