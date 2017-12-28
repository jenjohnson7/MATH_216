# 29 Nov
# Machine Learning: using data to make predictions about new data

# Supervised vs Unsupervised
# supervised: given known classifications/response variable values, labelled training data
# upsupervised: determine underlying structure in data w/o classifications, unlabelled training data

# Classification/Regression/Decision Trees

library(okcupiddata)
library(tidyverse)
library(caret)
library(rattle)
# https://gist.github.com/zhiyzuo/a489ffdcc5da87f28f8589a55aa206dd
library(reshape2)

profiles <- profiles
profiles2 <- profiles %>% 
  select(height, age, sex, drinks) %>%
  na.omit() %>%
  mutate(height2 = ifelse(height>60, "Tall", "Short"))

model1 <- train(height2 ~ height + age + sex + drinks, profiles2, method = "rpart")

fancyRpartPlot(model1$finalModel)

profiles3 <- profiles %>% 
  select(height, age, drinks, sex, body_type) %>%
  na.omit() %>%
  mutate(height2 = ifelse(height>60, "Tall", "Short"))

model2 <- train(sex ~ age + height + drinks + body_type, profiles3, method = "rpart")

fancyRpartPlot(model2$finalModel)

titanic <- read.csv("Titanic.csv")

# wide to long method

titanic2 <- data.frame()

for (i in 1:nrow(titanic)){
  if(titanic$Freq[i] >0){
    for (j in i:titanic$Freq[i]){
      titanic2 <- rbind(titanic2, titanic[i, ])
    }
  }
}

model3 <- train(Survived ~ ., titanic2, method = "rpart")

fancyRpartPlot(model3$finalModel)