library(tidyverse)
library(caTools)

data <- read_csv('https://raw.githubusercontent.com/guozhaosengzs/LoL_strategy/master/high_diamond_ranked_10min.csv')

# Train : Test = 0.75 : 0.25
set.seed(12345)
data$spl <- sample.split(seq_len(nrow(data)), SplitRatio = 0.75)

data.train <- subset(data, spl == TRUE)
data.test <- subset(data, spl == FALSE)

# First blood took place every game beofre 10min 
x <- data.train$blueFirstBlood - data.train$redFirstBlood
unique(x)


# Remake a less confusing data set
colnames(data.train)
data.train.lite <- data.train %>% 
  select(blueWins, blueFirstBlood, blueKills, blueDeaths, blueDragons, blueGoldDiff, blueExperienceDiff)

data.train.lite$killParticipationDiff <- data.train$blueAssists - data.train$redAssists
data.train.lite$visionScore <- 
  data.train$blueWardsPlaced + data.train$blueWardsDestroyed - data.train$redWardsPlaced - data.train$redWardsDestroyed

# First try at a logistic regression model

model1 <- glm(data = data.train.lite, blueWins ~., family=binomial())
summary(model1)


# Multinomial logistic regression?
