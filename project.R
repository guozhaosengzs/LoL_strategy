library(tidyverse)
library(caTools)
library(funModeling)

data <- read_csv('https://raw.githubusercontent.com/guozhaosengzs/LoL_strategy/master/high_diamond_ranked_10min.csv')

set.seed(12345)
data$spl <- sample.split(seq_len(nrow(data)), SplitRatio = 0.75)

data.train <- subset(data, spl == TRUE)
data.test <- subset(data, spl == FALSE)

sum(data.train$blueWins) / nrow(data.train)





