####Prepare####
library(tidyverse)
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)

data <- read_csv('https://raw.githubusercontent.com/guozhaosengzs/LoL_strategy/master/high_diamond_ranked_10min.csv')

####Explore Raw Data####
data.raw <- data
print(colnames(data.raw))
print(head(data.raw))

ggplot(data = data.raw, aes(x = factor(blueWins), fill = factor(blueFirstBlood))) + 
    geom_bar(position = "fill") +
    ggtitle("First Blood's Effect on Winning")

ggplot(data = data.raw, aes(x = factor(blueWins), y = blueGoldDiff, fill = factor(blueWins))) + 
    geom_violin(trim = TRUE) + 
    geom_boxplot(width=0.3, fill="grey67") + 
    theme_classic() +
    ggtitle("Gold Difference at 10min vs Winning")

ggplot(data = data.raw, aes(x = factor(blueWins), y = blueWardsPlaced)) + 
    geom_jitter(height = 0, width = 0.2, size = 0.9) +
    ggtitle("Amount of Wards Place vs Winning")

####Let the machine go at it!####
data.raw$blueWins = as.factor(data.raw$blueWins)
set.seed(12345)
data.raw$spl <- sample.split(seq_len(nrow(data.raw)), SplitRatio = 0.75)
data.raw.train <- subset(data.raw, spl == TRUE)[,-41]
data.raw.test <- subset(data.raw, spl == FALSE)[,-41]


rforest = randomForest(data = data.raw.train, blueWins ~ ., ntree = 1000)
rforest$confusion
plot(rforest$votes[,2], col =  data.raw$blueWins, pch = 1)

rforest.test = predict(rforest , type='class' , newdata = data.raw.test)
table(actual = data.raw.test$blueWins , predicted = rforest.test)

importance(rforest)


####Data Cleaning#### 
data.cleaned <- data
data.cleaned$blueKillDiff <- data.cleaned$blueKills -  data.cleaned$redKills
data.cleaned$blueTowerDiff <- data.cleaned$blueTowersDestroyed -  data.cleaned$redTowersDestroyed
data.cleaned$blueMinionDiff <- data.cleaned$blueTotalMinionsKilled - data.cleaned$redTotalMinionsKilled
data.cleaned$blueJungleDiff <- data.cleaned$blueTotalJungleMinionsKilled - data.cleaned$redTotalJungleMinionsKilled

data.cleaned$blueWardsPlacedDiff <- data.cleaned$blueWardsPlaced - data.cleaned$redWardsPlaced
data.cleaned$blueWardsDestroyedDiff <- data.cleaned$blueWardsDestroyed - data.cleaned$redWardsDestroyed
data.cleaned$firstBlood <- data.cleaned$blueFirstBlood - data.cleaned$redFirstBlood
data.cleaned$blueEliteMonstersDiff <- data.cleaned$blueEliteMonsters - data.cleaned$redEliteMonsters
data.cleaned$blueDragonsDiff <- data.cleaned$blueDragons - data.cleaned$redDragons
data.cleaned$blueHeraldsDiff <- data.cleaned$blueHeralds - data.cleaned$redHeralds

vars <- colnames(data.cleaned)
selected_vars <- vars[c(2, 18:19, 41:50)]
data.cleaned <- data.cleaned[selected_vars]
data.cleaned$blueWins = as.factor(data.cleaned$blueWins)
data.cleaned$firstBlood = as.factor(data.cleaned$firstBlood)
data.cleaned$blueDragonsDiff = as.factor(data.cleaned$blueDragonsDiff)
data.cleaned$blueHeraldsDiff = as.factor(data.cleaned$blueHeraldsDiff)


# Train : Test = 0.75 : 0.25
set.seed(12345)
data.cleaned$spl <- sample.split(seq_len(nrow(data.cleaned)), SplitRatio = 0.75)

data.train <- subset(data.cleaned, spl == TRUE)[,-14]
data.test <- subset(data.cleaned, spl == FALSE)[,-14]


####PCA####
model.pca <- prcomp(data[,-c(1,2)])
summary(model.pca)
model.pca
PCscores <- predict(model.pca)
PCscores <- data.frame(PCscores)
PCscores$group <- data$blueWins
ggplot(data = PCscores) +
  geom_point(mapping = aes(x = PC1, 
                           y = PC2, 
                           col = group), alpha = 0.5, size = 0.65) +    theme_classic() 
####Classification
trained_tree <- rpart(blueWins ~ ., method = 'class', data = data.train, control=rpart.control(cp=0.00145))
summary(trained_tree)
rpart.plot(trained_tree, extra = 101)

trained_tree_train = predict(trained_tree, type ='class')
table(actual = data.train$blueWins, predicted = trained_tree_train)

trained_tree_test= predict(trained_tree, type='class', newdata = data.test)
table(actual = data.test$blueWins, predicted = trained_tree_test)


####Logistic regression model####
model1 <- glm(data = data.train, blueWins ~ blueGoldDiff + blueDragonsDiff + blueExperienceDiff + blueMinionDiff, family='binomial')
summary(model1)

log_test_pred = predict(model1 , type ='response', newdata = data.test)
log_test_pred_stats = ifelse(log_test_pred > 0.5, 1, 0)
table(actual = data.test$blueWins , predicted = log_test_pred_stats)


####Gold is so Important?####

model2 <- lm(data = data.train, blueGoldDiff ~ blueKillDiff + blueTowerDiff + blueMinionDiff + blueJungleDiff)

summary(model2)


