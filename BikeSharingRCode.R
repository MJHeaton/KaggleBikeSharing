##
## Code for Analyzing Bike Sharing Dataset
##

## Libraries I need
library(tidyverse)
library(DataExplorer)
library(caret)
library(vroom)
library(lubridate)

## Read in the data
system.time(bike.train <- read.csv("../bike-sharing-demand/train.csv"))
system.time(bike.train <- vroom("../bike-sharing-demand/train.csv"))
bike.test <- vroom("../bike-sharing-demand/test.csv")
bike <- bind_rows(train=bike.train, test=bike.test, .id="id")

## Drop casual and registered
bike <- bike %>% select(-casual, -registered)

## Feature Engineering
bike$month <- month(bike$datetime) %>% as.factor()
bike$season <- as.factor(bike$season)

## Exploratory Plots
ggplot(data=bike, aes(x=datetime, y=count, color=as.factor(month(datetime)))) +
  geom_point()
plot_missing(bike)
plot_correlation(bike, type="continuous",
                 cor_args=list(use='pairwise.complete.obs'))
ggplot(data=bike, aes(x=season, y=count)) + 
  geom_boxplot()

## Dummy variable encoding - one-hot encoding
dummyVars(count~season, data=bike, sep="_") %>% 
  predict(bike) %>% as.data.frame() %>%
  bind_cols(bike %>% select(-season), .)

## Target encoding
bike$season <- lm(count~season, data=bike) %>% 
  predict(., newdata=bike %>% select(-count))

## Fit some models
bike.model <- train(form=count~season+holiday+atemp+humidity,
                    data=bike %>% filter(id=='train'),
                    method="ranger",
                    tuneLength=5,
                    trControl=trainControl(
                      method="repeatedcv",
                      number=10,
                      repeats=2)
)
plot(bike.model)
preds <- predict(bike.model, newdata=bike %>% filter(id=="test"))
submission <- data.frame(datetime=bike %>% filter(id=="test") %>% pull(datetime),
                         count=preds)
write.csv(x=submission, file="./MyfirstSubmission.csv", row.names=FALSE)                   







