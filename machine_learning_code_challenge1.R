# set seed
set.seed(123)

#Load Library
library(e1071)
library(tidyverse)
getwd()

#Import datasets
df <- read.csv("NBA_Player_Stats.csv")
head(df)

# Data Processing
summary(df)
str(df)

#Split dataset into training and testing
index <- sample(1:nrow(df), nrow(df) * 0.7)

train <- df[index, ]
test <- df[-index, ]

#Gamescored Model
Gamescored_model <- lm(GS ~ Age + MP + FG, data=train)
summary(Gamescored_model)

Gamescoredmodel_r2 <- 0.6344


#set seed
set.seed(23)

#Import dataset
df1 <- read.csv("london_merged.csv")
head(df1)
df1 <- df1[, -1]

#Data processing
str(df1)
summary(df1)

#split data into training and testing
index <- sample(1:nrow(df1), nrow(df1) * 0.7)

train <- df1[index, ]
test <- df1[-index, ]

#Fit logistic regression model to predict wheather a bike was rented on a weekday or weekend
bike_model <- glm(is_weekend ~ ., data = train, family= "binomial")
summary(bike_model)


#Make prediction
prediction <- predict(bike_model, newdata = test, type = "response")
range(prediction)

#Threshhold predictions
predicted_bike <- ifelse(prediction >= 0.5, 1, 0)

#predicted_bike
table(df1$is_holiday,df1$is_weekend)
#Generate confusion matrix
confusion_matrix <- table(predicted_bike, test$is_weekend)
confusion_matrix

#calculate accuracy
accuracy <- sum(diag(confusion_matrix) / sum(confusion_matrix))
round((accuracy * 100), 2)

