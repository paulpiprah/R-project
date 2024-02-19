#Load library
library(randomForest)
library(e1071)
install.packages("mice")
library(mice)
library(dplyr)

#load dataset
df<- read.csv("IBM.csv")
head(df)

#chech summary and structure of dataset
summary(df)
str(df)


#Data Preprocessing
#convert Attrition to binary variable
  df$Attrition <- ifelse (df$Attrition == "Yes", 1, 0)
  df$Attrition <- as.factor(df$Attrition)

set.seed(12)

#split data into training and testing
index <- sample(1:nrow(df), nrow(df) * 0.7)
train <- df[index, ]
test <- df[-index, ]

#Fit random forest classification
rf_model <- randomForest(Attrition ~ . , data=train)
rf_model

#Make predictions
pred <- predict(rf_model, newdata = test)

# confusion matrix
cm <- table(pred, test$Attrition)

# model evaluation- accuracy
acc <- sum(diag(cm))/ sum(cm)
round(acc * 100, 2)

# Plot model trees and error rate
plot(rf_model)

# Plot variable importance
importance(rf_model)
varImpPlot(
  rf_model,
  sort = T,
  main = "Variable Importance for predicting Employees that left or stayed"
)


#Fit  logistic regression to classify Attrition based on other models
baseline_model <- glm(Attrition ~ . , data = train, family = "binomial")
summary(baseline_model)

# Make predictions with the baseline model
predictions <- predict(baseline_model, newdata = test, type = "response")

# Threshold predictions to obtain predicted classes
predicted_Attrition <- ifelse(predictions > 0.3, 1, 0)

# Generate confusion matrix for baseline model
confusion_matrix_bm  <- table(predicted_Attrition, test$Attrition)
confusion_matrix_bm

# Calculate accuracy of the baseline model
accuracy <- sum(diag(confusion_matrix_bm)) / sum(confusion_matrix_bm)
round((accuracy * 100), 2)



#Fit Naive BAYES 
comparison_model <- naiveBayes(Attrition ~ ., data = train)

# Display summary of the naive Bayes model
summary(comparison_model)

# Make predictions with the naive Bayes model
predictions_nb <- predict(comparison_model, newdata = test)

# Generate confusion matrix for naive Bayes model
confusion_matrix_cm <- table(predictions_nb, test$Attrition)
confusion_matrix_cm

# Calculate accuracy of the naive Bayes model
accuracy <- sum(diag(confusion_matrix_cm)) / sum(confusion_matrix_cm)
round((accuracy * 100), 2)



#load new dataset
df1 <- read.csv("horse.csv")

#preprocess data
summary(df1)
str(df1)


#convert surgery into binary
df1$surgery <- ifelse(df1$surgery == "no",0,1)

#convert surgical_lesion to binary
df1$surgical_lesion <- ifelse(df1$surgical_lesion == "yes",1,0)

df1$

#covert into integer
#df1$lesion_1 <- as.factor(df1$lesion_1)
#df1$lesion_2 <- as.factor(df1$lesion_2)
#df1$lesion_3 <- as.factor(df1$lesion_3)

#replace NA in pulse with mean
mean_pulse = mean(df1$pulse, na.rm = T)
mean_pulse

df1$pulse = ifelse(is.na(df1$pulse), mean_pulse, df1$pulse)
summary(df1$pulse)

#replace NA in respiratory with mean
mean_respiratory = mean(df1$respiratory_rate, na.rm = T)
mean_respiratory

df1$respiratory_rate = ifelse(is.na(df1$respiratory_rate), mean_pulse, df1$respiratory_rate)
summary(df1$respiratory_rate)

#replace NA in rectal temp with mean
mean_rectal = mean(df1$rectal_temp, na.rm = T)
mean_rectal

df1$rectal_temp = ifelse(is.na(df1$rectal_temp), mean_rectal, df1$rectal_temp)
summary(df1$rectal_temp)

#replace NA in nasogastric with mean
mean_nasogastric = mean(df1$nasogastric_reflux_ph, na.rm = T)
mean_nasogastric

df1$nasogastric_reflux_ph = ifelse(is.na(df1$nasogastric_reflux_ph), mean_rectal, df1$nasogastric_reflux_ph)
summary(df1$nasogastric_reflux_ph)

df1<- df1[, -c(7,8,9,10,11,12,13,14,15,17,18,19,20,21,22)]


set.seed(11)

#split into testing and training
index <- sample(1:nrow(df1), nrow(df1) * 0.7)
train <- df1[index, ]
test <- df1[-index, ]

#Fit random forest classifcation model
rf_model <- randomForest(surgery ~ ., data = train)
rf_model

#Make predictions
pred <- predict(rf_model, newdata = test)

# confusion matrix
cm <- table(pred, test$surgery)
cm

# model evaluation- accuracy
acc <- sum(diag(cm))/ sum(cm)
round(acc * 100, 2)

# Plot model trees and error rate
plot(rf_model)

# Plot variable importance
importance(rf_model)
varImpPlot(
  rf_model,
  sort = T,
  main = "Variable Importance for predicting Employees that left or stayed"
)


#fit logistic model
logist_model <- glm(surgery ~ .,  data =train, family = "binomial")
logist_model

# Make predictions with the logistic model model
predictions <- predict(logist_model, newdata = test, type = "response")

# Threshold predictions to obtain predicted surgery
predicted_surgery <- ifelse(predictions > 0.3, 1, 0)

# Generate confusion matrix for logistic model
confusion_matrix_bm  <- table(predicted_surgery, test$surgery)
confusion_matrix_bm

# Calculate accuracy of the logistic model
accuracy <- sum(diag(confusion_matrix_bm)) / sum(confusion_matrix_bm)
round((accuracy * 100), 2)


#Fit Naive BAYES 
logist_model <- naiveBayes(surgery ~ ., data = train)

# Display summary of the naive Bayes model
summary(logist_model)

# Make predictions with the naive Bayes model
predictions_nb <- predict(logist_model, newdata = test)

# Generate confusion matrix for naive Bayes model
confusion_matrix_cm <- table(predictions_nb, test$surgery)
confusion_matrix_cm

# Calculate accuracy of the naive Bayes model
accuracy <- sum(diag(confusion_matrix_cm)) / sum(confusion_matrix_cm)
round((accuracy * 100), 2)






