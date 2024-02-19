#load library
library(randomForest)
library(e1071)


#load dataset
df <- read.csv("heart.csv")

str(df)
summary(df)

#convert binary variable to an integer variable
df$exng <- as.factor(df$exng)
df$sex <- as.factor(df$sex)
str(df)

set.seed(12)

#split data train and test
index <- sample(1:nrow(df), nrow(df) * 0.8)

train <- df [index, ]
test <- df[-index, ]

#Fit  logistic regression to classify exgn based on other models
baseline_model <- glm(exng ~ . , data = train, family = "binomial")
summary(baseline_model)

# Make predictions with the baseline model
predictions <- predict(baseline_model, newdata = test, type = "response")

# Threshold predictions to obtain predicted classes
predicted_exng <- ifelse(predictions > 0.3, 1, 0)

# Generate confusion matrix for baseline model
confusion_matrix_bm  <- table(predicted_exng, test$exng)
confusion_matrix_bm

# Calculate accuracy of the baseline model
accuracy <- sum(diag(confusion_matrix_bm)) / sum(confusion_matrix_bm)
round((accuracy * 100), 2)


#fit random forest model for classification
randomforest_model <- randomForest(exng ~ . , data=train)
summary(randomforest_model)
randomforest_model

#make predictions
prediction <- predict(randomforest_model, newdata = test)

#confusion matrix
cm <- table (prediction, test$exng)
cm
#model evaluation accuracy
accuracy <- sum(diag(cm) / sum(cm))
round(accuracy * 100, 2)

#plot variable importance
importance(randomforest_model)
varImpPlot(
  randomforest_model,
  sort = T,
  main = "Variable Importance for apatient angica"
)





#titanic data

df1 <- read.csv("Titanic-Dataset.csv")
summary(df1)
str(df1)

#remove multiple columns
df1 <- df1[, -c(1,4,9,11)]

#remove NA values
df1 <- na.omit(df1)

#convert survived to integer variable
df1$Survived <- as.factor(df1$Survived)

#convert sex to binary variable
df1$Sex <- ifelse(df1$Sex =="female", 1, 0)

#convert Embarked to a binary variable
df1$Embarked <- ifelse(df1$Embarked == "S", 1, 0)
str(df1)

set.seed(11)

#split into train and test
index <- sample(1:nrow(df1), nrow(df1) * 0.7)
train <- df1 [index, ]
test <- df1 [-index, ]

#Fit random forest model classification
rf_forest <- randomForest(Survived ~ . , data = train)
rf_forest

#make predictions
prediction <- predict(rf_forest, newdata = test)

#confusion matrix
cm <- table (prediction, test$Survived)
cm

#model evaluation accuracy
accuracy <- sum(diag(cm)) / sum(cm)

round(accuracy * 100, 2)

#plot error trees
plot(rf_forest)


# Plot variable importance
importance(rf_forest)
varImpPlot(
  rf_forest,
  sort = T,
  main = "Variable Importance for Passenger Survival"
)

#Fit  logistic regression to classify Survival based on other models
baseline_model <- glm(Survived ~ . , data = train, family = "binomial")
summary(baseline_model)

# Make predictions with the baseline model
predictions <- predict(baseline_model, newdata = test, type = "response")

# Threshold predictions to obtain predicted classes
predicted_exng <- ifelse(predictions > 0.3, 1, 0)

# Generate confusion matrix for baseline model
confusion_matrix_bm  <- table(predicted_exng, test$Survived)
confusion_matrix_bm

# Calculate accuracy of the baseline model
accuracy <- sum(diag(confusion_matrix_bm)) / sum(confusion_matrix_bm)
round((accuracy * 100), 2)





